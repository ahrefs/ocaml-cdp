(* cdp-gen: generates OCaml types from Chrome DevTools Protocol JSON
   definitions. JSON codecs come from the melange-json ppx ([@@deriving json]
   plus wire attributes); equal/show/make come from ppx_deriving.

   Usage:
     gen.exe <browser_protocol.json> <js_protocol.json> <output_dir> <Domain1,Domain2,...>

   Output layout:
     base.ml            -- sealed modules for every primitive alias type
                           (FrameId, TimeSinceEpoch, ...). Depends on nothing,
                           so alias references can never create a cycle
                           between domain modules.
     <domain>_types.ml  -- alias re-exports + named types. Refs to primitive
                           aliases (own or other domain) point at Base.
                           Remaining cross-domain type refs are checked to
                           form a DAG; a real cycle fails generation.
     <domain>.ml        -- includes the types module; one submodule per
                           command and event (Navigate.params / .result /
                           .name). These reference only *_types modules and
                           Base, so they can never participate in a cycle. *)

module J = Yojson.Safe
module U = Yojson.Safe.Util

let spf = Printf.sprintf

(* protocol revision stamped into generated headers; read from the REVISION
   file living next to the protocol JSON (written by `cdp-gen fetch`) *)
let revision = ref "unknown"

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  String.trim s

let keywords =
  [
    "and";
    "as";
    "assert";
    "asr";
    "begin";
    "class";
    "constraint";
    "do";
    "done";
    "downto";
    "else";
    "end";
    "exception";
    "external";
    "false";
    "for";
    "fun";
    "function";
    "functor";
    "if";
    "in";
    "include";
    "inherit";
    "initializer";
    "land";
    "lazy";
    "let";
    "lor";
    "lsl";
    "lsr";
    "lxor";
    "match";
    "method";
    "mod";
    "module";
    "mutable";
    "new";
    "nonrec";
    "object";
    "of";
    "open";
    "or";
    "private";
    "rec";
    "sig";
    "struct";
    "then";
    "to";
    "true";
    "try";
    "type";
    "val";
    "virtual";
    "when";
    "while";
    "with";
  ]

let is_upper c = c >= 'A' && c <= 'Z'
let is_lower c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')

(* FrameId -> frame_id, DOMSnapshot -> dom_snapshot, targetInfo -> target_info *)
let camel_to_snake s =
  let b = Buffer.create (String.length s * 2) in
  String.iteri
    (fun i c ->
      if is_upper c then begin
        let prev_lower = i > 0 && is_lower s.[i - 1] in
        let next_lower = i < String.length s - 1 && is_lower s.[i + 1] in
        if i > 0 && (prev_lower || next_lower) then Buffer.add_char b '_';
        Buffer.add_char b (Char.lowercase_ascii c)
      end
      else Buffer.add_char b c)
    s;
  Buffer.contents b

let sanitize_lower s =
  let s = camel_to_snake s in
  if List.mem s keywords then s ^ "_" else s

let module_of_domain d = String.capitalize_ascii (sanitize_lower d)

(* generated files are cdp_-prefixed so the library can be (wrapped false);
   the cdp.ml index re-exposes them as Cdp.Network etc. *)
let file_of_domain d = "cdp_" ^ sanitize_lower d
let domain_module_of_domain d = String.capitalize_ascii (file_of_domain d)
let types_module_of_domain d = domain_module_of_domain d ^ "_types"
let submodule_of_name n = String.capitalize_ascii (sanitize_lower n)

(* enum value -> constructor: "optionally-blockable" -> Optionally_blockable,
   "text/css" -> Text_css, "-Infinity" -> Minus_Infinity, "0" -> V0 *)
let constructor_of_enum_value v =
  let v =
    match String.length v > 0 && v.[0] = '-' with
    | true -> "Minus_" ^ String.sub v 1 (String.length v - 1)
    | false -> v
  in
  let b = Buffer.create (String.length v) in
  String.iter
    (fun c ->
      if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_' then Buffer.add_char b c
      else Buffer.add_char b '_')
    v;
  let s = Buffer.contents b in
  let s = if String.length s > 0 && s.[0] >= '0' && s.[0] <= '9' then "V" ^ s else s in
  let s = String.capitalize_ascii s in
  match s with
  | "None" | "Some" | "Ok" | "Error" | "Other" -> s ^ "_"
  | s -> s

type domain = {
  name : string;
  types : J.t list;
  commands : J.t list;
  events : J.t list;
}

let jstr m j = U.member m j |> U.to_string
let jlist m j =
  match U.member m j with
  | `Null -> []
  | x -> U.to_list x
let jbool m j =
  match U.member m j with
  | `Bool b -> b
  | _not_a_bool -> false

let load_domains path =
  let json = J.from_file path in
  U.member "domains" json
  |> U.to_list
  |> List.map (fun d ->
    { name = jstr "domain" d; types = jlist "types" d; commands = jlist "commands" d; events = jlist "events" d })

(* deprecated / experimental -> item attributes appended to a declaration *)
let item_attrs j =
  (if jbool "deprecated" j then "\n[@@ocaml.deprecated \"deprecated in CDP\"]" else "")
  ^ if jbool "experimental" j then "\n[@@alert experimental \"experimental in CDP, may change with Chrome\"]" else ""

let parse_ref ~current r =
  match String.index_opt r '.' with
  | Some i -> String.sub r 0 i, String.sub r (i + 1) (String.length r - i - 1)
  | None -> current, r

(* every "$ref" string reachable inside a json fragment *)
let rec collect_refs (j : J.t) acc =
  match j with
  | `Assoc kvs ->
    List.fold_left
      (fun acc (k, v) ->
        match k, v with
        | "$ref", `String r -> r :: acc
        | _not_a_ref -> collect_refs v acc)
      acc kvs
  | `List l -> List.fold_left (fun acc v -> collect_refs v acc) acc l
  | _scalar -> acc

let is_primitive_alias t =
  let has m = U.member m t <> `Null in
  if has "enum" || has "properties" || has "items" then None
  else (
    match U.member "type" t with
    | `String (("string" | "integer" | "number" | "boolean") as p) -> Some p
    | _not_a_primitive -> None)

(* (domain, TypeId) -> primitive kind, for aliases like Network.RequestId *)
let build_alias_table domains =
  let tbl = Hashtbl.create 64 in
  List.iter
    (fun d ->
      List.iter
        (fun t ->
          match is_primitive_alias t with
          | Some p -> Hashtbl.replace tbl (d.name, jstr "id" t) p
          | None -> ())
        d.types)
    domains;
  tbl

let sealed_path ~dom ~id = spf "Cdp_base.%s.%s" (module_of_domain dom) (submodule_of_name id)

(* the OCaml type expression for a protocol type; codecs are derived *)
let rec map_type ~selected ~alias_tbl ~domain (p : J.t) =
  match U.member "$ref" p with
  | `String r ->
    let dom, id = parse_ref ~current:domain r in
    if not (List.mem dom selected) then
      failwith (spf "cdp-gen: ref %s from domain %s escapes the selected set" r domain);
    if Hashtbl.mem alias_tbl (dom, id) then
      (* sealed primitive alias: lives in Base (a leaf), never cycles *)
      sealed_path ~dom ~id ^ ".t"
    else (
      let n = sanitize_lower id in
      match dom = domain with
      | true -> n
      | false -> spf "%s.%s" (types_module_of_domain dom) n)
  | `Null ->
    (match U.member "type" p with
    | `String "string" -> "string"
    | `String "integer" -> "int"
    | `String "number" -> "float"
    | `String "boolean" -> "bool"
    | `String "binary" -> "string" (* base64 on the wire *)
    | `String ("any" | "object") -> "Cdp_json.t"
    | `String "array" -> spf "%s list" (map_type ~selected ~alias_tbl ~domain (U.member "items" p))
    | j -> failwith (spf "cdp-gen: unhandled type in %s: %s" domain (J.to_string j)))
  | j -> failwith (spf "cdp-gen: bad $ref: %s" (J.to_string j))

(* one type declaration body ("name = <shape> <per-decl attrs>"), chained
   later as "type a = .. and b = .." with one [@@deriving] for the group *)
type decl = {
  name : string;
  body : string;
}

let enum_decl ~tname ~attrs values =
  let ctors = List.map (fun v -> v, constructor_of_enum_value v) values in
  let names = List.map snd ctors in
  let dedup = List.sort_uniq compare names in
  (match List.length dedup = List.length names with
  | true -> ()
  | false -> failwith (spf "cdp-gen: constructor collision in enum %s: %s" tname (String.concat "," names)));
  {
    name = tname;
    body =
      spf "%s =\n%s\n  | Other of Cdp_json.unknown [@json.catch_all]\n[@@compact_variants]%s" tname
        (ctors |> List.map (fun (v, c) -> spf "  | %s [@json.name %S]" c v) |> String.concat "\n")
        attrs;
  }

(* a record; fields with an inline enum get that enum hoisted to a named
   type (name chosen by ~hoist_name). Returns hoisted decls ++ [record]. *)
let record_decl ~selected ~alias_tbl ~domain ~tname ~attrs ~hoist_name props =
  let hoisted = ref [] in
  let hoist raw_name enum_json =
    let hname = hoist_name raw_name in
    let values = List.map U.to_string (U.to_list enum_json) in
    hoisted := !hoisted @ [ enum_decl ~tname:hname ~attrs:"" values ];
    hname
  in
  let fields =
    List.map
      (fun p ->
        let orig = jstr "name" p in
        let fname = sanitize_lower orig in
        let optional = jbool "optional" p in
        let t =
          match U.member "enum" p with
          | `List _ as enum_json -> hoist orig enum_json
          | _no_inline_enum ->
          match U.member "type" p, U.member "items" p with
          | `String "array", (`Assoc _ as items) when U.member "enum" items <> `Null ->
            spf "%s list" (hoist orig (U.member "enum" items))
          | _not_an_enum_array -> map_type ~selected ~alias_tbl ~domain p
        in
        let attrs = spf " [@key %S]%s" orig (if optional then " [@option] [@json.drop_default]" else "") in
        spf "  %s : %s%s%s;" fname t (if optional then " option" else "") attrs)
      props
  in
  !hoisted
  @ [ { name = tname; body = spf "%s = {\n%s\n}\n[@@allow_extra_fields]%s" tname (String.concat "\n" fields) attrs } ]

let alias_decl ~tname ~attrs t = { name = tname; body = spf "%s = %s%s" tname t attrs }

(* a named type from the "types" section (primitive aliases excluded) *)
let named_type_decls ~selected ~alias_tbl ~domain t =
  let tname = sanitize_lower (jstr "id" t) in
  let attrs = item_attrs t in
  match U.member "enum" t, U.member "properties" t with
  | `List values, _ -> [ enum_decl ~tname ~attrs (List.map U.to_string values) ]
  | `Null, `List (_ :: _ as props) ->
    let parent = camel_to_snake (jstr "id" t) in
    record_decl ~selected ~alias_tbl ~domain ~tname ~attrs ~hoist_name:(fun f -> parent ^ "_" ^ camel_to_snake f) props
  | `Null, _ -> [ alias_decl ~tname ~attrs (map_type ~selected ~alias_tbl ~domain t) ]
  | _ -> failwith ("cdp-gen: unhandled named type shape: " ^ jstr "id" t)

(* "type a = .. and b = .." with ONE [@@deriving] for the whole group *)
let render_chain ~deriving decls =
  match decls with
  | [] -> ""
  | _ :: _ -> spf "type %s\n[@@deriving %s]\n" (String.concat "\n\nand " (List.map (fun d -> d.body) decls)) deriving

let check_no_dup ~what names =
  let sorted = List.sort compare names in
  let rec go = function
    | a :: b :: _ when a = b -> failwith (spf "cdp-gen: duplicate %s: %s" what a)
    | _ :: tl -> go tl
    | [] -> ()
  in
  go sorted

let sealed_module ~mname ~prim ~attrs =
  let ml_ty, conv, eq_mod, prim_fn, show_expr =
    match prim with
    | "string" -> "string", "string", "String", "string", "Printf.sprintf \"%S\" x"
    | "integer" -> "int", "int", "Int", "int", "string_of_int x"
    | "number" -> "float", "float", "Float", "float", "string_of_float x"
    | "boolean" -> "bool", "bool", "Bool", "bool", "string_of_bool x"
    | p -> failwith ("cdp-gen: unknown primitive: " ^ p)
  in
  spf
    "module %s : sig\n\
    \  type t\n\
    \  val of_%s : %s -> t\n\
    \  val to_%s : t -> %s\n\
    \  val equal : t -> t -> bool\n\
    \  val compare : t -> t -> int\n\
    \  val pp : Format.formatter -> t -> unit\n\
    \  val show : t -> string\n\
    \  val of_json : Melange_json.t -> t\n\
    \  val to_json : t -> Melange_json.t\n\
     end = struct\n\
    \  type t = %s\n\
    \  let of_%s x = x\n\
    \  let to_%s x = x\n\
    \  let equal = %s.equal\n\
    \  let compare = %s.compare\n\
    \  let show (x : t) = %s\n\
    \  let pp fmt x = Format.pp_print_string fmt (show x)\n\
    \  let of_json = Melange_json.Primitives.%s_of_json\n\
    \  let to_json = Melange_json.Primitives.%s_to_json\n\
     end%s\n"
    mname conv ml_ty conv ml_ty ml_ty conv conv eq_mod eq_mod show_expr prim_fn prim_fn attrs

(* primitive aliases of a domain, in protocol order *)
let domain_aliases ~alias_tbl (d : domain) = List.filter (fun t -> Hashtbl.mem alias_tbl (d.name, jstr "id" t)) d.types

let emit_base_file ~alias_tbl domains =
  let buf = Buffer.create 4096 in
  Buffer.add_string buf
    (spf
       "(* Generated by cdp-gen from Chrome DevTools Protocol %s. DO NOT EDIT. *)\n\
        (* Sealed modules for every primitive alias type. This file depends on\n\
       \   nothing, so alias references can never create a module cycle. *)\n\n"
       !revision);
  List.iter
    (fun d ->
      match domain_aliases ~alias_tbl d with
      | [] -> ()
      | aliases ->
        Buffer.add_string buf (spf "module %s = struct\n" (module_of_domain d.name));
        List.iter
          (fun t ->
            let prim = Hashtbl.find alias_tbl (d.name, jstr "id" t) in
            let mname = submodule_of_name (jstr "id" t) in
            Buffer.add_string buf (sealed_module ~mname ~prim ~attrs:(item_attrs t));
            Buffer.add_string buf "\n")
          aliases;
        Buffer.add_string buf "end\n\n")
    domains;
  Buffer.contents buf

let header () = spf "(* Generated by cdp-gen from Chrome DevTools Protocol %s. DO NOT EDIT. *)\n\n" !revision

let emit_types_file ~selected ~alias_tbl (d : domain) =
  let buf = Buffer.create 4096 in
  Buffer.add_string buf (header ());
  Buffer.add_string buf "open Melange_json.Primitives\n\n";
  (* re-export sealed aliases so users write Cdp.Network.Request_id.t *)
  List.iter
    (fun t ->
      let mname = submodule_of_name (jstr "id" t) in
      Buffer.add_string buf (spf "module %s = %s%s\n" mname (sealed_path ~dom:d.name ~id:(jstr "id" t)) (item_attrs t)))
    (domain_aliases ~alias_tbl d);
  Buffer.add_string buf "\n";
  let named = List.filter (fun t -> not (Hashtbl.mem alias_tbl (d.name, jstr "id" t))) d.types in
  let decls = List.concat_map (named_type_decls ~selected ~alias_tbl ~domain:d.name) named in
  check_no_dup ~what:(spf "type name in %s_types" (sanitize_lower d.name)) (List.map (fun d -> d.name) decls);
  Buffer.add_string buf (render_chain ~deriving:"json, show, eq" decls);
  Buffer.contents buf

(* one submodule per command / event *)
let emit_item_module ~selected ~alias_tbl ~domain ~mname ~wire_name ~attrs ~params ~returns =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf (spf "module %s = struct\n" mname);
  Buffer.add_string buf (spf "let name = %S\n\n" wire_name);
  let local_names = ref [] in
  let block ~tname ~make props =
    let decls =
      record_decl ~selected ~alias_tbl ~domain ~tname ~attrs:"" ~hoist_name:(fun f -> sanitize_lower f) props
    in
    local_names := !local_names @ List.map (fun d -> d.name) decls;
    (* hoisted enums first (own group: `make` cannot derive on variants),
       then the record with its own deriving list *)
    let hoisted, record =
      match List.rev decls with
      | record :: rev_hoisted -> List.rev rev_hoisted, record
      | [] -> assert false
    in
    Buffer.add_string buf (render_chain ~deriving:"json, show, eq" hoisted);
    Buffer.add_string buf "\n";
    Buffer.add_string buf
      (render_chain ~deriving:(if make then "json, show, eq, make" else "json, show, eq") [ record ]);
    Buffer.add_string buf "\n"
  in
  (match params with
  | [] -> ()
  | props -> block ~tname:"params" ~make:true props);
  (match returns with
  | `Event -> ()
  | `Returns [] ->
    (* zero-return command. CDP answers {} while derived unit codecs expect
       null, so the json decoder stays hand-written. *)
    Buffer.add_string buf
      "type result = unit [@@deriving show, eq]\n\nlet result_of_json (_ : Cdp_json.t) : result = ()\n\n"
  | `Returns props -> block ~tname:"result" ~make:false props);
  check_no_dup ~what:(spf "type name in %s.%s" domain mname) !local_names;
  Buffer.add_string buf (spf "end%s\n\n" attrs);
  Buffer.contents buf

let emit_domain_file ~selected ~alias_tbl (d : domain) =
  let buf = Buffer.create 4096 in
  Buffer.add_string buf (header ());
  Buffer.add_string buf (spf "include %s\n" (types_module_of_domain d.name));
  Buffer.add_string buf "open Melange_json.Primitives\n\n";
  let used = ref (List.map (fun t -> submodule_of_name (jstr "id" t)) (domain_aliases ~alias_tbl d)) in
  let claim ~fallback_suffix n =
    let n = if List.mem n !used then n ^ fallback_suffix else n in
    if List.mem n !used then failwith (spf "cdp-gen: module name collision in %s: %s" d.name n);
    used := n :: !used;
    n
  in
  List.iter
    (fun c ->
      let cname = jstr "name" c in
      let mname = claim ~fallback_suffix:"_command" (submodule_of_name cname) in
      Buffer.add_string buf
        (emit_item_module ~selected ~alias_tbl ~domain:d.name ~mname
           ~wire_name:(d.name ^ "." ^ cname)
           ~attrs:(item_attrs c) ~params:(jlist "parameters" c)
           ~returns:(`Returns (jlist "returns" c))))
    d.commands;
  List.iter
    (fun e ->
      let ename = jstr "name" e in
      let mname = claim ~fallback_suffix:"_event" (submodule_of_name ename) in
      Buffer.add_string buf
        (emit_item_module ~selected ~alias_tbl ~domain:d.name ~mname
           ~wire_name:(d.name ^ "." ^ ename)
           ~attrs:(item_attrs e) ~params:(jlist "parameters" e) ~returns:`Event))
    d.events;
  Buffer.contents buf

(* type-level cross-domain graph; edges to sealed aliases excluded because
   those references are routed through Base (a leaf). Must be a DAG. *)
let check_types_dag domains ~alias_tbl =
  let edges (d : domain) =
    List.concat_map (fun t -> collect_refs t []) d.types
    |> List.filter_map (fun r ->
      let dom, id = parse_ref ~current:d.name r in
      match dom = d.name with
      | true -> None
      | false -> if Hashtbl.mem alias_tbl (dom, id) then None (* routed through Base *) else Some dom)
    |> List.sort_uniq compare
  in
  let graph = List.map (fun (d : domain) -> d.name, edges d) domains in
  let visiting = Hashtbl.create 16
  and done_ = Hashtbl.create 16 in
  let rec visit path n =
    if Hashtbl.mem done_ n then ()
    else if Hashtbl.mem visiting n then
      failwith
        (spf
           "cdp-gen: type-level cycle between domains: %s. Sealed-alias routing through Base cannot break it; the \
            domains in the cycle must be merged into one compilation unit."
           (String.concat " -> " (List.rev (n :: path))))
    else begin
      Hashtbl.add visiting n ();
      List.iter (visit (n :: path)) (try List.assoc n graph with Not_found -> []);
      Hashtbl.remove visiting n;
      Hashtbl.add done_ n ()
    end
  in
  List.iter (fun (d : domain) -> visit [] d.name) domains

(* cdp.ml: the module users open. Cdp.Network -> Cdp_network etc. *)
let emit_index domains =
  let buf = Buffer.create 512 in
  Buffer.add_string buf (header ());
  Buffer.add_string buf "(* Index: users write Cdp.Network, Cdp.Page, ... *)\n\n";
  Buffer.add_string buf "module Json = Cdp_json\nmodule Base = Cdp_base\n";
  List.iter
    (fun (d : domain) ->
      Buffer.add_string buf
        (spf "module %s = %s\nmodule %s_types = %s\n" (module_of_domain d.name) (domain_module_of_domain d.name)
           (module_of_domain d.name) (types_module_of_domain d.name)))
    domains;
  Buffer.contents buf

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let generate ~browser ~js ~outdir ~selected =
  (revision :=
     let f = Filename.concat (Filename.dirname browser) "REVISION" in
     try read_file f with Sys_error _ -> "unknown");
  let all = load_domains browser @ load_domains js in
  let domains = List.filter (fun (d : domain) -> List.mem d.name selected) all in
  (match List.filter (fun s -> not (List.exists (fun (d : domain) -> d.name = s) all)) selected with
  | [] -> ()
  | missing -> failwith ("cdp-gen: unknown domains: " ^ String.concat "," missing));
  let alias_tbl = build_alias_table domains in
  check_types_dag domains ~alias_tbl;
  write_file (Filename.concat outdir "cdp_base.ml") (emit_base_file ~alias_tbl domains);
  Printf.printf "generated cdp_base.ml: %d sealed alias modules\n" (Hashtbl.length alias_tbl);
  List.iter
    (fun (d : domain) ->
      let base = Filename.concat outdir (file_of_domain d.name) in
      write_file (base ^ "_types.ml") (emit_types_file ~selected ~alias_tbl d);
      write_file (base ^ ".ml") (emit_domain_file ~selected ~alias_tbl d);
      Printf.printf "generated %s(_types).ml: %d types, %d commands, %d events\n" (file_of_domain d.name)
        (List.length d.types) (List.length d.commands) (List.length d.events))
    domains;
  write_file (Filename.concat outdir "cdp.ml") (emit_index domains);
  Printf.printf "generated cdp.ml index (%d domains, protocol %s)\n" (List.length domains) !revision

let run_cmd cmd =
  match Sys.command cmd with
  | 0 -> ()
  | n -> failwith (spf "cdp-gen: command failed with exit %d: %s" n cmd)

(* Google publishes every protocol snapshot to npm as devtools-protocol@0.0.<rev>,
   so an exact revision is one tarball away. Without a revision we resolve
   "latest" through the npm registry first, so REVISION is always exact. *)
let fetch ~outdir ~rev =
  let rev =
    match rev with
    | Some r -> r
    | None ->
      let tmp = Filename.temp_file "cdp_gen" ".json" in
      run_cmd (spf "curl -sfL https://registry.npmjs.org/devtools-protocol/latest -o %s" (Filename.quote tmp));
      let v = U.member "version" (J.from_file tmp) |> U.to_string in
      Sys.remove tmp;
      (match String.split_on_char '.' v with
      | [ "0"; "0"; n ] -> n
      | _unexpected_format -> failwith ("cdp-gen: unexpected npm version: " ^ v))
  in
  let url = spf "https://registry.npmjs.org/devtools-protocol/-/devtools-protocol-0.0.%s.tgz" rev in
  List.iter
    (fun f ->
      run_cmd
        (spf "curl -sfL %s | tar -xzO package/json/%s > %s" (Filename.quote url) f
           (Filename.quote (Filename.concat outdir f))))
    [ "browser_protocol.json"; "js_protocol.json" ];
  write_file (Filename.concat outdir "REVISION") ("r" ^ rev ^ "\n");
  Printf.printf "fetched protocol r%s into %s/\n" rev outdir

let usage =
  "usage:\n\
  \  cdp-gen generate <browser_protocol.json> <js_protocol.json> <outdir> <Domain1,Domain2,...>\n\
  \  cdp-gen fetch <outdir> [<revision>]\n\
   generate stamps headers from the REVISION file next to the protocol JSON."

let () =
  match Array.to_list Sys.argv with
  | _ :: "generate" :: [ browser; js; outdir; domains ] ->
    generate ~browser ~js ~outdir ~selected:(String.split_on_char ',' domains)
  | _ :: "fetch" :: [ outdir ] -> fetch ~outdir ~rev:None
  | _ :: "fetch" :: [ outdir; rev ] -> fetch ~outdir ~rev:(Some rev)
  | _invalid_arguments ->
    prerr_endline usage;
    exit 1
