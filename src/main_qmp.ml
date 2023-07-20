module M = MParser
open M

let for_list xs iter = List.iter iter xs
let for_seq xs iter = Seq.iter iter xs
let for_ht_keys xs iter = for_seq (Hashtbl.to_seq_keys xs) iter
let for_ht_kv xs iter = for_seq (Hashtbl.to_seq xs) iter
let for_opt xs iter = Option.iter iter xs
let for_arr xs iter = Array.iter iter xs
let id x = x

let list_any xs pred = List.fold_left (fun acc x -> pred x || acc) false xs

module Qmake = struct
  type qmake_condition =
    | Symbol of string
    | Or of (qmake_condition * qmake_condition)
    | And of (qmake_condition * qmake_condition)
    | Not of qmake_condition
  and qmake_op_kind = Assign | Append | Remove | AppendUnique | Replace
  and qmake_value_piece = Expand of string | Literal of string
  and qmake_value = qmake_value_piece list
  and qmake_directive =
    | InfixOp of (qmake_op_kind * string * string list)
    | Function of string * string
    | Block of qmake_directive list
    | Condition of (qmake_condition * qmake_directive option * (qmake_condition option * qmake_directive option) list)
  and qmake_document = qmake_directive list
end

module SrcList = struct
  type t = string list
  type r = string list ref
  let finalize xs = List.rev !xs
  let for_ xs iter = for_list xs iter
  let create () = ref []
  let clear rxs = rxs := []
  let append rxs x = rxs := x :: !rxs
end

let hashset_of_list xs = Hashtbl.of_seq (Seq.map (fun name -> (name, ())) (List.to_seq xs))

open Qmake

let (|>>) x y = M.(|>>) x y

let is_hspace c = c = ' ' || c = '\t'

let is_nl c = c = '\n' || c = '\r'

let is_identifier_char c =
  (c >= 'a' && c <= 'z') ||
  (c >= 'A' && c <= 'Z') ||
  (c >= '0' && c <= '9') ||
  (c = '_') ||
  (c = '.')

let is_include_char c = (c <> ')') && (c <> '#')

let skipescape_satisfy pred = skip_many (
  (char '\\' >> skip_any_char_or_nl) <|>
  (skip_satisfy pred))

let skip_raw_hspace = skipescape_satisfy is_hspace

let require_eol = (skip newline <|> eof) <??> "newline or end of input"

let skip_line_comment =
  skip_many_satisfy (fun c -> not (is_nl c)) <??> "line comment"

let skip_hspace =
  skip_raw_hspace >>
  ((attempt ((skip_char '#' <??> "line comment") >> return true)) <|> return false) >>= fun comment ->
  if comment then skip_line_comment else return ()

let skip_sep_by p sep = p >> skip_many (sep >> p)

let skip_hspace_or_nl = skip_sep_by skip_hspace newline

let ident = M.many1_satisfy is_identifier_char <??> "identifier"
let token p = skip_hspace >> p

let bare_escaped_chars pred : (string, 's) M.t = fun s ->
  let ret = Buffer.create 16 in
  let rec loop s =
    (match M.read_char s with
    | Some c when c = '\\' ->
      let s = M.advance_state s 1 in
      (match (M.read_char s, M.next_char s) with
      | (Some '\r', Some '\n') ->
        let s = M.advance_state_nl s 2 in
        loop s
      | (Some '\r', _) | (Some '\n', _) ->
        let s = M.advance_state_nl s 1 in
        loop s
      | (Some c, _) when pred c ->
        let s = M.advance_state s 1 in
        Buffer.add_char ret c;
        loop s
      | (Some _, _) -> s
      | (None, _) -> s)
    | Some c when pred c ->
      Buffer.add_char ret c;
      let s = M.advance_state s 1 in
      loop s
    | Some _ | None -> s
    )
  in
  let s = loop s in
  let str = Buffer.contents ret in
  if String.length str = 0
  then M.Empty_failed (M.unknown_error s)
  else M.Consumed_ok (str, s, M.No_error)

let barestring () =
  let count = ref 0 in
  let is_barestring_char c = match c with
  | '{' -> count := !count + 1; true
  | '}' when !count = 0 -> false
  | '}' -> count := !count - 1; true
  | c ->
    not (is_hspace c) &&
    not (is_nl c) &&
    true
    (*
    (c <> '"')
    *)
  in
  skip_hspace >>
  (bare_escaped_chars is_barestring_char <??> "bare string") >>= fun r ->
  skip_hspace >>
  return r

let qmake_func_prefix =
  token ident >>= fun name ->
  skip_char '(' >>
  return (`PfxFunction name)

let qmake_func_args name =
  (M.many_satisfy (fun c -> c <> ')') <??> "function arguments") >>= fun args ->
  skip_char ')' >>
  skip_hspace >>
  return (Function (name, args))

let qmake_infix_prefix =
  let kinds = [
    (skip_string "=" >> return Assign);
    (skip_string "+=" >> return Append);
    (skip_string "-=" >> return Remove);
    (skip_string "*=" >> return AppendUnique);
    (skip_string "~=" >> return Replace);
  ] in
  token ident >>= fun name ->
  skip_hspace >>
  choice kinds >>= fun kind ->
  return (`PfxInfix (kind, name))

let qmake_infix_directive kind name =
  (skip_hspace >> many (barestring ())) >>= fun strs ->
  skip_hspace >>
  return (InfixOp (kind, name, strs))

let qmake_block_prefix =
  skip_hspace >>
  skip_char '{' >>
  skip_hspace >>
  return `PfxBlock

let qmake_empty_line_or_comment = skip_hspace >> require_eol

let rec qmake_empty_lines_or_comments () =
  (eof >> return ()) <|> (
  ((attempt (qmake_empty_line_or_comment >> return true)) <|> return false) >>= fun ok ->
  if ok then qmake_empty_lines_or_comments () else return ())

let opt_to_list x = match x with
| Some x -> [x]
| None -> []

let qmake_condition =
  let parse_cond_identifier = (
    let is_cond_identifier c =
      is_identifier_char c || c = '(' || c = ')' || c = '"' ||
      c = ',' || c = '$' || c == '*' || c == '-' || c = ' '
    in
    many1_satisfy is_cond_identifier >>= fun id ->
    return (Symbol (String.trim id))) <??> "conditional identifier"
  in
  let parse_not = (
    skip_char '!' >>
    return (fun x -> Not x)) <??> "conditional not" in
  let parse_or = (
    skip_char '|' >>
    return (fun x y -> Or (x, y))
    ) <??> "conditional or" in
  let parse_and =
    let trailing_function = look_ahead (skip_char ':' >> qmake_func_prefix >> qmake_func_args "" >> require_eol >> return ()) in
    let trailing_infix = look_ahead (skip_char ':' >> qmake_infix_prefix >> return ()) in
    let trailing_block = look_ahead (skip_char ':' >> qmake_block_prefix >> return ()) in
    let trailing_empty = look_ahead (skip_char ':' >> skip_hspace >> require_eol >> return ()) in
    (
      (*
      *)
      return (Some 5) >>= fun trailing ->
      (option ((trailing_function <|> trailing_infix <|> trailing_block <|> trailing_empty <|> zero))) >>= fun trailing ->
      match trailing with
      | Some x -> zero
      | None -> skip_char ':' >> return (fun x y -> And (x, y))
    ) <??> "conditional and" in
  let q_not = M.Prefix parse_not in
  let q_or = M.Infix (parse_or, M.Assoc_left) in
  let q_and = M.Infix (parse_and, M.Assoc_left) in
  let precedence = [[q_not]; [q_or; q_and]; []] in
  M.expression precedence parse_cond_identifier <??> "condition" >>= fun cond ->
  return cond

let conditional_delimiter =
  look_ahead (any_of " :{") >>= fun block_ch ->
  if block_ch = '{' then return ()
  else
    skip_char ':' >>
    skip_hspace >>
    return ()

let qmake_conditional_prefix possibly_optional =
  skip_hspace >>
  possibly_optional qmake_condition >>= fun cond ->
  skip_hspace >>
  conditional_delimiter >>
  return (`PfxConditional cond)

let rec qmake_block () =
  many_until (qmake_directive ()) (skip_hspace >> skip_char '}') >>= fun directives ->
  return (Block directives)
and qmake_conditional cond =
  let parse_single = option (attempt (qmake_single_directive ())) in
  let else_conditional =
    skip_char ':' >>
    qmake_conditional_prefix (fun x -> x >>= fun y -> return (Some y))
  in
  let parse_else =
    skip_hspace >>
    skip_string "else" >>
    skip_hspace >>
    ((attempt else_conditional) <|> (conditional_delimiter >> return `NoCondition)) >>= fun cond ->
    let cond = match cond with
    | `NoCondition -> None
    | `PfxConditional cond -> cond
    in
    parse_single >>= fun elsebranch ->
    skip_hspace_or_nl >>
    return (cond, elsebranch)
  in
  skip_hspace >>
  parse_single >>= fun r ->
  skip_hspace_or_nl >>
  many parse_else >>= fun elsebranches ->
  return (Condition (cond, r, elsebranches))
and qmake_single_directive () : (qmake_directive, 's) M.t =
  (
    choice [
      (attempt qmake_block_prefix) <??> "block";
      (attempt (qmake_conditional_prefix id)) <??> "conditional";
      (attempt qmake_func_prefix) <??> "function application";
      (attempt qmake_infix_prefix) <??> "variable assignment/append/remove";
      fail "expected a block, function application, variable manipulation, or conditional";
    ]
  ) >>= fun pfx ->
  (match pfx with
  | `PfxFunction name -> qmake_func_args name
  | `PfxInfix (kind, name) -> qmake_infix_directive kind name
  | `PfxConditional cond -> qmake_conditional cond
  | `PfxBlock -> qmake_block ()
  ) >>= fun ret ->
  return ret
and qmake_directive () : (qmake_directive, 's) M.t =
  qmake_empty_lines_or_comments () >>
  qmake_single_directive () >>= fun ret ->
  skip_hspace >>
  (*
  ((look_ahead (skip_char '}')) <|> require_eol) >>
  *)
  qmake_empty_lines_or_comments () >>
  return ret

let qmake_doc : (qmake_document, 's) M.t =
  many_until (qmake_directive ()) eof

let print_indent indent =
  for i = 0 to indent - 1 do
    Printf.printf "  "
  done

let rec ppr_directive indent directive =
  let open Printf in
  print_indent indent;
  let rec ppr_cond cond = match cond with
  | Not c -> printf "(not "; ppr_cond c; printf ")"
  | Or (l, r) -> printf "(or "; ppr_cond l; printf " "; ppr_cond r; printf ")"
  | And (l, r) -> printf "(and "; ppr_cond l; printf " "; ppr_cond r; printf ")"
  | Symbol s -> printf "\"%s\"" s
  in
  match directive with
  | InfixOp (kind, name, strs) ->
    let k = match kind with
    | Assign -> "assign"
    | Append -> "append"
    | Remove -> "remove"
    | AppendUnique -> "append-unique"
    | Replace -> "replace"
    in
    printf "(%s %s " k name;
    for_list strs (fun str -> printf " \"%s\"" str);
    printf ")\n"
  | Block dirs ->
    printf "(\n";
    for_list dirs (fun dir -> ppr_directive (indent + 1) dir);
    print_indent indent;
    printf ")\n"
  | Condition (cond, thenbranch, elsebranches) ->
    let match_branch tag branch = match branch with
    | Some d ->
        printf "\n";
        print_indent (indent + 1);
        printf "(%s\n" tag;
        ppr_directive (indent + 2) d;
        print_indent (indent + 1);
        printf ")";
    | None -> ()
    in
    printf "(condition ";
    ppr_cond cond;
    match_branch "then" thenbranch;
    for_list elsebranches (fun (elsecond, elsebranch) ->
      printf "\n";
      print_indent (indent + 1);
      printf "(else ";
      for_opt elsecond ppr_cond;
      printf "\n";
      for_opt elsebranch (ppr_directive (indent + 2));
      print_indent (indent + 1);
      printf ")"
    );
    printf ")\n";
  | Function (name, args) ->
    printf "(function \"%s\" args \"%s\"" name args;
    printf ")\n"

type ('ok, 'err) result = ('ok, 'err) Result.t

type 'a parsed_qmake = {
  path : string;
  doc : 'a;
}

let parse path =
  try (
    let ic = open_in path in
    try
      let parse_result = M.parse_channel qmake_doc ic () in
      let ret = match parse_result with
      | Success e -> Ok { path = path; doc = e }
      | Failed (msg, e) -> Error (`ParseError (msg, e))
      in
      close_in ic;
      ret
    with e ->
      close_in_noerr ic;
      raise e
  ) with Sys_error reason ->
    Error (`SysError reason)

exception EarlyAbort

let pwd_regexp = Str.regexp "\\$\\$PWD\\b"

let parse_and_process_includes parsed path =
  let err = ref None in
  let rec work path stack =
    if not (Hashtbl.mem parsed path)
    then (
      let current_dir = Filename.dirname path in
      let result = parse path in
      match result with
      | Ok { doc = doc; } ->
        let next = ref [] in
        let rec process_directive directive = match directive with
        | Function ("include", path) ->
          let path = Str.global_replace pwd_regexp current_dir path in
          let path = if Filename.is_relative path
            then Filename.concat current_dir path
            else path
          in
          next := path :: !next;
          Function ("include", path)
        | Block directives ->
          Block (List.map process_directive directives)
        | any -> any
        in
        let processed = List.map process_directive doc in
        Hashtbl.replace parsed path processed;
        for_list !next (fun next_path ->
          work next_path (next_path :: stack)
        )
      | Error (`ParseError (msg, e)) ->
        err := Some (`ParseError (msg, e, stack));
        raise EarlyAbort
      | Error (`SysError reason) ->
        err := Some (`SysError (reason, stack));
        raise EarlyAbort
    )
    else ()
  in
  (try work path [path]
  with EarlyAbort -> ());
  match !err with
  | Some err -> Error err
  | None -> Ok (Hashtbl.find parsed path, path)

type 'a srcgroup = { sources : 'a; headers : 'a; resources : 'a; forms : 'a }

let create_srcgroup () = {
  sources = SrcList.create ();
  headers = SrcList.create ();
  resources = SrcList.create ();
  forms = SrcList.create ();
}
let finalize_srcgroup src = {
  sources = SrcList.finalize src.sources;
  headers = SrcList.finalize src.headers;
  resources = SrcList.finalize src.resources;
  forms = SrcList.finalize src.forms;
}

let platform_names = hashset_of_list [
  "win32"; "winrt"; "unix"; "mac"; "macx"; "darwin"; "osx"; "android"; "haiku";
  "linux"; "unix"; "nacl"; "integrity"; "qnx"; "vxworks";
]

let is_platform_condition cond =
  let ret = ref false in
  let rec work cond = match cond with
  | Symbol str ->
    if Hashtbl.mem platform_names str
    then (ret := true; raise EarlyAbort)
    else ()
  | Or (l, r) -> work l; work r
  | And (l, r) -> work l; work r
  | Not c -> work c
  in
  (try work cond
  with EarlyAbort -> ());
  !ret

type eval_environment = {
  definitions : (string, unit) Hashtbl.t;
  parent : eval_environment option;
}

let default_eval_environment = {
  definitions = hashset_of_list [
    "qtConfig(shared)";
    "qtConfig(c__11)";

    (* core *)
    "qtConfig(big_codecs)";
    "qtConfig(codecs)";
    "qtConfig(commandlineparser)";
    "qtConfig(datestring)";
    "qtConfig(datetimeparser)";
    "qtConfig(doubleconversion)";
    "qtConfig(filesystemwatcher)";
    "qtConfig(itemmodel)";
    "qtConfig(library)";
    "qtConfig(library)";
    "qtConfig(mimetype)";
    "qtConfig(process)";
    "qtConfig(processenvironment)";
    "qtConfig(proxymodel)";
    "qtConfig(regularexpression)";
    "qtConfig(settings)";
    "qtConfig(sortfilterproxymodel)";
    "qtConfig(std_atomic64)";
    "qtConfig(stringlistmodel)";
    "qtConfig(temporaryfile)";
    "qtConfig(textcodec)";
    "qtConfig(textdate)";
    "qtConfig(thread)";
    "qtConfig(timezone)";
    "qtConfig(translation)";
    "qtConfig(identityproxymodel)";
    "qtConfig(itemmodel)";
    "qtConfig(proxymodel)";
    "qtConfig(sortfilterproxymodel)";
    "qtConfig(stringlistmodel)";
    "qtConfig(signaling_nan)";
    "qtConfig(cursor)";
    "qtConfig(cborstreamreader)";
    "qtConfig(cborstreamwriter)";
    "qtConfig(binaryjson)";
    "qtConfig(filesystemiterator)";

    (* gui *)
    "qtConfig(draganddrop)";
    "qtConfig(png)";
    "qtConfig(harfbuzz)";
    "qtConfig(standarditemmodel)";

    (* widgets *)
    "qtConfig(abstractbutton)";
    "qtConfig(abstractslider)";
    "qtConfig(accessibility)";
    "qtConfig(buttongroup)";
    "qtConfig(checkbox)";
    "qtConfig(columnview)";
    "qtConfig(combobox)";
    "qtConfig(dialog)";
    "qtConfig(dialogbuttonbox)";
    "qtConfig(errormessage)";
    "qtConfig(filedialog)";
    "qtConfig(filesystemmodel)";
    "qtConfig(fontcombobox)";
    "qtConfig(fontdialog)";
    "qtConfig(formlayout)";
    "qtConfig(groupbox)";
    "qtConfig(itemviews)";
    "qtConfig(label)";
    "qtConfig(lineedit)";
    "qtConfig(listview)";
    "qtConfig(listwidget)";
    "qtConfig(mainwindow)";
    "qtConfig(menu)";
    "qtConfig(menubar)";
    "qtConfig(messagebox)";
    "qtConfig(progressbar)";
    "qtConfig(pushbutton)";
    "qtConfig(radiobutton)";
    "qtConfig(resizehandler)";
    "qtConfig(rubberband)";
    "qtConfig(scrollarea)";
    "qtConfig(scrollbar)";
    "qtConfig(sizegrip)";
    "qtConfig(slider)";
    "qtConfig(spinbox)";
    "qtConfig(splitter)";
    "qtConfig(stackedwidget)";
    "qtConfig(statusbar)";
    "qtConfig(style-fusion)";
    "qtConfig(tabbar)";
    "qtConfig(tableview)";
    "qtConfig(tablewidget)";
    "qtConfig(tabwidget)";
    "qtConfig(textedit)";
    "qtConfig(toolbar)";
    "qtConfig(toolbox)";
    "qtConfig(toolbutton)";
    "qtConfig(treeview)";
    "qtConfig(treewidget)";
    "qtConfig(whatsthis)";
    "qtConfig(widgettextcontrol)";
    "qtConfig(tooltip)";

    "qtConfig(sessionmanager)";
    "qtConfig(clipboard)";
    "qtConfig(systemtrayicon)";
    "qtConfig(tabletevent)";
    "qtConfig(opengl)";
    "qtConfig(imageformat_png)";

    (* network *)
    "qtConfig(http)";
    (*
    "qtConfig(ssl)";
    *)
    "qtConfig(networkinterface)"; (* Needed by qnetworkproxy_win.cpp *)
  ];
  parent = None;
}

let rec is_defined env sym =
  if Hashtbl.mem env.definitions sym then true else
  match env.parent with
  | Some x -> is_defined x sym
  | None -> false

let evaluate_condition env cond =
  let rec work cond = match cond with
  | Symbol str -> is_defined env str
  | Or (l, r) -> work l || work r
  | And (l, r) -> work l && work r
  | Not c -> not (work c)
  in
  work cond

let platform_implications = Hashtbl.of_seq (List.to_seq [
  ("linux", ["unix"]);
  ("freebsd", ["unix"]);
  ("openbsd", ["unix"]);
  ("netbsd", ["unix"]);
  ("osx", ["unix"; "mac"; "macx"; "darwin"]);
])
let platforms = [""; "win32"; "linux"; "unix"; "osx"]
let add_platform_definitions dst platform =
  Hashtbl.replace dst platform ();
  (match Hashtbl.find_opt platform_implications platform with
  | Some xs -> for_list xs (fun x -> Hashtbl.replace dst x ())
  | None -> ())
let generate_platform_definitions platform =
  let ret = Hashtbl.create 4 in
  add_platform_definitions ret platform;
  ret

exception ParseFailure

let parse_or_exn parsed path =
  let printf = Printf.printf in
  let result = parse_and_process_includes parsed path in
  let print_include_history stack =
    printf "include history:\n";
    for_list stack (fun path ->
      printf "    %s\n" path
    )
  in
  let (doc, docpath) = match result with
  | Ok doc -> doc
  | Error err ->
    (match err with
    | `SysError (reason, stack) ->
      print_include_history stack;
      printf "system error: %s\n" reason
    | `ParseError (msg, e, stack) ->
      print_include_history stack;
      (match e with
      | Parse_error ((_idx, line, col), msgs) ->
        printf "parse error(s) at line %d column %d:\n" line col;
        for_list msgs (fun msg -> match msg with
          | Unexpected_error msg -> printf "  error: unexpected: %s\n" msg
          | Expected_error msg -> printf "  error: expected: %s\n" msg
          | Message_error msg -> printf "  error: message: %s\n" msg
          | Compound_error (msg, err) -> printf "  compound error: %s\n" msg
          | Backtrack_error err -> printf "  backtrack error\n"
          | Unknown_error -> printf "  unknown error\n"
        )
      | No_error -> printf "??? parse error: no error\n"));
    raise ParseFailure
  in
  (doc, docpath)

let accumulate_sources_and_headers parsed root_dir doc docpath =
  let accum_values mapval kind target values = match kind with
  | Assign ->
    SrcList.clear target;
    for_list values (fun value -> SrcList.append target (mapval value))
  | Append ->
    for_list values (fun value -> SrcList.append target (mapval value))
  | AppendUnique -> ()
  | Replace -> ()
  | Remove -> ()
  in
  let document_dir = Filename.dirname docpath in
  let process_with_env env is_platform_specific =
    let srcs = create_srcgroup () in
    let rec process_file in_platform_context directives current_path =
      let current_dir = Filename.dirname current_path in
      let rec process_directives in_platform_context directives =
        let expand_pwd str =
          let str = Str.global_replace pwd_regexp current_dir str in
          let str = if Filename.is_relative str then Filename.concat document_dir str else str in
          let str = FilePath.reduce ~no_symlink:true str in
          let str = FilePath.make_relative root_dir str in
          str
        in
        let accum = match (is_platform_specific, in_platform_context) with
        | (true, true) -> accum_values expand_pwd
        | (false, false) -> accum_values expand_pwd
        | (_, _) -> fun _ _ _ -> ()
        in
          for_list directives (fun directive -> match directive with
          | InfixOp (kind, "SOURCES", values) -> accum kind srcs.sources values
          | InfixOp (kind, "HEADERS", values) -> accum kind srcs.headers values
          | InfixOp (kind, "RESOURCES", values) -> accum kind srcs.resources values
          | InfixOp (kind, "FORMS", values) -> accum kind srcs.forms values
          | Function ("include", path) -> (
            let path = Str.global_replace pwd_regexp current_dir path in
            let path = if Filename.is_relative path
              then Filename.concat current_dir path
              else path
            in
            let (nested_doc, nested_doc_path) = parse_or_exn parsed path in
            process_file in_platform_context nested_doc nested_doc_path
            )
          | Function ("return", _) -> raise EarlyAbort
          | Block nested_directives -> process_directives in_platform_context nested_directives
          | Condition (cond, then', elsebranches) ->
            let branches = (Some cond, then') :: elsebranches in
            let next_in_platform_context =
              in_platform_context ||
              list_any branches (fun (cond, _) -> match cond with
                | Some cond -> is_platform_condition cond
                | None -> false)
            in
            let rec evaluate_branch branches = match branches with
            | (cond, directives) :: rest -> (
              let result = match cond with
              | None -> true
              | Some cond -> evaluate_condition env cond
              in
              match result with
              | false -> evaluate_branch rest
              | true -> (match directives with
                | Some x -> process_directives next_in_platform_context [x]
                | None -> ())
              )
            | [] -> ()
            in
            evaluate_branch branches
          | _ -> ()
          )
      in
      try process_directives in_platform_context directives
      with EarlyAbort -> ()
    in
    process_file false doc docpath;
    finalize_srcgroup srcs
  in
  let iter platform =
    let is_platform = platform <> "" in
    let env = if not (is_platform)
      then
        let defns = Hashtbl.create 16 in
        let () = for_ht_keys platform_names (fun platform ->
          add_platform_definitions defns platform
        ) in
        {
          definitions = defns;
          parent = Some default_eval_environment;
        }
      else
        {
          definitions = generate_platform_definitions platform;
          parent = Some default_eval_environment;
        }
    in
    (platform, process_with_env env is_platform)
  in
  List.map iter platforms


let emit_lua root_dir file_lists =
  let open Printf in
  for_ht_kv file_lists (fun (path, (name, srcs)) ->
      let print_path path = printf "\n    \"%s\"," path in
      printf "\nlocal %s = { }\n" name;
      for_list srcs (fun (platform, srcs) ->
        let platform = if platform <> "" then platform else "generic" in
        printf "\n%s.%s = {" name platform;
        for_list srcs.sources print_path;
        for_list srcs.headers print_path;
        for_list srcs.resources print_path;
        for_list srcs.forms print_path;
        printf "\n}\n";
      );
  );
  printf "\nreturn {";
  for_ht_kv file_lists (fun (_, (name, _)) ->
    printf "\n    %s = %s," name name
  );
  printf "\n}\n";
  ()

let emit_cmake root_dir file_lists =
  let open Printf in
  let file_lists = Array.of_seq (Hashtbl.to_seq file_lists) in
  Array.sort (fun (_, (namel, _)) (_, (namer, _)) -> compare namel namer) file_lists;
  let file_lists = Array.map (fun (path, (name, srcs)) -> (path, (name, List.sort compare srcs))) file_lists in
  for_arr file_lists (fun (path, (name, srcs)) ->
      let print_path path = printf "\n    \"%s\"" path in
      for_list srcs (fun (platform, srcs) ->
        let platform = if platform <> "" then platform else "generic" in
        printf "\nset(SQT_FILES_%s_%s" name platform;
        for_list srcs.sources print_path;
        for_list srcs.headers print_path;
        for_list srcs.resources print_path;
        for_list srcs.forms print_path;
        printf "\n)\n";
      );
  );
  printf "\nset(SQT_ALL_MODULES";
  for_arr file_lists (fun (_, (name, _)) ->
    printf "\n    %s" name);
  printf ")\n";
  ()

let parse_cli_args args =
  let args = Array.to_list args in
  let rec work acc args =
    match args with
    | name :: path :: args -> Printf.printf "name = %s, path = %s\n" name path; work ((name, path) :: acc) args
    | [name] -> failwith (Printf.sprintf "found unbalaned positional parameter %s" name)
    | [] -> List.rev acc
  in
  let (root_dir, args) = match args with
  | _argv0 :: root_dir :: args -> (root_dir, args)
  | [_] | []  -> failwith "missing root_dir (positional parameter 0)"
  in
  let names_and_paths = work [] args in
  (root_dir, names_and_paths)

let main () =
  let open Printf in
  let parsed = Hashtbl.create 16 in
  let ast_dump = false in
  (try
    let (root_dir, names_and_paths) = parse_cli_args Sys.argv in
    let file_lists = Hashtbl.create 16 in
    for_list names_and_paths (fun (name, path) ->
      let (doc, docpath) = parse_or_exn parsed path in
      let srcs = accumulate_sources_and_headers parsed root_dir doc docpath in
      Hashtbl.replace file_lists path (name, srcs)
    );
    (*
    emit_lua root_dir file_lists;
    *)
    emit_cmake root_dir file_lists;
    ()
  with ParseFailure -> ());
  (if ast_dump
  then
    for_ht_kv parsed (fun (path, doc) ->
      printf "\n(%s\n" path;
      for_list doc (fun elem -> ppr_directive 0 elem);
      printf "\n)\n";
    )
  else ())
;;

main ()
