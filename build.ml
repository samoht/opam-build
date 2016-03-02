(*
 * Copyright (c) 2016 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Astring

let () = Fmt_tty.setup_std_outputs ()

(* PRELUDE *)
(* FIXME: use Bos *)

let debug = ref true

let date () =
  let now = Unix.gettimeofday () in
  (* Feb 26 07:07:50 *)
  let local = Unix.localtime now in
  let month = match local.Unix.tm_mon with
    | 0 -> "Jan" | 1 -> "Feb" | 2 -> "Mar" | 3 -> "Apr" | 4 -> "May" | 5 -> "Jun"
    | 6 -> "Jul" | 7 -> "Aug" | 8 -> "Sep" | 9 -> "Oct" | 10 -> "Nov" | 11 -> "Dec"
    | _ -> assert false in
  Printf.sprintf "%s %d %02d:%02d:%02d" month local.Unix.tm_mday
    local.Unix.tm_hour local.Unix.tm_min local.Unix.tm_sec

let err fmt = Printf.ksprintf (fun e -> failwith (date () ^ " " ^ e)) fmt
let fail cmd fmt = Printf.ksprintf (fun e -> err "%s %s: %s" (date ()) cmd e) fmt
let failv cmd fmt =
  Printf.ksprintf (fun e -> `Error (Printf.sprintf "%s %s: %s" (date ()) cmd e)) fmt

let show fmt =
  Fmt.kstrf (fun str ->
      Fmt.(pf stdout) "%a\n%!" Fmt.(styled `Cyan string) str
    ) fmt

let debug fmt =
  Fmt.kstrf (fun cmd ->
      if !debug then
        Fmt.(pf stdout) "%s %a %s\n%!"
          (date ()) Fmt.(styled `Yellow string) "=>" cmd
    ) fmt

let check_exit_status cmd (out, err) status =
  match status with
  | Unix.WEXITED 0   ->
    debug "command successful";
    `Ok (out, err)
  | Unix.WEXITED i   ->
    debug "command failed with code %d" i;
    failv cmd "exit %d" i
  | Unix.WSIGNALED i ->
    debug "command caught signal %d" i;
    if i = Sys.sigkill then fail cmd "timeout" else fail cmd "signal %d" i
  | Unix.WSTOPPED i  ->
    debug "command stopped %d" i;
    fail cmd "stopped %d" i

let read_lines ?(prefix="") oc =
  let rec aux acc =
    let line =
      try
        let line = input_line oc in
        debug "%s %s" prefix line;
        Some line
      with End_of_file -> None
    in
    match line with
    | Some l -> aux (l :: acc)
    | None   -> List.rev acc
  in
  aux []

let syscall ?env cmd =
  let env = match env with None -> Unix.environment () | Some e -> e in
  let oc, ic, ec = Unix.open_process_full cmd env in
  let out = ref [] in
  let err = ref [] in
  (* FIXME: deadlock *)
  out := read_lines ~prefix:"stdout:" oc;
  err := read_lines ~prefix:"stderr:" ec;
  let exit_status = Unix.close_process_full (oc, ic, ec) in
  check_exit_status cmd (!out, !err) exit_status

let read_outputs ?env fmt = Printf.ksprintf (syscall ?env) fmt

let exec ?env fmt =
  Printf.ksprintf (fun cmd ->
      debug "%s" cmd;
      match read_outputs ?env "%s" cmd with
      | `Ok _    -> ()
      | `Error e -> err "exec: %s" e
    ) fmt

let read_stdout ?env fmt =
  Printf.ksprintf (fun cmd ->
      debug "%s" cmd;
      match read_outputs ?env "%s" cmd with
      | `Ok (out, _) -> out
      | `Error e     -> err "exec: %s" e
    ) fmt

let read_line ?env fmt =
  Printf.ksprintf (fun str ->
      match read_stdout ?env "%s" str with
      | []   -> ""
      | h::_ -> h
    ) fmt

let in_dir dir f =
  let pwd = Sys.getcwd () in
  let reset () = if pwd <> dir then Sys.chdir pwd in
  if pwd <> dir then Sys.chdir dir;
  try let r = f () in reset (); r
  with e -> reset (); raise e

let rmdir path = exec "rm -rf %s" path
let mkdir path = exec "mkdir -p %s" path
let (/) = Filename.concat

let some x = Some x

(* END OF PRELUDE *)

let opam ?env ?root fmt =
  let root = match root with
    | None   -> ""
    | Some s -> " --root=" ^ s
  in
  Printf.ksprintf (fun cmd ->
      exec ?env "OPAMYES=1 opam %s%s" cmd root
    ) fmt

let opam_readl ?env ?root fmt =
  let root = match root with
    | None   -> ""
    | Some s -> " --root=" ^ s
  in
  Printf.ksprintf (fun cmd ->
      read_line ?env "OPAMYES=1 opam %s%s" cmd root
    ) fmt

(* let opam_info ?root ~pkg field = opam_readl ?root "info %s -f %s" pkg field *)

type pin = string * [`Local of string | `Dev | `Other of string]

let string_of_pin (k, v) = match v with
  | `Other path
  | `Local path -> k ^ ":" ^ path
  | `Dev        -> k

let string_of_pins pins = String.concat ~sep:" " @@ List.map string_of_pin pins

let pin_of_string str =
  match String.cut ~sep:":" str with
  | None              -> str, `Dev
  | Some (name, path) ->
    if Sys.file_exists path && Sys.is_directory path then name, `Local path
    else name, `Other path

let pins_of_string str =
  String.cuts ~sep:" " ~empty:false str
  |> List.map pin_of_string

type config = {
  name: string;
  ocaml_version: string;
  compiler: string;
  preinstalled: bool;
  os: string;
  pins: pin list;
}

let pp_config ppf config =
  let pp k v = Fmt.(pf ppf "%s %a\n" k (styled `Yellow @@ string) v) in
  pp "name:         " config.name;
  pp "ocaml-version:" config.ocaml_version;
  pp "compiler:     " config.compiler;
  pp "preinstalled: " (string_of_bool config.preinstalled);
  pp "os:           " config.os;
  pp "pins:         " (string_of_pins config.pins)

let default_config () =
  let ocaml_version = opam_readl "config var ocaml-version" in
  let compiler = opam_readl "config var compiler" in
  let preinstalled = opam_readl "config var preinstalled" |> bool_of_string in
  let os = opam_readl "config var os" in
  let name = "local-pkg" in
  let pins = [] in
  { name; ocaml_version; compiler; preinstalled; os; pins }

(* FIXME: this is horrible, I'm so sorry... *)
let config_of_string str =
  let read ~d k =
    match read_line "echo '%s' | jq '.%s' | xargs" str k with
    | "null" -> d
    | x      -> x
  in
  let name = read ~d:"local-pkg" "name" in
  let default_config = default_config () in
  let ocaml_version = read ~d:default_config.ocaml_version "ocaml" in
  let compiler = read ~d:default_config.compiler "compiler" in
  let preinstalled =
    read ~d:(string_of_bool default_config.preinstalled) "preinstalled"
    |> bool_of_string
  in
  let os = read ~d:default_config.os "os" in
  let pins = read ~d:"" "pins" |> pins_of_string in
  { name; ocaml_version; compiler; preinstalled; os; pins }

let string_of_config t =
  let one (k, v) = Printf.sprintf "%S: %S" k v in
  "{ " ^
  String.concat ~sep:", " (List.map one [
      "name"         , t.name;
      "ocaml"        , t.ocaml_version;
      "preinstalled" , string_of_bool t.preinstalled;
      "os"           , t.os;
      "pins"         , string_of_pins t.pins;
    ])
  ^ " }"

let read_config opamconfig =
  let ic = open_in opamconfig in
  let contents = input_line ic in
  close_in ic;
  config_of_string contents

let write_config t opamconfig =
  let contents = string_of_config t in
  let oc = open_out opamconfig in
  output_string oc contents;
  close_out oc

let add_to_env extra_env =
  Array.of_list extra_env |>
  Array.map (fun (k,v) -> k^"="^v) |>
  Array.append (Unix.environment ())

let env config =
  let extra_env = [
    "OPAMVAR_ocaml_version" , config.ocaml_version;
    "OPAMVAR_compiler"      , config.compiler;
    "OPAMVAR_preinstalled"  , string_of_bool config.preinstalled;
    "OPAMVAR_os"            , config.os;
    "OPAMYES"               , "1";
  ] in
  add_to_env extra_env

let name s = match String.cut ~sep:"." s with Some (n, _) -> n | None -> s

let finally f g =
  try let r = f () in g (); r
  with e -> g (); raise e

let packages_to_install t ~root_pkg =
  (* 1. we create a temporary switch to have a clean universe where
     we can call the solver *)
  let json = Filename.get_temp_dir_name () / "pkg-" ^ t.name ^ ".json" in
  let tmp_switch = "tmpswitch-" ^ string_of_int @@ Random.int 1024 in
  let current_switch = opam_readl "switch show" in
  finally (fun () ->
      (* FIXME: seems that --no-switch is broken for opam 1.2.2 *)
      opam "switch %s -A system --no-switch" tmp_switch;
      opam "pin add %s . -n" root_pkg;
      show "Calling the solver...";
      let env = env t in
      opam ~env "install %s --dry-run --json=\"%s\" --switch=%s" t.name json tmp_switch;
    ) (fun () ->
      opam "switch %s" current_switch;
      opam "switch remove %s" tmp_switch
    );
  (* 2. parse the JSON file that the solver returned. I'm sorry. *)
  let all_packages =
    read_line
      "jq '.[] | map(select(.install)) | \
       map( [.install.name, .install.version] | join(\".\")) | \
       join(\" \")' \"%s\"" json
  in
  let all_packages = String.trim ~drop:((=)'"') all_packages in
  let all_packages = String.cuts ~sep:" " ~empty:false all_packages in
  all_packages

let opam_source t ~sources_dir ~root_pkg pkgs =
  (* 3. gather all the metada in the global opam state, and populate
     sources/ *)
  in_dir sources_dir (fun () ->
      List.iter (fun pkg ->
          if pkg <> root_pkg then (
            let pkg_name = name pkg in
            let pin =
              try List.find (fun (p, _) -> p = pkg_name) t.pins |> snd |> some
              with Not_found -> None
            in
            match pin with
            | None ->
              if Sys.file_exists pkg then
                show "pkg: %s, already there\n%!" pkg
              else (
                show "pkg: %s, getting the sources" pkg;
                opam "source %s" pkg;
              )
            | Some (`Local path) ->
              show "pkg: %s, linking from %s" pkg path;
              exec "ln -s %s %s" path pkg;
            | _  -> err "TODO"
          )
        ) pkgs
    );
  show "The sources are all in %s" sources_dir

let local_pin t ~root_pkg ~sources_dir ~build_dir pkgs =
  let root = build_dir in
  (* 4. we create an opam root in the repo and pin all the local
     packages in there. *)
  if not (Sys.file_exists root) then (
    (* FIXME: we should be able to init opam with an empty repo! *)
    exec "opam init -n --root=%s --compiler=%s" root t.compiler;
    exec "opam remote remove default --root=%s" root;
  );
  List.iter (fun pkg ->
      if pkg = root_pkg then
        (* FIXME: cannot pin if opam root is a subdir *)
        (* opam ~root "pin add %s . -n" (name pkg) *)
        ()
      else
        opam ~root "pin add %s %s -n" pkg (sources_dir / pkg)
    ) pkgs;
  show "All the packages are pinned to the local sources"

let setup t ~sources_dir =
  let root_pkg = t.name ^ ".root" in
  let all_packages = packages_to_install t ~root_pkg in

  if not (Sys.file_exists sources_dir) || not (Sys.is_directory sources_dir) then
    mkdir "sources";
  opam_source t ~sources_dir ~root_pkg all_packages;
  local_pin t ~sources_dir ~root_pkg all_packages

let build ~sources_dir ~build_dir =
  let root = build_dir in
  let t0 = Unix.time () in
  (* FIXME: this doesn't seem to work with opam 1.2.2 *)
  let packages = Array.to_list @@ Sys.readdir sources_dir in
  opam ~root "install %s" (String.concat ~sep:" " packages);
  (* FIXME: run the right commands *)
  exec "opam config exec --root=%s -- make" root;
  show "Project built in %.2f" (Unix.time () -. t0)

let () =
  let cwd = Sys.getcwd () in
  let opamconfig = cwd / ".opamconfig" in
  let sources_dir = cwd / "sources" in
  let build_dir = cwd / "build" in
  let init () =
    rmdir sources_dir;
    rmdir build_dir;
    let config = default_config () in
    write_config config opamconfig;
    config
  in
  let config =
    if not (Sys.file_exists opamconfig) then init ()
    else (
      show "%s exists, picking an existing configuration" opamconfig;
      try read_config opamconfig
      with Failure _ -> init ()
    )
  in
  Fmt.(pf stdout) "%a" pp_config config;
  setup config ~sources_dir ~build_dir;
  build ~sources_dir ~build_dir
