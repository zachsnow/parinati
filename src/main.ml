(**********************************************************************
* Copyright 2008-2012 Zach Snow
**********************************************************************)
(**********************************************************************
* This file is part of Parinati.
*
* Parinati is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* Parinati is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Parinati.  If not, see <http://www.gnu.org/licenses/>.
**********************************************************************)
let input_filename = ref ""
let input_directory = ref ""
let output_basename = ref ""
let output_directory = ref ""

let chop_extension s =
  try Filename.chop_extension s
  with Invalid_argument(_) -> s

let set_output_basename s =
  (output_directory := Filename.dirname s;
  output_basename := Filename.basename s)

let set_input_filename s =
  if Filename.is_implicit s then
    (input_filename := s;
    input_directory := Filename.current_dir_name)
  else
    (input_filename := Filename.basename s;
    input_directory := Filename.dirname s)

(*  version: print version information. *)
let print_version () =
  (Errormsg.print Errormsg.none ("Parinati version " ^ Options.version);
  exit 0)

(*  Make sure that there is some input file.  *)
let check_input () =
  if !input_filename = "" then
    (print_endline "Error: no input file specified";
    exit (-1))
  else
    ()

(*  Fix up when the output is just a directory. *)
let set_output () =
  if !output_basename = "" then
    set_output_basename (chop_extension !input_filename)
  else
    ()

(**********************************************************************
*parseArguments:
* Handles printing usage information, handling arguments, etc.
**********************************************************************)
let parse_arguments () =
  let speclist =
    [("--input", Arg.String(set_input_filename), "input file");
    ("-i", Arg.String(set_input_filename), "input file");
    
    ("--output", Arg.String(set_output_basename), "output directory and module name");
    ("-o", Arg.String(set_output_basename), "output directory and module name");
    
    ("--translation", Arg.String(Options.set_translation), "translation: original, simplified, optimized, extended");
    ("-t", Arg.String(Options.set_translation), "translation: original, simplified, optimized, extended");
    
    ("--opt", Arg.Set(Options.optimizations), "enable default optimizations");
    ("--opt-index", Arg.Set(Options.indexing_optimization), "enable indexing order optimization");
    ("--opt-pts", Arg.Set(Options.proof_terms_optimization), "enable proof term erasure optimization");
    ("--opt-type", Arg.Set(Options.type_embedding_optimization), "enable type embedding optimization");
    
    ("--log", Arg.Set(Errormsg.logging_enabled), "enable logging information");
    ("--version", Arg.Unit(print_version), "show version information")]
  in
  Arg.parse speclist
    (fun _ -> Errormsg.error Errormsg.none "specify input using -i or --input"; exit (-1))
    ("Usage: parinati -i <LF specification> -t <original|simplified|optimized>")

(**********************************************************************
*post_parse_actions:
* A list of functions to be executed after parsing arguments.
**********************************************************************)
let post_parse_actions = [
  Options.check_optimizations;
  check_input;
  set_output
]

(**********************************************************************
*translate:
* Given a Twelf specification, translate it to lambdaProlog and
* then output it.
**********************************************************************)
let translate twelf =
  let module_name = !output_basename in  
  let mod_filename = Filename.concat (!output_directory) (module_name ^ ".mod") in
  let sig_filename = Filename.concat (!output_directory) (module_name ^ ".sig") in

  let lp = Translate.translate module_name twelf in
  match lp with
      Some lp' ->
        let (sig', mod') = Lp.string_of_absyn lp' in

        let o_mod = Parse.open_file mod_filename open_out in
        let o_sig = Parse.open_file sig_filename open_out in
        
        (output_string o_sig sig';
        output_string o_mod mod';
        close_out o_sig;
        close_out o_mod;
        print_endline "Done.";
        exit 0)
    | None ->
        (Errormsg.error Errormsg.none "unable to translate specification.";
        exit (-1))

(**********************************************************************
*main:
**********************************************************************)
let main () =
  let () = parse_arguments () in
  let () = List.iter (fun f -> f ()) post_parse_actions in
  
  let input = Filename.concat (!input_directory) (!input_filename) in
  let twelf = Parse.parse input in
  match twelf with
    | Some(twelf') ->
        (* Rudimentary error checking. *)
        if Twelf.typecheck twelf' then
          translate twelf'
        else
          (Errormsg.error Errormsg.none "specification has errors";
          exit (-1))
    | None ->
        (Errormsg.error Errormsg.none "unable to parse specification";
        exit (-1))

(* Entrypoint *)
let () = main ()
