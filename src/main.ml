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
let inputFilename = ref ""
let inputDirectory = ref ""
let outputBasename = ref ""
let outputDirectory = ref ""

let chopExtension s =
  try Filename.chop_extension s
  with Invalid_argument(_) -> s

let setOutputBasename s =
  (outputDirectory := Filename.dirname s;
  outputBasename := Filename.basename s)

let setInputFilename s =
  if Filename.is_implicit s then
    (inputFilename := s;
    inputDirectory := Filename.current_dir_name)
  else
    (inputFilename := Filename.basename s;
    inputDirectory := Filename.dirname s)

(*  version: print version information. *)
let printVersion () =
  (Errormsg.print Errormsg.none ("Parinati version " ^ Options.version);
  exit 0)

(*  Make sure that there is some input file.  *)
let checkInput () =
  if !inputFilename = "" then
    (print_endline "Error: no input file specified";
    exit (-1))
  else
    ()

(*  Fix up when the output is just a directory. *)
let setOutput () =
  if !outputBasename = "" then
    setOutputBasename (chopExtension !inputFilename)
  else
    ()

(**********************************************************************
*parseArguments:
* Handles printing usage information, handling arguments, etc.
**********************************************************************)
let parseArguments () =
  let speclist =
    [("--input", Arg.String(setInputFilename), "input file");
    ("-i", Arg.String(setInputFilename), "input file");
    
    ("--output", Arg.String(setOutputBasename), "output directory and module name");
    ("-o", Arg.String(setOutputBasename), "output directory and module name");
    
    ("--translation", Arg.String(Options.setTranslation), "translation: original, simplified, optimized, extended");
    ("-t", Arg.String(Options.setTranslation), "translation: original, simplified, optimized, extended");
    
    ("--opt", Arg.Set(Options.optimizations), "enable default optimizations");
    ("--opt-index", Arg.Set(Options.indexingOptimization), "enable indexing order optimization");
    ("--opt-pts", Arg.Set(Options.noProofTermsOptimization), "enable proof term erasure optimization");
    
    ("--log", Arg.Set(Errormsg.loggingEnabled), "enable logging information");
    ("--version", Arg.Unit(printVersion), "show version information")]
  in
  Arg.parse speclist
    (fun _ -> Errormsg.error Errormsg.none "specify input using -i or --input"; exit (-1))
    ("Usage: parinati -i <LF specification> -t <original|simplified|optimized|extended>")

(**********************************************************************
*postParseActions:
* A list of functions to be executed after parsing arguments.
**********************************************************************)
let postParseActions = [Options.setDefaultOptimizations; checkInput; setOutput]

(**********************************************************************
*translate:
* Given a Twelf specification, translate it to lambdaProlog and
* then output it.
**********************************************************************)
let translate twelf =
  let moduleName = !outputBasename in  
  let modFilename = Filename.concat (!outputDirectory) (moduleName ^ ".mod") in
  let sigFilename = Filename.concat (!outputDirectory) (moduleName ^ ".sig") in

  let lp = Translate.translate moduleName twelf in
  match lp with
      Some lp' ->
        let (sig', mod') = Lp.string_of_absyn lp' in

        let oMod = Parse.openFile modFilename open_out in
        let oSig = Parse.openFile sigFilename open_out in
        
        (output_string oSig sig';
        output_string oMod mod';
        close_out oSig;
        close_out oMod;
        print_endline "Done.";
        exit 0)
    | None ->
        (Errormsg.error Errormsg.none "unable to translate specification.";
        exit (-1))

(**********************************************************************
*main:
* Entrypoint.
**********************************************************************)
let main () =
  let () = parseArguments () in
  let () = List.iter (fun f -> f ()) postParseActions in
  
  let input = Filename.concat (!inputDirectory) (!inputFilename) in
  let twelf = Parse.parse input in
  match twelf with
      Some(twelf') ->
        (* Rudimentary error checking. *)
        if Twelf.typecheck twelf' then
          translate twelf'
        else
          (Errormsg.error Errormsg.none "specification has errors";
          exit (-1))
    | None ->
        (Errormsg.error Errormsg.none "unable to parse specification";
        exit (-1))
let () = main ()
