(****************************************************************************
* Copyright 2008-2012 Zach Snow
****************************************************************************)
(****************************************************************************
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
****************************************************************************)
(****************************************************************************
*open_file:
* Opens a file by filename; exits if there is an error.
****************************************************************************)
let open_file open_f filename =
  try
    let inchannel = open_f filename in
    inchannel
  with Sys_error(s) ->
    (Errormsg.error Errormsg.none ("unable to open file '" ^ filename ^ "'.");
    exit (-1))

(****************************************************************************
*parse:
* Parses the Twelf file given by filename and returns an AST, or
* None on error.
****************************************************************************)
let parse filename =
  try
    let inchannel = open_file open_in filename in
    let lexbuf = Lexing.from_channel inchannel in
    let () = Lflexer.setFileName lexbuf filename in
    let result = Lfparser.parse Lflexer.initial lexbuf in
    (close_in inchannel;
    Some result)
  with
      Parsing.Parse_error ->
        (Errormsg.error Errormsg.none "syntax error";
        None)
    | Failure(s) ->
        (Errormsg.error Errormsg.none s;
        None)
