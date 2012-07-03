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
let version = "0.3.0"

(**********************************************************************
*Optimizations:
* There are several available optimizations; the default ones are
* used when 'optimizations' is set, along with any others that are
* set.  If 'optimizations' is not set, just the manually specified
* optimizations are used.
**********************************************************************)
(* Optimization options *)
let optimizations = ref false
let type_embedding_optimization = ref false
let indexing_optimization = ref false
let proof_terms_optimization = ref false

let check_optimizations () =
  (if !optimizations then
    indexing_optimization := true
  else
    ();
    
  if !proof_terms_optimization && (not !type_embedding_optimization) then
    Errormsg.error Errormsg.none ("proof term optimization may only be used type embedding optimization")
  else
    ())


(**********************************************************************
*Translations:
**********************************************************************)
type translation =
    Original
  | Simplified
  | Optimized
  
let translation = ref Original
let set_translation s =
  match s with
  | "original" -> translation := Original
  | "simplified" -> translation := Simplified
  | "optimized" -> translation := Optimized
  | _ ->
    Errormsg.error Errormsg.none ("invalid translation: " ^ s)
