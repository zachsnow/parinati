(**********************************************************************
*Copyright 2008, 2009 Zach Snow
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
(**********************************************************************
*Optimizations:
* There are several available optimizations; the default ones are
* used when 'optimizations' is set, along with any others that are
* set.  If 'optimizations' is not set, just the manually specified
* optimizations are used.
**********************************************************************)
let version = "0.3.0"

(* Optimization options *)
let optimizations = ref false
let indexingOptimization = ref false
let typeOptimization = ref false
let noProofTermsOptimization = ref false

let setDefaultOptimizations () =
  ();

(* Translation options *)
type translation =
    Original
  | Simplified
  | Optimized
  | Extended
  | Strange
  
let translation = ref Original
let setTranslation s =
  (match s with
    "original" -> translation := Original
  | "simplified" -> translation := Simplified
  | "optimized" -> translation := Optimized
  | "extended" -> translation := Extended
  | "strange" -> translation := Strange
  | _ ->
    Errormsg.error Errormsg.none ("invalid translation: " ^ s);
  
  if !noProofTermsOptimization && !translation <> Extended then
    Errormsg.error Errormsg.none ("proof term optimization may only be used with the extended translation");
  
  ())
