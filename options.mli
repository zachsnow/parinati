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
val version : string

(**********************************************************************
* Optimizations:
**********************************************************************)
val optimizations : bool ref
val indexingOptimization : bool ref
val noProofTermsOptimization : bool ref

val setDefaultOptimizations : unit -> unit

(**********************************************************************
* Translation:
**********************************************************************)
type translation =
    Original
  | Simplified
  | Optimized
  | Extended
  | Strange
  
val translation : translation ref
val setTranslation : string -> unit