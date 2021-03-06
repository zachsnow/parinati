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
val version : string

(**********************************************************************
* Optimizations:
**********************************************************************)
val optimizations : bool ref
val type_embedding_optimization : bool ref
val indexing_optimization : bool ref
val proof_terms_optimization : bool ref

val check_optimizations : unit -> unit

(**********************************************************************
* Translation:
**********************************************************************)
type translation =
    Original
  | Simplified
  | Optimized

val translation : translation ref
val set_translation : string -> unit
