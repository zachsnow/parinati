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
type pos = Errormsg.pos
type absyn = Specification of contextitem list * declaration list

and contextitem = assertion
and assertion = Assertion of term * term * pos

and declaration =
    Solve of string * term * pos
  | Domain of string * pos

and term =
    IdTerm of string * pos
  | AbstractionTerm of string * term * term * pos
  | ApplicationTerm of term * term * pos
  | ImplicationTerm of term * term * pos
  | PiTerm of string * term * term * pos
  | Type of pos

val getContextItemPos : contextitem -> pos
val string_of_contextitem : contextitem -> string

val getDeclarationPos : declaration -> pos

val getTermPos : term -> pos
val string_of_term : term -> string
val freeVariables : term -> string list

val isUniversal : string -> bool

val typecheck : absyn -> bool
