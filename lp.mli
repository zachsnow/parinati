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
type absyn = Program of string * lpsignature * lpmodule

and lpsignature = Signature of lptype list * constant list
and lpmodule = Module of declaration list

and declaration =
    Term of term
  | Accum of string
and lptype = Type of string * int
and constant = Constant of string * ty

and term =
    IdTerm of string
  | ApplicationTerm of term * term list
  | AbstractionTerm of string * term
  | ImplicationTerm of term * term
  | ConjunctionTerm of term * term
  | PiTerm of term

and ty =
    IdType of string
  | VariableType of string
  | ArrowType of ty * ty

val idType : string -> ty
val arrowType : ty list -> ty -> ty
val predicateType : ty

val top : term
val forAll : string -> term -> term
val implies : term -> term -> term

val string_of_absyn : absyn -> string * string
val string_of_term : term -> string
val string_of_type : ty -> string

val targetType : ty -> ty
val argumentTypes : ty -> ty list
val getTypeHeadAndArgs : term -> (string * term list) option

val normalize : term -> term
val elide : term -> term option