%{
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
(**********************************************************************
* Lfparser
**********************************************************************
* This module implements a rudimentary parser for LF.  It assumes
* that the module is fully explicit, and ignores many Twelf commands
* not relevant to Parinati.  It also isn't quite correct (universal
* and lambda abstraction have the wrong precedences).
**********************************************************************)
open Lexing

(**********************************************************************
*getPos:
* Gets the character position of the given token.
**********************************************************************)
let getPos i =
  Parsing.rhs_start_pos i

let contextItems = ref []
let declarations = ref []
let domains = ref []

let add el l =
  l := !l @ [el]

let addContextItem i = add i contextItems
let addDeclaration d = add d declarations

let reset () =
  (contextItems := [];
  declarations := [])

%}

%token SOLVE QUERY USE INFIX PREFIX POSTFIX
%token TYPE KIND ARROW REVERSE_ARROW COLON DOT APP
%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK LANGLE RANGLE
%token <string> ID
%token END

%type <Twelf.absyn> parse

%start parse

%%
parse
  : program     {$1}
  | program END {$1}
  ;

program
  : declaration_list  {let result = Twelf.Specification(!contextItems, !declarations) in (reset (); result)}
  |                   {(reset (); Twelf.Specification([], []))}
  ;

declaration_list
  : declaration                        {()}
  | declaration declaration_list       {()}
  ;

declaration
  : context_item  {addContextItem $1}
  | solve         {addDeclaration $1}
  | use           {Errormsg.warning (getPos 1) "constraint domains are not implemented"; addDeclaration $1}
  | fixity        {Errormsg.warning (getPos 1) "fixity declarations are ignored"}
  ;

fixity
  : fix ID ID ID DOT  {()}
  ;

fix
  : INFIX             {()}
  | PREFIX            {()}
  | POSTFIX           {()}
  ;

solve
  : SOLVE ID COLON term DOT {Twelf.Solve($2, $4, getPos 1)}
  ;

use
  : USE ID DOT              {Errormsg.warning (getPos 1) "constraint domains are ignored"; Twelf.Domain($2, getPos 1)}
  ;

context_item
  : ID COLON term DOT  {Twelf.Assertion(Twelf.IdTerm($1, getPos 1), $3, getPos 2)}
  ;

ground_term
  : ID                              {Twelf.IdTerm($1, getPos 1)}
  | TYPE                            {Twelf.Type(getPos 1)}
  | LPAREN term RPAREN              {$2}
  ;

application_term
  : application_term ground_term    {Twelf.ApplicationTerm($1, $2, getPos 1)}
  | ground_term                     {$1}
  ;

rarrow_term
  : rarrow_term REVERSE_ARROW application_term  {Twelf.ImplicationTerm($3, $1, getPos 2)}
  | application_term {$1}
  ;
 
arrow_term
  : rarrow_term ARROW arrow_term          {Twelf.ImplicationTerm($1, $3, getPos 2)}
  | rarrow_term                           {$1}
  ;

prefix_term
  : LBRACE ID COLON term RBRACE prefix_term  {Twelf.PiTerm($2, $4, $6, getPos 1)}
  | LBRACK ID COLON term RBRACK prefix_term  {Twelf.AbstractionTerm($2, $4, $6, getPos 1)}
  | LANGLE ID COLON term RANGLE prefix_term  {Twelf.AlephTerm($2, $4, $6, getPos 1)}
  | arrow_term                               {$1}
  ;

term
  : prefix_term                {$1}
  ;

%%
