parinati
========

A somewhat broken implementation of LF logic programming
--------------------------------------------------------

Parinati is a tool for "compiling" LF specifications in the style of
Twelf to lambdaProlog modules suitable for use with the Teyjus implementation
of lambdaProlog.  Parinati was written by Zach Snow.

Parinati is Free Software, released under the GPL version 3 for now; if you'd
prefer another license just let me know.

Parinati lives here now: http://github.com/zachsnow/parinati

Building
--------

  * Dependencies:
    - O'Caml (http://caml.inria.fr)
    - ocamake (see /ocamake/)
        Building ocamake:
          
            # ocamlc unix.cma str.cma ocamake.ml -o ocamake
        
        Be sure to place the binary in your path (consider the /bin directory
        of your O'Caml installation).
    
    - Microsoft Visual Studio 2008 (for Windows only)

  * Building (Windows):
    Just build the included Microsoft Visual Studio 2008 project;
    alternatively run `compile.bat` (requires the relevant Visual Studio tools
    to be in your path, along with ocamake).

  * Building (*nix):
    Execute the following:
    
        ./compile.sh
    
    Place the resulting executable (parinati) in your path as well.
 
                                
General Usage
-------------
  
      parinati -i <LF specification> -t <original|simplified|optimized|extended>
    
        --input input file
        -i input file
        --output output directory and module name
        -o output directory and module name
        --translation translation: original, simplified, optimized, extended
        -t translation: original, simplified, optimized, extended
        --opt enable default optimizations
        --opt-index enable indexing order optimization
        --opt-pts enable proof term erasure optimization
        --opt-types enable type embedding optimization
        --log enable logging information
        --version show version information
        -help  Display this list of options
        --help  Display this list of options

  Running the translator on an LF specification produces a lambdaProlog module
  and signature that can be compiled, linked, and run on the Teyjus simulator.

  The translator assumes that the input is a *valid* and *fully explicit*
  LF specification; in particular no checking is done to ensure type-correctness.
  Therefore it is possible that though translation succeeds, compilation using
  tjcc will fail; this should always be due to an ill-formed specification (if
  not it indicates a bug in parinati).

  For examples specifications, see `/examples/` (and `/timing/`, which includes
  some additional specifications, and some duplicates).

  To run queries specified by %solve, (of the form `%solve query_name t.`)
  you should load the generated module in tjsim and run the query
  `query_name Proof A1 A2 ... An.`, where n is the number of free variables in
  `t`, and `query_name` is the name of the query.  Other forms of queryies
  supported by Twelf (in particular `%query` and the "top-level") are not supported,
  however it is possible to use tjsim in an interactive mode if you understand
  how LF types are translated to lambdaProlog terms.
    
Supported Features
------------------

  Parinati supports only fully explicit and valid LF specifications written
  in Twelf.  It does not support definitions or any annotations (%name, %infix,
  etc.)
  
  If you have an LF specification written in implicit style, with perhaps several
  infix operators, you may wish to generate a specification that can be used
  with Parinati.  To generate such a specification from a specification "spec.lf",
  first load up the Twelf SML image.  Then set the following flags:
  
      Twelf.Print.implicit := true;;
      Twelf.Print.printInfix := false;;
    
  Then load the file:
  
      Twelf.loadFile();;
    
  Twelf will print the specification in a form that Parinati will understand.
  
Example
-------

  Given an LF specification "nat.lf" defining natural numbers and the property
  of a natural being even, and with a query for determining whether 4 is even:
  
      nat : type.
      z : nat.
      s : nat -> nat.
      
      even : nat -> type.
      even-z : even z.
      even-s : {N : nat} even N -> even (s (s N)).
      
      %solve q : even (s (s (s (s z)))).
  
  We can generate a lambdaProlog module as follows:
  
      parinati --input nat.lf --translation extended
    
  This generates two files, nat.mod and nat.sig.  Compile and link them using
  Teyjus:
  
      $ tjcc nat
      $ tjlink nat
  
  Then load it into the Teyjus simulator:
  
      $ tjsim nat
    
  To execute the %solve query you must execute a lambdaProlog query within
  tjsim:
  
      -? q Proof.
    
  A query of the form `%solve name : type` generates a lambdaProlog predicate
  `name` taking one argument, which corresponds to the inhabitant of the type.
  If the type contains N logic variables (for instance, `even X`), then the
  predicate takes N + 1 arguments -- N for the instantiable variables, and 1 for
  the inhabitant.
  
  Note that the modules generated by parinati are not pretty-printed.  Use the
  Teyjus tool tjparse (available in the Teyjus SVN) to generate a more readable
  lambdaProlog module.
