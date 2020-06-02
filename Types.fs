module Types

//-- values. we interpret Exp into. "model of untyped CBV lambda calculus"
type U  = UF of (U -> U) 
        | UP of U*U 
        | UI of int 
        | US of string 
        | UUnit 
        | UB of bool

//--- object language
type Exp = EId of string                          // (* identifier    *)
         | EI  of int                             // (* integer const *)
         | ES of string                           // (* string const  *)
         | EApp of Exp * Exp                      // (* application   *)
         | EPair of Exp * Exp                     // (* pairing       *)
         | ELet of string * Exp * Exp             // (* let binding   *)
         | EIf of Exp * Exp * Exp                 // (* conditional   *)
         | ELam of string * Exp                   // (* abstraction   *)
         | ELetfun of string * string * Exp * Exp // (* recursive fn  *)


let mkApp a b = EApp(a,b)
let mkId a = EId(a)
let mkPair a b = EPair(a,b)
let mkLam a b = ELam(a,b)