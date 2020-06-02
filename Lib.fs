module Lib

open Types
open EP

let uncurry f (a,b) = f a b
let fst (a,_) = a
let snd (_,b) = b

//-- Dans le meta language
let rec iter n f x = 
    if n = 0 then x else f (iter (n - 1) f x)

let times a b = a * b

let builtins = 
        [ ("true", embed bool true);
          ("*"   , embed ((int -- int) ^->int) (uncurry times)) ;
          // -- iter accessible au language object
          ("iter", embed (int ^-> (int ^-> int) ^-> int ^-> int) iter) ;
          ("suc", embed (int ^-> int) (fun n -> n + 1)) 
        ]


let lookupb s = snd (List.find ((=) s << fst) builtins) 


let a = defaultArg
let u = failwith ""
let fpair (f,g) x = (f x, g x) 


type Staticenv = string list
type Dynamicenv = U list


let indexof names x =
 List.tryFindIndex ((=) x) names

// evaluateur du language objet vers U
let rec interpret (e,staticEnv) = match e with 
    | EId s ->  let mfind = indexof staticEnv s  //
                              |> Option.bind (fun n -> Some (fun dynamic -> List.item n dynamic))
                  in defaultArg mfind (fun _ -> lookupb s)
    | EI n -> (fun _ -> UI n)
    | ES s -> (fun _ -> US s)
    | EApp(e1,e2)  -> let s1 = interpret (e1,staticEnv)
                      let s2 = interpret (e2,staticEnv)
                      (fun dynamic -> let (UF f) = s1 dynamic
                                      let a = s2 dynamic
                                      f a)
    //| EPair(e1,e2) -> fun dynamic -> UP(interpret (e1, sEnv) dynamic, interpret (e2, sEnv) dynamic )
    | EPair(e1,e2) -> UP << fpair (interpret (e1, staticEnv), interpret (e2, staticEnv))
    | ELet(x, b, n) ->  let s1 = interpret(b, staticEnv)
                        let s2 = interpret(n, x :: staticEnv)
                        fun dynamic -> let v = s1 dynamic
                                       in s2 (v::dynamic)
    | EIf(eb,e1,e2 )  -> let sb = interpret (eb, staticEnv)
                         let s1 = interpret (e1, staticEnv)
                         let s2 = interpret (e2, staticEnv)
                         fun dynamic -> let (UB b) = sb dynamic
                                        if b then s1 dynamic else s2 dynamic
    | ELam(x, ebody) -> let sebody = interpret (ebody, x::staticEnv)
                        fun dynamic -> UF (fun v -> sebody (v::dynamic)) 
                        // la valeur de x sera en 1e position sur la pile
                        //la fonction construite depend elle meme d'autres variables
    | ELetfun(f,x,e1,e2)-> //-- letrec f x = e1 in e2
                           let s1 = interpret (e1, x::f::staticEnv)
                           let s2 = interpret (e2,f::staticEnv)
                           fun dynamic -> let rec g v = s1 (v::(UF g)::dynamic) //-- g a acces a g
                                          s2 ((UF g)::dynamic)              //-- la suite a acces a g

let interpretclosed e = 
  interpret (e,[]) []