// Learn more about F# at http://fsharp.org

open System

open Lib
open EP
open Types

let true0 = project bool (interpretclosed (EId "true"))
//-- 
let p = project int (interpretclosed (mkApp (mkId "*") (mkPair (EI 5)(EI 5))))
//-- p == 25


//let ierr   = project int (interpretclosed (mkApp (mkApp (mkId "iter") (mkId "suc")) (EI 5)))
let inoerr = project int (interpretclosed (mkApp (mkApp (mkApp (EId "iter") (EI 2)) (EId "suc")) (EI 5)))


let ep1 = 
    fun a b -> a ^-> b
let ep2 = 
    fun a b -> ((a ^-> b) ^-> a ^-> b) ^-> a ^-> b

//--Yf=fYf
let embY = mkLam "f" (
                     mkApp (mkLam "x" (mkApp (EId "f") (mkApp(EId "x")(EId "x"))))
                           (mkLam "x" (mkApp (EId "f") (mkApp(EId "x")(EId "x")))))

let embY' = mkLam "f" (
                 mkApp (EId "f") (
                     mkApp (mkLam "x" (mkApp (EId "f") (mkApp(EId "x")(EId "x"))))
                           (mkLam "x" (mkApp (EId "f") (mkApp(EId "x")(EId "x"))))))
                           
let embY'' =  mkLam "f" (
                 mkApp (EId "f") (
                     mkApp (EId "f") (mkApp(mkLam "x" (mkApp (EId "f") (mkApp(EId "x")(EId "x"))))
                                            (mkLam "x" (mkApp (EId "f") (mkApp(EId "x")(EId "x")))))))


//--Yfx=fYfx
let embY0 = mkLam "f" (mkApp
                   (mkLam "g"  (mkApp (EId "f") 
                                      (mkLam "a" (mkApp (mkApp (EId "g")
                                                                (EId "g"))
                                                        (EId "a")))))
                   (mkLam "g"  (mkApp (EId "f") 
                                      (mkLam "a" (mkApp (mkApp  (EId "g")
                                                                (EId "g"))
                                                        (EId "a")))))
            )

let embY0' = mkLam "f" 
                (mkApp (EId "f") (mkLam "a" 
                  (mkApp (mkApp (mkLam "g"  (mkApp (EId "f") (mkLam "a" (mkApp (mkApp (EId "g")(EId "g"))(EId "a")))))
                                  (mkLam "g"  (mkApp (EId "f") (mkLam "a" (mkApp (mkApp (EId "g")(EId "g"))(EId "a")))))) 
                                                               (EId "a"))))

let embY0'' = mkLam "f" 
                (mkApp (EId "f") (mkLam "a"  
                  (mkApp 
                    (mkApp (EId "f") (mkLam "a" 
                                        (mkApp 
                                             (mkApp (mkLam "g"  (mkApp (EId "f") 
                                                                        (mkLam "a" (mkApp (mkApp (EId "g")
                                                                                                 (EId "g"))
                                                                                          (EId "a")))))
                                                    (mkLam "g"  (mkApp (EId "f") 
                                                                       (mkLam "a" (mkApp (mkApp (EId "g")
                                                                                                (EId "g"))
                                                                                         (EId "a"))))))
                                             (EId "a"))))
                    (EId "a"))))

let polyYErr = 
     fun a b -> project (((a ^-> b) ^-> a ^-> b) ^-> (a ^-> b)) (interpretclosed embY)
// let factorialErr = //blows 
//     polyYErr int int (fun r n -> if n = 0 then 1 else n * r (n - 1) )
//let thisisErr = factorialErr 5

let polyY = 
    fun a b -> project (((a ^-> b) ^-> a ^-> b) ^-> (a ^-> b)) (interpretclosed embY0)
let factorial = 
    polyY int int (fun r n -> if n = 0 then 1 else n * r (n - 1) )
let thisis = factorial 5

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    Console.WriteLine("Factorial {0} from interpreted language!", thisis)
    0 // return an integer exit code
