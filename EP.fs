module EP

open Types


type RecordTest = { X: int; Y: int }

let record1 = { X = 1; Y = 2 }
let record2 = { X = 1; Y = 2 }

type 'a EP = { Embed : 'a -> U; 
               Project : U -> 'a } //no camelCase

let MkEP e p = {Embed = e; Project = p}
let embed e = e.Embed
let project e = e.Project


let unit = MkEP (fun () -> UUnit) (fun _ -> ())

let bool = MkEP UB (fun (UB b) -> b)
let int = MkEP UI (fun (UI i) -> i)
let str = MkEP US (fun (US s) -> s)
let (--) ea eb = MkEP (fun (a,b) -> UP (embed ea a, embed eb b)) 
                      (fun (UP (ua, ub)) -> (project ea ua, project eb ub) )
let (^->) ea eb = 
    MkEP (fun f -> UF (embed eb << f << project ea)) 
         (fun (UF uf) -> project eb << uf << embed ea)


