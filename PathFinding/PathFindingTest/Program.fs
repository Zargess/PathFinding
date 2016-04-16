// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open PathFinding
open System



let adjacent (source : Node) (target : Node) =
    let x,y = source.pos
    let tx,ty = target.pos
    let north = x = tx && y = (ty + 1)
    let south = x = tx && y = (ty - 1)
    let east  = x = (tx - 1) && y = ty
    let west  = x = (tx + 1) && y = ty
    north || south || east || west

[<EntryPoint>]
let main argv = 
    let file = "C:\Users\Marcus\OneDrive\Dokumenter\\test.txt"
    let graph = Graphing.constructGraphFromFile file adjacent
    let path = Dijkstra.search graph (1,1) (39,5)
    printfn "%A" path
    Console.ReadLine() |> ignore

    0 // return an integer exit code
