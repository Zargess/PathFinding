// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open PathFinding
open System

[<EntryPoint>]
let main argv = 
    let file = "C:\Users\Marcus\OneDrive\Dokumenter\\test.txt"
    let graph = Graphing.constructGraphFromFile file
    let path = Dijkstra.search graph (1,1) (39,5)
    printfn "%A" path
    Console.ReadLine() |> ignore

    0 // return an integer exit code
