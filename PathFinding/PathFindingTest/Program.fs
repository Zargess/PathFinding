// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open PathFinding
open System
open FibonacciHeap



let adjacent (source : Node) (target : Node) =
    let x,y = source.pos
    let tx,ty = target.pos
    let north = x = tx && y = (ty + 1)
    let south = x = tx && y = (ty - 1)
    let east  = x = (tx - 1) && y = ty
    let west  = x = (tx + 1) && y = ty
    north || south || east || west




let rec constructHeap (source : Position) (nodes : Node list) (heap : FibonacciHeap<Node>) (entryMap : Map<Position, FibonacciHeapNode<Node>>) : (FibonacciHeap<Node> * Map<Position, FibonacciHeapNode<Node>>) =
    match nodes with 
    | car::cdr when car.pos = source -> 
        let node = new FibonacciHeapNode<Node>(car, 0.0)
        let map = entryMap.Add(car.pos, node)
        heap.Insert(node, 0.0)
        constructHeap source cdr heap map 
    | car::cdr ->
        let node = new FibonacciHeapNode<Node>(car, infinity)
        let map = entryMap.Add(car.pos, node)
        heap.Insert(node, infinity)
        constructHeap source cdr heap map 
    | [] -> (heap, entryMap)

let cost (x : Position) (y : Position) = 1.0

let manhattan (x1, y1) (x2, y2) = abs(x1 - x2) + abs(y1 - y2) |> float

[<EntryPoint>]
let main argv = 
    let file = "C:\Users\Marcus\OneDrive\Dokumenter\\test.txt"
    let graph = Graphing.constructGraphFromFile file adjacent
    let path = Dijkstra.search graph (1,1) (39,5) constructHeap
    let path' = AStar.search graph (1,1) (39,5) manhattan cost
    printfn "%A" path
    printfn "%A" path'
    printfn "%A" (path = path')
    Console.ReadLine() |> ignore

    0 // return an integer exit code
