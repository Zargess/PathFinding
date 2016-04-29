// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Shipping
open PathFinding
open System

let random = new Random()

let getPositionThatIsNotHarbor (harbor : Harbor) (route : Route) : Position =
    if harbor.pos = (route.from, route.from)
    then (route.dest, route.dest)
    else (route.from, route.from)

let rec constructGraph (calcFee : Position -> float) (routes : Route list) (harbors : Harbor list) (vertices : Position list) (edges : Map<Position, Position list>) : Graph =
    match harbors with
    | [] -> { nodes = List.map (fun x -> { pos = x; prev = None; cost = calcFee x }) vertices; vertices = vertices; edges = edges }
    | harbor::cdr ->
        let children : Position list = 
            List.filter (fun x -> x.dest = harbor.id || x.from = harbor.id) routes
            |> List.map (getPositionThatIsNotHarbor harbor)
        let newEdges = edges.Add(harbor.pos, children)
        let newVertices = harbor.pos::vertices
        constructGraph calcFee routes cdr newVertices newEdges

let calculateFee (harbors : Harbor list) (pos : Position) =
    let harbor = List.find (fun (x : Harbor) -> x.pos = pos) harbors
    harbor.fee

let isHarborDone (harbor : Harbor) (containers : Container list) =
    containers
    |> List.exists (fun (c : Container) -> c.dest = harbor.id)
    |> not

let isRouteBetweenHarbors (from : Harbor) (dest : Harbor) (route : Route) : bool =
    (route.from = from.id && route.dest = dest.id) || (route.from = dest.id && route.dest = from.id)

let cost (harbors : Harbor list) (routes : Route list) (from : Position) (dest : Position) : float =
    let fromHarbor : Harbor = List.find (fun (h : Harbor) -> h.pos = from) harbors
    let destHarbor : Harbor = List.find (fun (h : Harbor) -> h.pos = dest) harbors
    let route = List.find (isRouteBetweenHarbors fromHarbor destHarbor) routes
    route.cost + destHarbor.fee

let heuristic (harbors : Harbor list) (containers : Container list) x y : float =
    List.fold (fun soFar (h : Harbor) -> if isHarborDone h containers then soFar else soFar + h.fee) 0.0 harbors

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
