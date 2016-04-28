// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Shipping
open PathFinding

// TODO : Consider if this is good enough as there might be cases where a harbor has |children|>8 which destroys this approach. Consider giving random positions and then use dijkstra instead.
(*let rec constructGraph (pos : Position) (harbors : Harbor list) (routes : Route list) (posMap : Map<int, Position>) (currentVisiting : Position list) (vertices : Position list) (edges : Map<Position, Position list>) =
    match harbors with
    | [] -> { nodes = List.map (fun x -> { pos = x; prev = None; cost = 0.0 }) vertices; edges = edges; vertices = vertices; }, posMap
    | harbor::cdr ->
        let neighbours : int list = [] //getNeighbours harbor routes []
        let children, newPosMap : Position list * Map<int, Position> = [],Map.empty//matchNeighboursWithAvailablePositions adjacent pos neighbours posMap
        let newEdges = edges.Add (pos, children)
        let newVertices = pos::vertices
        let notInVertices = List.filter (fun p -> not(List.contains p vertices)) children
        match notInVertices with
        | hd::tl -> constructGraph hd cdr routes newPosMap (currentVisiting@tl) newVertices newEdges
        | [] ->
            let currentVisitingNotInVertices = List.filter (fun p -> not(List.contains p newVertices)) currentVisiting
            match currentVisitingNotInVertices with
            | [] -> { nodes = List.map (fun x -> { pos = x; prev = None; cost = 0.0 }) newVertices; edges = newEdges; vertices = newVertices; }, posMap
            | head::tail -> constructGraph head cdr routes newPosMap (currentVisiting@tail) newVertices newEdges
*)

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
