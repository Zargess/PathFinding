// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Shipping
open PathFinding

// TODO : Consider if this is good enough as there might be cases where a harbor has |children|>8 which destroys this approach. Consider giving random positions and then use dijkstra instead.
let rec constructGraph (pos : Position) (harbors : Harbor list) (routes : Route list) (posMap : Map<int, Position>) (currentVisiting : Position list) (vertices : Position list) (edges : Map<Position, Position list>) =
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



[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
