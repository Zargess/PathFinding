namespace PathFinding

module Dijkstra =
    let getPath (x : Node) =
        let rec helper (y : Node) (pathSoFar : Position list) : Position list =
            match y.prev with
            | Some(z) -> helper z (y.pos::pathSoFar)
            | None -> pathSoFar
        helper x []

    let getRemainingNeighbours (graph : Graph) (n : Node) (nodes : Node list) : Node list =
        let allNeighbours = List.filter (fun (x,y) -> x = n.pos) graph.edges
        List.filter (fun n -> List.exists (fun (x,y) -> y = n.pos) allNeighbours) nodes

    let updateNeighbour (node : Node) (neighbour : Node) : Node =
        let alt = node.dist + 1.0
        match neighbour with
        | x when alt < x.dist -> { x with prev = Some(node); dist = alt }
        | _ -> neighbour

    let rec findShortestPathToTarget (graph : Graph) (target : Position) (Q : Node list) : Position list =
        match Q with
        | [] -> failwith "No path found to target"
        | hd::tl ->
            match hd with
            | x when x.pos = target -> getPath x
            | _ ->
                let neighbours = getRemainingNeighbours graph hd tl
                let rest = List.filter (fun x -> not(List.contains x neighbours) && x <> hd) Q
                let Q' = (List.map (updateNeighbour hd) neighbours)@rest
                findShortestPathToTarget graph target Q'

    let shortestPathToTarget (graph : Graph) (source : Position) (target : Position) : Position list =
        let nodes = { pos= source; prev = None; dist = 0.0 }::(List.filter (fun (x : Node) -> x.pos <> source) graph.nodes)
        findShortestPathToTarget graph target nodes