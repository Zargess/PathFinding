namespace PathFinding

module Dijkstra =
    open FibonacciHeap

    let getPath (x : Node) =
        let rec helper (y : Node) (pathSoFar : Position list) : Position list =
            match y.prev with
            | Some(z) -> helper z (y.pos::pathSoFar)
            | None -> pathSoFar
        helper x []

    let getRemainingNeighbours (edges : Edge list) (n : Node) (nodes : Node list) : Node list =
        let allNeighbours = List.filter (fun (x,y) -> x = n.pos) edges
        List.filter (fun n -> List.exists (fun (x,y) -> y = n.pos) allNeighbours) nodes

    //let updateNeighbour (node : Node) (neighbour : Node) (heuristic : Position -> Position -> float)

//    let updateNeighbour (node : Node) (neighbour : Node) : Node =
//        let alt = node.dist + 1.0
//        match neighbour with
//        | x when alt < x.dist -> { x with prev = Some(node); dist = alt }
//        | _ -> neighbour
//
//    let rec findShortestPathToTarget (graph : Graph) (target : Position) (Q : Node list) : Position list =
//        match Q with
//        | [] -> failwith "No path found to target"
//        | hd::tl ->
//            match hd with
//            | x when x.pos = target -> getPath x
//            | _ ->
//                let neighbours = getRemainingNeighbours graph hd tl
//                let rest = List.filter (fun x -> not(List.contains x neighbours) && x <> hd) Q
//                let Q' = (List.map (updateNeighbour hd) neighbours)@rest
//                findShortestPathToTarget graph target Q'

    let rec searchForPath (edges : Edge list) (target : Position) (Q : FibonacciHeap<Node>) (fibNodes : Map<Position, FibonacciHeapNode<Node>>) (visitedNodes : Node list) : Position list = 
        if Q.IsEmpty()
        then getPath (List.find (fun x -> x.pos = target) visitedNodes)
        else
            let min = Q.RemoveMin().Data
            match min with
            | x when min.pos = target -> getPath min
            | _ ->
                let neighbours =
                    List.filter (fun (x,y) -> x = min.pos && not(List.exists (fun (n : Node) -> n.pos = x) visitedNodes)) edges
                []


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

    let search (graph : Graph) (source : Position) (target : Position) : Position list =
//        let nodes = { pos= source; prev = None; dist = 0.0 }::(List.filter (fun (x : Node) -> x.pos <> source) graph.nodes)
//        findShortestPathToTarget graph target nodes
        let heap, map = constructHeap source graph.nodes (new FibonacciHeap<Node>()) Map.empty
//        let n = new FibonacciHeapNode<Node>({pos = source; prev = None; dist = 0.0}, 0.0);
//        n.Data <- {n.Data with dist = 1.0}
        []