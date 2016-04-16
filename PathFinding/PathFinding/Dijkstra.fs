namespace PathFinding

module Dijkstra =
    open FibonacciHeap

    let rec updateNeighbours (n : Node) (neighbours : Position list) (heap : FibonacciHeap<Node>) (fibNodes : Map<Position, FibonacciHeapNode<Node>>) (heuristic : Position -> Position -> float) : FibonacciHeap<Node> =
        match neighbours with
        | [] -> heap
        | car::cdr ->
            let node = fibNodes.[n.pos]
            let neighbour = fibNodes.[car]
            let alt = node.Key + (heuristic n.pos car)
            if alt < neighbour.Key
            then
                neighbour.Data <- {neighbour.Data with prev = Some(n)} 
                heap.DecreaseKey(neighbour, alt)
                updateNeighbours n cdr heap fibNodes heuristic
            else
                updateNeighbours n cdr heap fibNodes heuristic

    let rec searchForPath (edges : Map<Position, Position list>) (target : Position) (Q : FibonacciHeap<Node>) (fibNodes : Map<Position, FibonacciHeapNode<Node>>) (visitedNodes : Position list) (heuristic : Position -> Position -> float) : Position list = 
        if Q.IsEmpty()
        then []
        else
            let min = Q.RemoveMin().Data
            match min with
            | x when min.pos = target -> Graphing.getPath min
            | _ ->
                let neighbours = List.filter (fun x -> not(List.contains x visitedNodes)) edges.[min.pos]
                let heap = updateNeighbours min neighbours Q fibNodes heuristic
                searchForPath edges target heap fibNodes (min.pos::visitedNodes) heuristic

    let dijkstraHeuristic (x : Position) (y : Position) = 1.0

    let search (graph : Graph) (source : Position) (target : Position) (constructHeap : Position -> Node list -> FibonacciHeap<Node> -> Map<Position, FibonacciHeapNode<Node>> -> FibonacciHeap<Node> * Map<Position, FibonacciHeapNode<Node>>) : Position list =
        let heap, map = constructHeap source graph.nodes (new FibonacciHeap<Node>()) Map.empty
        searchForPath graph.edges target heap map [] dijkstraHeuristic