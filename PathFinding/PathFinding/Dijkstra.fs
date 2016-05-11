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

    let rec searchForPath (edges : Map<Position, Position list>) (target : Position) (Q : FibonacciHeap<Node>) (fibNodes : Map<Position, FibonacciHeapNode<Node>>) (visitedNodes : Position list) (heuristic : Position -> Position -> float) : Position list = 
        if Q.IsEmpty()
        then []
        else
            match Q.RemoveMin().Data with
            | min when min.pos = target -> Graphing.getPath min
            | min ->
                let neighbours = List.filter (fun x -> not(List.contains x visitedNodes)) edges.[min.pos]
                let heap = updateNeighbours min neighbours Q fibNodes heuristic
                searchForPath edges target heap fibNodes (min.pos::visitedNodes) heuristic

    // TODO : Get function, that takes two positions and returns a float instead of this hard coding
    let dijkstraHeuristic (x : Position) (y : Position) = 1.0

    let search (graph : Graph) (source : Position) (target : Position) (constructHeap : Position -> Node list -> FibonacciHeap<Node> -> Map<Position, FibonacciHeapNode<Node>> -> FibonacciHeap<Node> * Map<Position, FibonacciHeapNode<Node>>) : Position list =
        let heap, map = constructHeap source graph.nodes (new FibonacciHeap<Node>()) Map.empty
        searchForPath graph.edges target heap map [] dijkstraHeuristic