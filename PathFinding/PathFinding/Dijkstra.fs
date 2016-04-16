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

    let rec updateNeighbours (n : Node) (neighbours : Edge list) (heap : FibonacciHeap<Node>) (fibNodes : Map<Position, FibonacciHeapNode<Node>>) (heuristic : Position -> Position -> float) : FibonacciHeap<Node> =
        match neighbours with
        | [] -> heap
        | (x, y)::cdr ->
            let node = fibNodes.[n.pos]
            let neighbour = fibNodes.[y]
            let alt = node.Key + (heuristic n.pos y)
            if alt < neighbour.Key
            then
                neighbour.Data <- {neighbour.Data with prev = Some(n)} 
                heap.DecreaseKey(neighbour, alt)
                updateNeighbours n cdr heap fibNodes heuristic
            else
                updateNeighbours n cdr heap fibNodes heuristic

    let rec searchForPath (edges : Edge list) (target : Position) (Q : FibonacciHeap<Node>) (fibNodes : Map<Position, FibonacciHeapNode<Node>>) (visitedNodes : Node list) (heuristic : Position -> Position -> float) : Position list = 
        if Q.IsEmpty()
        then getPath (List.find (fun x -> x.pos = target) visitedNodes)
        else
            let min = Q.RemoveMin().Data
            match min with
            | x when min.pos = target -> getPath min
            | _ ->
                let neighbours =
                    List.filter (fun (x,y) -> x = min.pos && not(List.exists (fun (n : Node) -> n.pos = x) visitedNodes)) edges
                let heap = updateNeighbours min neighbours Q fibNodes heuristic
                searchForPath edges target heap fibNodes (min::visitedNodes) heuristic


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

    let dijkstraHeuristic (x : Position) (y : Position) = 1.0

    let search (graph : Graph) (source : Position) (target : Position) : Position list =
        let heap, map = constructHeap source graph.nodes (new FibonacciHeap<Node>()) Map.empty
        searchForPath graph.edges target heap map [] dijkstraHeuristic