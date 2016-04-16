namespace PathFinding

module AStar =
    open FibonacciHeap
    let rec findAStarPath (edges : Map<Position, Position list>) (start : Position) (goal : Position) (openSet : FibonacciHeap<Node>) (closedSet : Position list) (openEntryMap : Map<Position, FibonacciHeapNode<Node>>) : Position list =
        if openSet.IsEmpty()
        then []
        else
            let min = openSet.RemoveMin().Data
            match min with
            | x when x.pos = goal -> Graphing.getPath min
            | _ ->
                let newClosedSet = min.pos::closedSet
                let neighbours = List.filter (fun x -> not(List.contains x closedSet)) edges.[min.pos]
                []


    let search x = []

