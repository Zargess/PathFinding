namespace PathFinding

module AStar =
    open FibonacciHeap

    let rec createChildNodes (node : Node) (neighbours : Position list) (goal : Position) (openSet : FibonacciHeap<Node>) (openEntryMap : Map<Position, FibonacciHeapNode<Node>>) (heuristic : Position -> Position -> float) (cost : Position -> Position -> float) =
        match neighbours with
        | [] -> (openSet, openEntryMap)
        | car::cdr ->
            let child = { pos=car; prev=Some(node); cost=node.cost + (cost node.pos car) }
            let childNode = new FibonacciHeapNode<Node>(child, child.cost + (heuristic car goal))
            let newOpenEntryMap = openEntryMap.Add(car, childNode)
            openSet.Insert(childNode)
            createChildNodes node cdr goal openSet newOpenEntryMap heuristic cost


    let rec findAStarPath (edges : Map<Position, Position list>) (goal : Position) (openSet : FibonacciHeap<Node>) (closedSet : Position list) (openEntryMap : Map<Position, FibonacciHeapNode<Node>>) (heuristic : Position -> Position -> float) (cost : Position -> Position -> float) : Position list =
        if openSet.IsEmpty()
        then []
        else
            match openSet.RemoveMin().Data with
            | min when min.pos = goal -> Graphing.getPath min
            | min ->
                let newClosedSet = min.pos::closedSet
                let neighbours = List.filter (fun x -> not(List.contains x closedSet) && not(openEntryMap.ContainsKey(x))) edges.[min.pos]
                let (newOpenSet, newOpenEntryMap) = createChildNodes min neighbours goal openSet openEntryMap heuristic cost
                findAStarPath edges goal newOpenSet newClosedSet newOpenEntryMap heuristic cost


    let search (graph : Graph) (start : Position) (goal : Position) (heuristic : Position -> Position -> float) (cost : Position -> Position -> float) : Position list =
        let key = heuristic start goal
        let startNode = new FibonacciHeapNode<Node>({ pos=start; prev=None; cost=0.0; }, key)
        let openSet = new FibonacciHeap<Node>()
        openSet.Insert(startNode)
        let openEntryMap = Map.empty.Add(start, startNode)
        findAStarPath graph.edges goal openSet [] openEntryMap heuristic cost