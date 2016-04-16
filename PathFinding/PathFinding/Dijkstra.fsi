namespace PathFinding

module Dijkstra = 
    open FibonacciHeap
    val search : Graph -> Position -> Position -> (Position -> Node list -> FibonacciHeap<Node> -> Map<Position, FibonacciHeapNode<Node>> -> FibonacciHeap<Node> * Map<Position, FibonacciHeapNode<Node>>) -> Position list
