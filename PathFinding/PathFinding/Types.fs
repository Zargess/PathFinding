namespace PathFinding

type Position = int * int
type Node = { pos : Position; prev : Node option; cost : float }
// TODO : Don't have nodes in graph, let functions create them when needed instead
type Graph = { vertices : Position list; edges : Map<Position, Position list>; nodes : Node list }