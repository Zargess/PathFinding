namespace PathFinding

type Position = int * int
type Node = { pos : Position; prev : Node option; cost : float }
type Graph = { vertices : Position list; edges : Map<Position, Position list>; nodes : Node list }