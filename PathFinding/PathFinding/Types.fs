namespace PathFinding

type Position = int * int
type Node = { pos : Position; prev : Node option; dist : float }
type Edge = Position * Position
type Graph = { vertices : Position list; edges : Edge list; nodes : Node list }