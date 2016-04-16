namespace PathFinding

module Graphing =
    val constructGraphFromMap : string list -> (Node -> Node -> bool) -> Graph
    val constructGraphFromFile : string -> (Node -> Node -> bool) -> Graph
    val getPath : Node -> Position list
