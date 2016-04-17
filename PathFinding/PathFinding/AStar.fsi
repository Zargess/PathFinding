namespace PathFinding

module AStar =
    val search : Graph -> Position -> Position -> (Position -> Position -> float) -> (Position -> Position -> float) -> Position list