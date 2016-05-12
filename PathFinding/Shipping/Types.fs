namespace Shipping

open PathFinding

type Harbor = { id : int; maxShips : int; fee : float; pos : Position; }
type Container = { id : int; pos : Position; dest : int; cargoType : int; }
type Route = { from : int; dest : int; cost : float; }
type Ship = { id : int; pos : Position; capacity : int }