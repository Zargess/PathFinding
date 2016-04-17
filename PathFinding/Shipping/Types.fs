namespace Shipping

type Harbor = { id : int; maxShips : int; fee : int }
type Container = { id : int; destination : int; cargoType : int; }
type Route = { harbors : (int * int ); cost : int; }