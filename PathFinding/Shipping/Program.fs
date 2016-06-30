// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Shipping
open PathFinding
open System
open DataCreator

let random = new Random()

let getPositionThatIsNotHarbor (harbor : Harbor) (route : Route) : Position =
    if harbor.pos = (route.from, route.from)
    then (route.dest, route.dest)
    else (route.from, route.from)

let rec constructGraph (routes : Route list) (harbors : Harbor list) (vertices : Position list) (edges : Map<Position, Position list>) : Graph =
    match harbors with
    | [] -> { vertices = vertices; edges = edges }
    | harbor::cdr ->
        let children : Position list = 
            List.filter (fun x -> x.dest = harbor.id || x.from = harbor.id) routes
            |> List.map (getPositionThatIsNotHarbor harbor)
        let newEdges = edges.Add(harbor.pos, children)
        let newVertices = harbor.pos::vertices
        constructGraph routes cdr newVertices newEdges

let calculateFee (harbors : Harbor list) (pos : Position) =
    let harbor = List.find (fun (x : Harbor) -> x.pos = pos) harbors
    harbor.fee

let isHarborDone (harbor : Harbor) (containers : Container list) =
    containers
    |> List.exists (fun (c : Container) -> c.dest = harbor.id)
    |> not

let isRouteBetweenHarbors (from : Harbor) (dest : Harbor) (route : Route) : bool =
    (route.from = from.id && route.dest = dest.id) || (route.from = dest.id && route.dest = from.id)

let cost (harbors : Harbor list) (routes : Route list) (from : Position) (dest : Position) : float =
    let fromHarbor : Harbor = List.find (fun (h : Harbor) -> h.pos = from) harbors
    let destHarbor : Harbor = List.find (fun (h : Harbor) -> h.pos = dest) harbors
    let route = List.find (isRouteBetweenHarbors fromHarbor destHarbor) routes
    route.cost + destHarbor.fee

let heuristic (harbors : Harbor list) (containers : Container list) x y : float =
    List.fold (fun soFar (h : Harbor) -> if isHarborDone h containers then soFar else soFar + h.fee) 0.0 harbors

let findPathFromContainerToGoal (graph : Graph) heuristic cost (container : Container) =
    (container, AStar.search graph container.pos (container.dest, container.dest) heuristic cost)

[<EntryPoint>]
let main argv =
    printfn "%O" "Please enter the path to the game xml file:"
    let path = System.Console.ReadLine()
    let doc = createXmlDocument path

    let (harbors, containers) = createHarborsAndContainers doc
    let routes = createRoutes doc
    let ships = createShips doc

    // TODO : Remove and only use when a game has been created
    // TODO : Make a commandline walkthrough to create a game and descripe the game so far
    GameCreator.save "C:\Users\Marcus\desktop\\test.xml" 1 harbors containers ships routes |> ignore

    let graph = constructGraph routes harbors [] Map.empty
    let costFunction = cost harbors routes
    let heuristicFunction = heuristic harbors containers
    
    let containerPathPairs = List.map (fun c -> findPathFromContainerToGoal graph heuristicFunction costFunction c) containers

    printfn "%A" containerPathPairs

    printfn "%A" graph
    System.Console.ReadLine() |> ignore
    0 // return an integer exit code