namespace PathFinding

module Graphing =
    open System.IO
    
    let readAllLines (path : string) = File.ReadAllLines(path) |> List.ofArray    
    
    let rec lineToNodes x y list res =
        match list with
        | [] -> res
        | hd::tl ->
            let newRes =
                match hd with
                | ' ' -> { pos = (x,y); prev = None; dist = infinity }::res
                | _ -> res
            lineToNodes (x+1) y tl newRes

    let adjacent (source : Node) (target : Node) =
        let x,y = source.pos
        let tx,ty = target.pos
        let north = x = tx && y = (ty + 1)
        let south = x = tx && y = (ty - 1)
        let east  = x = (tx - 1) && y = ty
        let west  = x = (tx + 1) && y = ty
        north || south || east || west

    let adjacentNodes (source : Node) (nodes : Node list) =
        nodes
        |> List.filter (fun x -> source <> x)
        |> List.filter (adjacent source)

    let constructNodesFromMap (map : string list) : Node list =
        let tempNodes = map
                        |> List.map (fun (x : string) -> x.ToCharArray() |> List.ofArray)
                        |> List.mapi (fun i x -> lineToNodes 0 i x [])
                        |> List.fold (fun state x -> state@x) []
        tempNodes
        |> List.map (fun x -> 
                         { pos = x.pos; prev = None; dist = infinity })

    let constructNodesFromFile (path : string) : Node list =
        readAllLines path
        |> constructNodesFromMap

    let rec constructGraph (node : Node) (nodes : Node list) (currentVisiting : Node list) (vertices : Position list) (edges : Edge list) : Graph =
        let pos = node.pos
        let children = adjacentNodes node nodes
        let newEdges = (List.map (fun (x : Node) -> (pos, x.pos)) children)@edges
        let newVertices = pos::vertices
        let notInVertices = List.filter (fun (p : Node) -> not(List.contains p.pos vertices)) children
        match notInVertices with
        | hd::tl -> constructGraph hd nodes (currentVisiting@tl) newVertices newEdges
        | [] ->
            let currentVisitingNotInVertices = List.filter (fun (p : Node) -> not(List.contains p.pos newVertices)) currentVisiting
            match currentVisitingNotInVertices with
            | [] -> { vertices = newVertices; edges = newEdges; nodes = nodes }
            | head::tail -> constructGraph head nodes (currentVisiting@tail) newVertices newEdges

    let constructGraphFromMap (map : string list) : Graph =
        let nodes = constructNodesFromMap map
        let initialNode = List.head nodes
        constructGraph initialNode nodes [] [] []

    let constructGraphFromFile (path : string) : Graph =
        let nodes = constructNodesFromFile path
        let initialNode = List.head nodes
        constructGraph initialNode nodes [] [] []
        