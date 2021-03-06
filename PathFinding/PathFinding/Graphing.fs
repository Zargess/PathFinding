﻿namespace PathFinding

module Graphing =
    open System.IO

    let getPath (x : Node) =
        let rec helper (y : Node) (pathSoFar : Position list) : Position list =
            match y.prev with
            | Some(z) -> helper z (y.pos::pathSoFar)
            | None -> y.pos::pathSoFar
        helper x []
    
    let readAllLines (path : string) = File.ReadAllLines(path) |> List.ofArray    
    
    let rec lineToNodes x y list res =
        match list with
        | [] -> res
        | hd::tl ->
            let newRes =
                match hd with
                | ' ' -> { pos = (x,y); prev = None; cost=0.0; }::res
                | _ -> res
            lineToNodes (x+1) y tl newRes

    let adjacentNodes (adjacent : Node -> Node -> bool) (source : Node) (nodes : Node list) =
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
                         { pos = x.pos; prev = None; cost=0.0; })

    let constructNodesFromFile (path : string) : Node list =
        readAllLines path
        |> constructNodesFromMap

    let rec constructGraph (adjacent : Node -> Node -> bool) (node : Node) (nodes : Node list) (currentVisiting : Node list) (vertices : Position list) (edges : Map<Position, Position list>) : Graph =
        let pos = node.pos
        let children = adjacentNodes adjacent node nodes
        let newEdges = edges.Add (pos, (List.map (fun (x : Node) -> x.pos) children))
        let newVertices = pos::vertices
        let notInVertices = List.filter (fun (p : Node) -> not(List.contains p.pos vertices)) children
        match notInVertices with
        | hd::tl -> constructGraph adjacent hd nodes (currentVisiting@tl) newVertices newEdges
        | [] ->
            let currentVisitingNotInVertices = List.filter (fun (p : Node) -> not(List.contains p.pos newVertices)) currentVisiting
            match currentVisitingNotInVertices with
            | [] -> { vertices = newVertices; edges = newEdges }
            | head::tail -> constructGraph adjacent head nodes (currentVisiting@tail) newVertices newEdges

    let constructGraphFromMap (map : string list) (adjacent : Node -> Node -> bool) : Graph =
        let nodes = constructNodesFromMap map
        let initialNode = List.head nodes
        constructGraph adjacent initialNode nodes [] [] Map.empty

    let constructGraphFromFile (path : string) (adjacent : Node -> Node -> bool) : Graph =
        let nodes = constructNodesFromFile path
        let initialNode = List.head nodes
        constructGraph adjacent initialNode nodes [] [] Map.empty