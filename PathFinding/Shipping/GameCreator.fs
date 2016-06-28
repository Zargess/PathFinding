namespace Shipping

module GameCreator =
    open System.Xml

    let saveContainer (doc : XmlDocument) (container : Container) =
        let containerNode = doc.CreateElement("Container")

        let id = doc.CreateElement("containerId")
        id.InnerText <- string container.id
        containerNode.AppendChild(id) |> ignore

        let dest = doc.CreateElement("toHarborId")
        dest.InnerText <- string container.dest
        containerNode.AppendChild(dest) |> ignore

        let t = doc.CreateElement("type")
        t.InnerText <- string container.cargoType
        containerNode.AppendChild(t) |> ignore

        containerNode

    let rec saveContainers (doc : XmlDocument) (containers : Container list) (reslist : XmlElement list)  = 
        match containers with
        | [] -> reslist
        | hd::tl ->
            let node = saveContainer doc hd
            saveContainers doc tl (node::reslist)

    let saveHarbor (doc : XmlDocument) (harbor : Harbor) (containers : Container list) =
        let harborNode = doc.CreateElement("Harbor")

        let id = doc.CreateElement("harborId")
        id.InnerText <- string(harbor.id)
        harborNode.AppendChild(id) |> ignore
        
        let containerList = List.filter (fun (c : Container) -> c.pos = harbor.pos) containers
        let containerListNode = doc.CreateElement("containerList")
        let containerNodes = List.rev (saveContainers doc containerList [])

        for container in containerNodes do containerListNode.AppendChild(container) |> ignore

        harborNode.AppendChild(containerListNode) |> ignore

        let maxNumbersNode = doc.CreateElement("maxNumberOfShips")
        maxNumbersNode.InnerText <- string harbor.maxShips
        harborNode.AppendChild(maxNumbersNode) |> ignore

        let feeNode = doc.CreateElement("harborFee")
        feeNode.InnerText <- string harbor.fee
        harborNode.AppendChild(feeNode) |> ignore

        harborNode

    let rec saveHarbors (doc : XmlDocument) (harbors : Harbor list) (containers : Container list) (reslist : XmlElement list) =
        match harbors with
        | [] -> reslist
        | hd::tl ->
            let node = saveHarbor doc hd containers
            saveHarbors doc tl containers (node::reslist)

    let saveShip (doc : XmlDocument) (ship : Ship) =
        let shipNode = doc.CreateElement("Ship")

        let id = doc.CreateElement("shipId")
        id.InnerText <- string ship.id
        shipNode.AppendChild(id) |> ignore

        let max = doc.CreateElement("MaxContainerCapacity")
        max.InnerText <- string ship.capacity
        shipNode.AppendChild(max) |> ignore

        let (id,_) = ship.pos
        let pos = doc.CreateElement("currentHarborId")
        pos.InnerText <- string id
        shipNode.AppendChild(pos) |> ignore

        shipNode

    let rec saveShips (doc : XmlDocument) (ships : Ship list) (reslist : XmlElement list) =
        match ships with
        | [] -> reslist
        | hd::tl ->
            let node = saveShip doc hd
            saveShips doc tl (node::reslist)

    let saveRoute (doc : XmlDocument) (route : Route) =
        let routeNode = doc.CreateElement("Route")

        let from = doc.CreateElement("fromHarborId")
        from.InnerText <- string route.from
        routeNode.AppendChild(from) |> ignore

        let dest = doc.CreateElement("toHarborId")
        dest.InnerText <- string route.dest
        routeNode.AppendChild(dest) |> ignore
        
        let cost = doc.CreateElement("cost")
        cost.InnerText <- string route.cost
        routeNode.AppendChild(cost) |> ignore
        
        routeNode

    let rec saveRoutes (doc : XmlDocument) (routes : Route list) (reslist : XmlElement list) =
        match routes with
        | [] -> reslist
        | hd::tl ->
            let node = saveRoute doc hd
            saveRoutes doc tl (node::reslist)

    let save (path : string) (id : int) (harborlist : Harbor list) (containerlist : Container list) (shiplist : Ship list) (routelist : Route list) = 
        let doc = new XmlDocument()

        let gameNode = doc.CreateElement("Game")
        doc.AppendChild(gameNode) |> ignore

        let gameId = doc.CreateElement("gameId")
        gameId.InnerText <- string(id)
        gameNode.AppendChild(gameId) |> ignore

        let harborNodes = doc.CreateElement("harborList")
        gameNode.AppendChild(harborNodes) |> ignore

        let harborlistNodes = List.rev (saveHarbors doc harborlist containerlist [])
        for harbor in harborlistNodes do harborNodes.AppendChild(harbor) |> ignore

        let shiplistNode = doc.CreateElement("shipList")
        gameNode.AppendChild(shiplistNode) |> ignore
        let shiplistNodes = List.rev (saveShips doc shiplist [])
        for ship in shiplistNodes do shiplistNode.AppendChild(ship) |> ignore

        let routelistNode = doc.CreateElement("routeList")
        gameNode.AppendChild(routelistNode) |> ignore
        let routelistNodes = List.rev (saveRoutes doc routelist [])
        for route in routelistNodes do routelistNode.AppendChild(route) |> ignore


        doc.Save(path)
