namespace Shipping

module DataCreator = 
    open System.Xml
    
    let createXmlDocument (path : string) =
        let doc = new XmlDocument()
        doc.Load(path)
        doc

    let getNodes (doc : XmlDocument) (path : string) =
        doc.SelectNodes path
        |> Seq.cast<XmlNode>
        |> List.ofSeq

    let getNodesFromNode (node : XmlNode) (path : string) =
        node.SelectNodes path
        |> Seq.cast<XmlNode>
        |> List.ofSeq

    let createContainerFromXmlNode (harbor : Harbor) (node : XmlNode) =
        let id = int node.["containerId"].InnerText
        let dest = int node.["toHarborId"].InnerText
        let pos = harbor.pos
        let cargo = int node.["type"].InnerText
        { id = id; pos = pos; dest = dest; cargoType = cargo}

    // TODO : Create containers in this harbor!
    let createHarborAndContainersFromXmlNode (node : XmlNode) : Harbor * Container list =
        let id = int node.["harborId"].InnerText
        let maxNumberOfShips = int node.["maxNumberOfShips"].InnerText
        let harborFee = float node.["harborFee"].InnerText
        let containerNodes = getNodesFromNode node "descendant::Container"
        let harbor = { id = id; maxShips = maxNumberOfShips; fee = harborFee; pos = (id, id)}
        let containers = List.map (createContainerFromXmlNode harbor) containerNodes
        (harbor, containers)

    let createHarborsAndContainers (doc : XmlDocument) : Harbor list * Container list =
        let (harbors, containerLists) = 
            getNodes doc "/Game/harborList/Harbor"
            |> List.map createHarborAndContainersFromXmlNode
            |> List.unzip
        let containers = List.fold (fun res c -> c@res) [] containerLists
        (harbors, containers)

    let createRouteFromXmlNode (node : XmlNode) =
        let from = int node.["fromHarborId"].InnerText
        let dest = int node.["toHarborId"].InnerText
        let cost = float node.["cost"].InnerText
        { from = from; dest = dest; cost = cost }

    let createRoutes (doc : XmlDocument) : Route list = 
        getNodes doc "/Game/routeList/Route"
        |> List.map createRouteFromXmlNode

    let createShipFromXmlNode (node : XmlNode) = 
        let id = int node.["shipId"].InnerText
        let harborid = int node.["currentHarborId"].InnerText
        let pos = (harborid, harborid)
        let capacity = int node.["MaxContainerCapacity"].InnerText
        { id = id; pos = pos; capacity = capacity }

    let createShips (doc : XmlDocument) : Ship list =
        getNodes doc "/Game/shipList/Ship"
        |> List.map createShipFromXmlNode