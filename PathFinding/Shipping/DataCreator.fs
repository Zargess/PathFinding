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

    let createHarborFromXmlNode (node : XmlNode) =
        let id = int node.["harborId"].InnerText
        let maxNumberOfShips = int node.["maxNumberOfShips"].InnerText
        let harborFee = float node.["harborFee"].InnerText
        { id = id; maxShips = maxNumberOfShips; fee = harborFee; pos = (id, id)}

    let createHarbors (doc : XmlDocument) : Harbor list =
        getNodes doc "/Game/harborList/Harbor"
        |> List.map createHarborFromXmlNode

    let getHarborPositionById (id : int) (harbors : Harbor list) =
        let harbor = List.find (fun (x : Harbor) -> x.id = id) harbors
        harbor.pos

    let createContainerFromXmlNode (harbors : Harbor list) (node : XmlNode) =
        let id = int node.["containerId"].InnerText
        let dest = int node.["toHarborId"].InnerText
        let pos = getHarborPositionById dest harbors
        let cargo = int node.["type"].InnerText
        { id = id; pos = pos; dest = dest; cargoType = cargo}

    let createContainers (doc : XmlDocument) (harbors : Harbor list) : Container list = 
        getNodes doc "/Game/harborList/Harbor/containerList/Container"
        |> List.map (createContainerFromXmlNode harbors)

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