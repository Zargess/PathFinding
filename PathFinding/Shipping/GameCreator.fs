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

    // TODO : only need to save routes and ships now
    let save (id : int) (harborlist : Harbor list) (containerlist : Container list) (shiplist : Ship list) (routelist : Route list) = 
        let doc = new XmlDocument()
        let gameNode = doc.CreateElement("Game")
        doc.AppendChild(gameNode) |> ignore
        let gameId = doc.CreateElement("gameId")
        gameId.InnerText <- string(id)
        gameNode.AppendChild(gameId) |> ignore
        let harborNodes = doc.CreateElement("harborList")
        gameNode.AppendChild(harborNodes) |> ignore

        let harborlistNodes = saveHarbors doc harborlist containerlist []
        for harbor in harborlistNodes do harborNodes.AppendChild(harbor) |> ignore
        []
