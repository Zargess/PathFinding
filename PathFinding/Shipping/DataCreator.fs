namespace Shipping

module DataCreator = 
    let createHarbors (path : string) : Harbor list = []
    let createContainers (path : string) (harbors : Harbor list) : Container list = []
    let createRoutes (path : string) : Route list = []