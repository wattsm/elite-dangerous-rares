module Program

open System

///Runs the DBSCAN algorithm using the given maximum distance and minimum cluster size
let getCommodityClusters maxDistance minSize = 
    async {

        let! systems = IO.getSystems ()
        let! manifests = IO.getManifests systems
    
        let settings : DBSCAN.Settings = 
            { 
                Manifests = manifests;
                MaxDistance = maxDistance;
                MinSize = minSize;
            }

        let (clusters, _) = DBSCAN.apply settings

        return clusters
    }

///Prompts the user for basic settings and then generates report
//based on the results of the DBSCAN algorithm
[<EntryPoint>]
let rec main _ = 

    Console.Clear ()

    Console.Write ("Maximum distance between systems (LY): ")
    let distance = Convert.ToDecimal (Console.ReadLine ())

    Console.Write ("Minimum cluster size: ")
    let size = Convert.ToInt32 (Console.ReadLine ())

    Console.Clear ()

    let clusters = 
        getCommodityClusters distance size
        |> Async.RunSynchronously 

    Report.writeToConsole distance size clusters

    Console.Write ("Save? (Y/N): ")
    let save = Console.ReadLine ()

    if (save === "Y") then
        Report.writeToFile distance size clusters
        
    Console.Write ("Again? (Y/N): ")
    let again = Console.ReadLine ()

    if not (again === "Y") then
        0
    else
        main [||]
