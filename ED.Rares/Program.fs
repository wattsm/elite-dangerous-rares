module Program

open System
open DBSCAN

///Contains functions for reporting the results of the DBSCAN algorithm
[<RequireQualifiedAccess>]
module Report = 

    open Microsoft.FSharp.Core.Printf

    ///Writes a report detailing the clusters and potential trading partners
    let private writeTo writer maxDistance minSize clusters =

        fprintfn writer "Configuration"
        fprintfn writer "  Maximum distance: %M LY" maxDistance
        fprintfn writer "  Minimum size: %u" minSize

        fprintfn writer ""

        if (List.isEmpty clusters) then
            fprintfn writer "(No commodity clusters found.)"
        else

            fprintfn writer "Commodity clusters (%u)" (List.length clusters)
            fprintfn writer ""

            for (clusterNo, manifests) in clusters do

            fprintfn writer  "  Cluster %u (%u systems)" clusterNo (List.length manifests)
            
            for manifest in manifests do

                fprintfn writer  "    System: %s" manifest.System.Name

                for (station, commodity) in manifest.Commodities do

                    let allowance = 
                        match commodity.Allowance with
                        | Some value -> sprintf "%u T" value
                        | _ -> "?"

                    let supply = 
                        match commodity.Supply with
                        | (None, None) -> "?"
                        | (Some min, Some max) -> sprintf "%u - %u T" min max
                        | (Some min, _) -> sprintf "%uT minimum" min
                        | (_, Some max) -> sprintf "%uT maximum" max

                    let stationDetails = 
                        match station.Orbit with
                        | Some value ->  sprintf "%s (%M ls)" station.Name value
                        | _ -> station.Name

                    fprintfn writer  "      Station: %s" stationDetails
                    fprintfn writer  "      Item: %s" commodity.Name
                    fprintfn writer  "      Price: %u cr" commodity.Price
                    fprintfn writer  "      Allowance: %s" allowance
                    fprintfn writer  "      Supply: %s" supply
                    fprintfn writer  ""

            let potentialPartners = 
                clusters
                |> List.filter (fst >> ((=) clusterNo) >> not)
                |> List.map (fun (clusterNo', manifests') ->
                        let distance = Distance.betweenClusters manifests manifests'
                        in (clusterNo', distance)
                    )
                |> List.filter (snd >> ((<) 100M))

            if (List.isEmpty potentialPartners) then
                fprintfn writer  "    (No other clusters more than 100LY away.)"
            else
                fprintfn writer  "    Potential partner clusters:"

                for (clusterNo', distance) in potentialPartners do
                    fprintfn writer  "      Cluster %u @ %M LY" clusterNo' (Math.Round (distance, 2))

            fprintfn writer  ""

        writer.Flush ()

    ///Writes the report to the console
    let writeToConsole = writeTo Console.Out

    ///Writes the report to Report.txt
    let writeToFile maxDistance minSize clusters = 

        use writer = IO.getReportWriter ()
    
        writeTo writer maxDistance minSize clusters

        writer.Close ()

///Runs the DBSCAN algorithm using the given maximum distance and minimum cluster size
let getCommodityClusters maxDistance minSize = 
    async {

        let! systems = IO.getSystems ()
        let! manifests = IO.getManifests systems
    
        let settings = 
            {
                Manifests = manifests;
                MaxDistance = maxDistance;
                MinSize = minSize;
            }

        let (clusters, _) = apply settings

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
