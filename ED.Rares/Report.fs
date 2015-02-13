///Contains functions for reporting the results of the DBSCAN algorithm
[<RequireQualifiedAccess>]
module Report

open System
open Microsoft.FSharp.Core.Printf

///Writes a report detailing the clusters and potential trading partners
let private writeTo writer maxDistance minSize clusters =

    fprintfn writer "Configuration"
    fprintfn writer "  Maximum distance: %M LY" maxDistance
    fprintfn writer "  Minimum size: %u" minSize
    fprintfn writer ""

    let clusterCount = List.length clusters
    let systemCount = List.sumBy (snd >> List.length) clusters

    let avgSystemsPerCluster = 
        if (systemCount = 0) then
            0M
        else
            let value = (decimal systemCount) / (decimal clusterCount)
            in Math.Round (value, 2)

    fprintfn writer "Summary"
    fprintfn writer "  Clusters: %u" (List.length clusters)
    fprintfn writer "  Systems: %u" (List.sumBy (snd >> List.length) clusters)
    fprintfn writer "  Avg. Systems / Cluster: %M" avgSystemsPerCluster
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
                    | Some value ->  sprintf "%s (~%M ls)" station.Name value
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