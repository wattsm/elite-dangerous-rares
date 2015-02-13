[<RequireQualifiedAccess>]
module IO

open System
open System.IO

let [<Literal>] private systemsFileName = "Systems.csv"
let [<Literal>] private goodsFileName = "Goods.csv"
let [<Literal>] private reportFileName = "Report.txt"

///Contains helper functions for using regular expressions
[<RequireQualifiedAccess>]
module private Regex = 

    open System.Text.RegularExpressions

    ///Perform a match and return the value of a group by index if the match is successful
    let getGroupValue pattern (group : int) input = 

            let match' = Regex.Match (input, pattern, RegexOptions.IgnoreCase)

            if match'.Success then
                Some (match'.Groups.[group].Value)
            else
                None  

///Contains helper functions for loading data from CSV files
[<RequireQualifiedAccess>]
module private Csv = 

    ///Get the data (all rows except the header) from a CSV file 
    let getData fileName = 
        async {
        
            let path = Path.Combine (AppDomain.CurrentDomain.BaseDirectory, fileName)
            let lines = File.ReadAllLines (path)

            return lines.[1..] //Skip the header row
        }

///Get a list of systems from the Systems.csv file
let getSystems = 

    let parse (line : String) = 

        let bits = line.Split ([| ',' |])
        let name = bits.[0]
        let x = Convert.ToDecimal bits.[1]
        let y = Convert.ToDecimal bits.[2]
        let z = Convert.ToDecimal bits.[3]

        { Id = 0; Name = name; Coordinates = { X = x; Y = y; Z = z; }}

    let assignId id system = { system with Id = id; }

    fun () -> async {

        let! lines = Csv.getData systemsFileName

        let systems = 
            lines
            |> Array.Parallel.map parse
            |> Array.Parallel.mapi assignId

        return (Array.toList systems)
    }

///Get a list of rare good manifests from Goods.csv
let getManifests = 

    let parseCommodity =

        let parseAllowance = 
            Regex.getGroupValue "^([0-9]+)" 1
            >> Option.map Convert.ToInt32

        let parseOrbit = 
            Regex.getGroupValue "^(([0-9]+)(\\.([0-9]+))?)" 1
            >> Option.map Convert.ToDecimal

        let parseSupply (value : String) = 
            if (value.IndexOf ('-') < 0) then
                (None, None)
            else
                
                let bits = value.Split ([| '-' |])

                let min = 
                    try
                        Some (Convert.ToInt32 bits.[0])
                    with
                    | _ -> None

                let max = 
                    try 
                        Some (Convert.ToInt32 bits.[1])
                    with
                    | _ -> None

                (min, max)            

        fun (line : String) ->
            try 

                let bits = line.Split ([| ',' |])
                let systemName = bits.[0]
                let stationName = bits.[1]
                let allowance = parseAllowance bits.[2]
                let supply = parseSupply bits.[3]
                let price = int (Convert.ToDecimal bits.[4])
                let itemName = bits.[5]                        
                let orbit = parseOrbit bits.[6]
                

                let station = {
                    Name = stationName;
                    Orbit = orbit;
                }
                        
                let commodity = {
                    Name = itemName;
                    Price = price;
                    Allowance = allowance;
                    Supply = supply;
                }

                Some (systemName, station, commodity)

            with 
            | _ -> None

    let createManifest = 

        let tryFindByName systemName = 
            List.tryFind (fun (system : System) -> system.Name === systemName) 

        fun systems (systemName, commodities) ->
            match (tryFindByName systemName systems)  with
            | Some system ->

                let commodities = 
                    commodities
                    |> Seq.map (fun (_, station, commodity) -> (station, commodity))
                    |> Seq.toList
                        
                let manifest = 
                    {
                        System = system;
                        Commodities = commodities;
                    }
                        
                Some manifest

            | _ -> None

    fun systems -> async {

        let! lines = Csv.getData goodsFileName
        
        let manifests = 
            lines
            |> Array.Parallel.choose parseCommodity
            |> Seq.groupBy (fun (systemName, _, _) -> systemName)
            |> Seq.choose (createManifest systems)

        return (Seq.toList manifests)
    }

///Get a TextWriter for Report.txt
let getReportWriter () = 

    let path = Path.Combine (AppDomain.CurrentDomain.BaseDirectory, reportFileName)

    if (File.Exists path) then
        File.Delete path

    File.CreateText (path)