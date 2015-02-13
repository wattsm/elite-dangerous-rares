[<AutoOpen>]
module Model

open System

///Represents a 3 dimensional coordinate
type Coordinates = {
    X : Decimal;
    Y : Decimal;
    Z : Decimal;
}

///Represents a star system
type System = {
    Id : Int32;
    Name : String;
    Coordinates : Coordinates;
}

///Represents a station 
type Station = {
    Name : String;
    Orbit: Decimal option;
}

///Represents a commodity / rare good
type Commodity = {    
    Name : String;
    Price : Int32;
    Allowance : Int32 option;
    Supply : (Int32 option * Int32 option);
}

///Represents rare goods information for a system
type Manifest = {
    System : System;
    Commodities : (Station * Commodity) list;
}

///Contains functions for calculating distances
[<RequireQualifiedAccess>]
module Distance = 

    ///Calculate the Euclidian distance between two coordinates (ignore the curvature of spacetime ;-))
    let betweenCoordinates coordsA coordsB = 

        let dxSq = Math.Pow (float (coordsA.X - coordsB.X), 2.0)
        let dySq = Math.Pow (float (coordsA.Y - coordsB.Y), 2.0)
        let dzSq = Math.Pow (float (coordsA.Z - coordsB.Z), 2.0)

        decimal (Math.Sqrt (dxSq + dySq + dzSq))

    ///Calculate the Euclidian distance between two systems
    let betweenSystems systemA systemB = 

        let coordsA = systemA.Coordinates
        let coordsB = systemB.Coordinates

        betweenCoordinates coordsA coordsB

    ///Calculate the Euclidian distance between the centroids of two clusters of manifests
    let betweenClusters = 

        let averageCoordinates manifests =

            let systems = List.map (fun manifest -> manifest.System) manifests
            
            let x = List.averageBy (fun system -> system.Coordinates.X) systems
            let y = List.averageBy (fun system -> system.Coordinates.Y) systems
            let z = List.averageBy (fun system -> system.Coordinates.Z) systems
                
            { X = x; Y = y; Z = z; }

        fun clusterA clusterB -> 

            let coordsA = averageCoordinates clusterA
            let coordsB = averageCoordinates clusterB 

            betweenCoordinates coordsA coordsB
            
