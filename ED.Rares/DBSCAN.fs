module DBSCAN

open System

///Represents the settings that can be used for this DBSCAN implementation
type Settings = {
    Manifests : Manifest list;
    MaxDistance : Decimal;
    MinSize : Int32;
}

///Contains functions implementing the DBSCAN algorithm
[<AutoOpen>]
module private Algorithm = 

    ///Represents the current state of the algorithm
    type State = {
        ClusterNo : Int32;
        Clusters : (Int32 * Manifest) list;
        Noise : Manifest list;
        Visited : Int32 list;
    }
    with
        static member Empty = 
            {
                ClusterNo = 0;
                Clusters = [];
                Noise = [];
                Visited = [];
            }

    ///Contains miscellaneous helper functions
    [<AutoOpen>]
    module private Helpers = 

        ///True if a manifest has been visited previously
        let visited manifest state = 
            state.Visited
            |> List.exists ((=) manifest.System.Id)

        ///True if a manifest has been assigned to a cluster
        let clustered manifest state = 
            state.Clusters
            |> List.exists (fun (_, clustered) -> manifest.System.Id = clustered.System.Id)

        ///Returns all manifests, including the original, where the systems are within the maximum distance
        let getLocalGroup settings manifest = 
            settings.Manifests
            |> List.filter (fun candidate -> 
                    (Distance.betweenSystems manifest.System candidate.System) <= settings.MaxDistance
                ) 

        ///True if a manifest is a core point in a cluster
        let isCore settings localGroup =
            (List.length localGroup) >= settings.MinSize

        ///Adds a manifest to the list of visited manifests
        let recordVisit manifest state = 
            { state with 
                Visited = (manifest.System.Id :: state.Visited); 
            }

        ///Adds a manifest to the list of "noise" (potential outliers) manifests
        let markAsNoise manifest state = 
            { state with 
                Noise = (manifest :: state.Noise); 
            }

        ///Increments the current cluster counter and assigns the manifest to the new cluster
        let startNewCluster manifest state = 
            
            let clusterNo = state.ClusterNo + 1

            { state with
                ClusterNo = clusterNo;
                Clusters = ((clusterNo, manifest) :: state.Clusters);
            }

        ///Adds a manifest to the current cluster
        let addToCluster manifest state = 
            { state with 
                Clusters = ((state.ClusterNo, manifest) :: state.Clusters); 
            }

    //Expand a cluster by recursively moving out from core points
    let expandCluster settings =
        
        //Partially apply some functions with settings where appropriate
        let getLocalGroup' = getLocalGroup settings
        let isCore' = isCore settings

        let union x y = 
            List.append x y
            |> Seq.distinctBy (fun manifest -> manifest.System.Id)
            |> Seq.toList

        let rec expandLocalGroup state = function
            | [] -> state
            | manifest :: manifests when (visited manifest state) -> 

                //Manifest has already been visited; simply add to the current cluster if 
                //it has not already been clustered.
            
                let state' = 
                    if (clustered manifest state) then
                        state
                    else
                        addToCluster manifest state

                expandLocalGroup state' manifests

            | manifest :: manifests ->

                //Manifest has not been visited, so add to the visited list. As it has not been visited
                //it also cannot be clustered, so add to the current cluster. Expand the local group and
                //if this manifest represents a core point then add the local group to the list of
                //manifests to consider and recurse.

                let state' =                     
                    state
                    |> recordVisit manifest
                    |> addToCluster manifest                      

                let localGroup = getLocalGroup' manifest //Includes self

                let manifests' = 
                    if (isCore' localGroup) then
                        union manifests localGroup
                    else
                        manifests
                    
                expandLocalGroup state' manifests'

        fun manifest localGroup state ->

            //Increment the cluster, add the current manifest and expand

            let state' = startNewCluster manifest state
            in expandLocalGroup state' localGroup

    ///Runs the DBSCAN algorithm against a list of manifests using the given settings
    let rec run settings =

        //Partially apply some functions with settings where appropriate
        let getLocalGroup' = getLocalGroup settings
        let expandCluster' = expandCluster settings
        let isCore' = isCore settings

        fun state -> function
            | [] -> state
            | manifest :: manifests ->

                let state' = 
                    if (visited manifest state) then

                        //Point has already been visited, so it can be ignored 
                        state
                    else

                        let localGroup = getLocalGroup' manifest

                        if (isCore' localGroup) then

                            //Point is core, so it can be used to create a new cluster

                            state
                            |> recordVisit manifest
                            |> expandCluster' manifest localGroup

                        else

                            //Point is not core, so mark as noise. This leads to scenarios where a point is marked as noise
                            //and is then later included in another cluster. Presumably noise points are potential outliers, rather than 
                            //definite outliers.
                            //This mutable F# version counts a point as noise if it's marked as noise AND is not clustered:
                            //http://www.fssnip.net/jo (line 83).

                            state
                            |> recordVisit manifest
                            |> markAsNoise manifest
                
                run settings state' manifests

///Apply the DBSCAN to the given settings, returning the clusters and noise 
let apply settings = 

    let state = run settings State.Empty settings.Manifests
    
    let clusters = 
        state.Clusters
        |> Seq.groupBy fst
        |> Seq.map (fun (clusterNo, members) ->
                let manifests = 
                    members
                    |> Seq.map snd 
                    |> Seq.sortBy (fun manifest -> manifest.System.Name)
                    |> Seq.toList
                in (clusterNo, manifests)
            )
        |> Seq.sortBy fst
        |> Seq.toList

    (clusters, state.Noise)