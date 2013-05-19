namespace Titanic

open Titanic
open Titanic.Data
open Titanic.Transforms
open MachineLearning.DecisionTrees

module DecisionTreeModel =

    let headers = [| "Class"; "Gender"; "Fare"; "Outcome" |]

    let vars = Array.length headers - 1
    let variables = headers.[0 .. vars - 1]
    
    let binnedFare (fare:float) =
        if fare < 10.47 then "Cheap"
        elif fare < 73.5 then "Medium"
        else "Expensive"

    let extractFeatures p =
        [| p.Class.ToString();
           p.Gender;
           p.Fare |> binnedFare; |]

    let prepare example =
        let label, passenger = example
        [|  for feature in (extractFeatures passenger) do yield feature
            yield (outcomeToText label) |]

    let decisionTreePrepare sample =
        let data = 
            sample 
            |> List.map prepare 
            |> List.toArray
        headers, data

    let train sample =

        printfn "Training Decision Tree"
        let tree = build (decisionTreePrepare sample)
        
        let reverted tree p = 
            try classify p tree 
            with
            | _ -> "Death"

        let classify (p:Passenger) =
            let features = extractFeatures p 
            Array.zip variables features
            |> reverted tree
            |> textToOutcome
            
        printfn "Validating Decision Tree"
        sample 
        |> List.map (fun (l, e) -> if l = (classify e) then 1. else 0.)
        |> List.average
        |> printfn "Correct: %.3f"

        classify