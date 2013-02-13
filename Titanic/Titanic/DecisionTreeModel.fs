namespace Titanic

open Titanic.Data
open MachineLearning.DecisionTrees

module DecisionTreeModel =

    let extractLabel label =
        match label with
        | Life  -> "Life"
        | Death -> "Death"

    let convertLabel label =
        match label with
        | "Life"  -> Life
        | "Death" -> Death
        | _ -> failwith "kaboom"

    let headers = [| "Class"; "Gender"; "SibOrSp"; "ParOrChi"; "Embark"; "Outcome"; "GenderDetails"; "Title"; "CabinSection" |]
    let vars = Array.length headers - 1
    let variables = headers.[0 .. vars - 1]

    let correctedPoC v = if v > 2 then 2 else v

    let extractFeatures p =
        [| p.Class.ToString();
           p.Gender;
           p.SiblingsOrSpouses.ToString();
           (correctedPoC p.ParentsOrChildren).ToString();
           p.Embarked;
           genderDetails p.Name;
           job p.Name;
           cabinSection p.Cabin |]

    let transform example =
        let label, passenger = example
        Array.append (extractFeatures passenger) [| extractLabel label |]

    let decisionTreePrepare sample =
        let data = sample |> List.map transform |> List.toArray
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
            |> convertLabel
            
        printfn "Validating Decision Tree"
        sample 
        |> List.map (fun (l, e) -> if l = (classify e) then 1. else 0.)
        |> List.average
        |> printfn "Correct: %.3f"

        classify