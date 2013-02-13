namespace Titanic

open Titanic.Data
open MachineLearning.DecisionTrees

module CombinedModel =

    let extractLabel label =
        match label with
        | Life  -> "Life"
        | Death -> "Death"

    let convertLabel label =
        match label with
        | "Life"  -> Life
        | "Death" -> Death
        | _ -> failwith "kaboom"

    let headers = [| "Class"; "Gender"; "SibOrSp"; "ParOrChi"; "Embark"; "Outcome"; "GenderDetails"; "Title"; "CabinSection"; "AdaBoost" |]
    let vars = Array.length headers - 1
    let variables = headers.[0 .. vars - 1]

    let correctedPoC v = if v > 2 then 2 else v

    let extractFeatures p adaModel =
        [| p.Class.ToString();
           p.Gender;
           p.SiblingsOrSpouses.ToString();
           (correctedPoC p.ParentsOrChildren).ToString();
           p.Embarked;
           genderDetails p.Name;
           job p.Name;
           cabinSection p.Cabin;
           extractLabel (adaModel (p)) |]

    let transform adaModel example  =
        let label, passenger = example
        Array.append (extractFeatures passenger adaModel) [| extractLabel label |]

    let decisionTreePrepare sample adaModel =
        let data = sample |> List.map (fun e -> transform adaModel e) |> List.toArray
        headers, data

    let train sample =

        printfn "Training AdaBoost"
        let adaModel = Titanic.AdaBoostModel.train sample

        printfn "Training Decision Tree"
        let tree = build (decisionTreePrepare sample adaModel)
        let reverted tree p = 
            try classify p tree 
            with
            | _ -> "Death"

        let classify (p:Passenger) =
            let features = extractFeatures p adaModel
            Array.zip variables features
            |> reverted tree
            |> convertLabel
            
        printfn "Validating Decision Tree"
        sample 
        |> List.map (fun (l, e) -> if l = (classify e) then 1. else 0.)
        |> List.average
        |> printfn "Correct: %.3f"

        classify