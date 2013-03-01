namespace Titanic

open Titanic.Data
open MachineLearning.DecisionTrees

module GenderClassTicketModel =

    let extractLabel label =
        match label with
        | Life  -> "Life"
        | Death -> "Death"

    let convertLabel label =
        match label with
        | "Life"  -> Life
        | "Death" -> Death
        | _ -> failwith "kaboom"

    // tried embarked, degrades results
    let headers = [| "Class"; "Gender"; "Ticket"; "Age"; "Siblings"; "Children"; "Outcome" |]
    let vars = Array.length headers - 1
    let variables = headers.[0 .. vars - 1]

    let correctedPoC v = if v > 2 then 2 else v

    let ticket v =
        if   v < 10. then "10-"
        elif v < 20. then "10-20"
        elif v < 30. then "20-30"
        else              "30+"
    // age does not improve model
    let age v =
        if v < 18. then "Kid"
        else "Adult"

    let siblings v =
        if v = 0 then "None" else "Some"

    let children v =
        if v = 0 then "None" else "Some"

    let extendedGender gender (name: string) =
        match gender with
        | "male" ->
            if name.Contains("Master.") then "Master" else "Male"
        | "female" ->
            if name.Contains("Miss.") then "Miss" else "Female"
        | _ -> failwith "no match?"

    let extractFeatures p =
        [| p.Class.ToString();
           p.Gender;
           ticket p.Fare;
           age p.Age;
           siblings p.SiblingsOrSpouses;
           children p.ParentsOrChildren; |]

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