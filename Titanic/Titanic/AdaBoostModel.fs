namespace Titanic

open Titanic.Data
open MachineLearning.AdaBoost

module AdaBoostModel =
    
    let extractFeatures p =
        [| (float)p.Class;
           p.Age;
           genderAsNumber p.Gender;
           (float)p.SiblingsOrSpouses;
           (float)p.ParentsOrChildren;
           (float)p.Fare; |]
//           ticket p.Ticket;
//           cabin p.Cabin;
//           embark p.Embarked |]
    
    let extractLabel label =
        match label with
        | Life  -> 1.
        | Death -> -1.

    let adaboostTransform example =
        let label, passenger = example
        extractFeatures passenger,
        extractLabel label

    let adaboostPrepare sample =
        sample
        |> List.map adaboostTransform
        |> List.toArray
        |> Array.unzip

    let convertLabel v =
        match v with 
        | 1.  -> Life
        | -1. -> Death
        | _   -> failwith "Invalid classification"

    let train sample =

        printfn "Training AdaBoost"
        let examples, labels = adaboostPrepare sample
        let iterations = 50
        let numSteps = 20.
        let targetError = 0.01

        let classify = train examples labels iterations numSteps targetError

        printfn "Validating AdaBoost"
        Array.zip examples labels 
        |> Array.map (fun (e, l) -> if l = (classify e) then 1. else 0.)
        |> Array.average
        |> printfn "Correct: %.3f"

        fun (p:Passenger) ->
            extractFeatures p 
            |> classify
            |> convertLabel