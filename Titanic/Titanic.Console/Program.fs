open Titanic.Data
open MachineLearning.AdaBoost

[<EntryPoint>]
let main argv = 

    let examplesPath = @"C:\Users\Mathias\Desktop\titanic.csv"
    let examples, labels = 
        parseCsv examplesPath
        |> List.tail
        |> List.map readPassenger
        |> List.map prepare
        |> List.toArray
        |> Array.unzip

    let iterations = 50
    let numSteps = 20.
    let targetError = 0.01

    let classify = train examples labels iterations numSteps targetError
    Array.zip examples labels 
    |> Array.map (fun (e, l) -> if l = (classify e) then 1. else 0.)
    |> Array.average
    |> printfn "Correct: %.2f"

    let submissionPath = @"C:\Users\Mathias\Desktop\submission.csv"
    let submit = @"C:\Users\Mathias\Desktop\result.csv"
    let model passenger = classify (passengerToArray passenger)
    create submissionPath submit model

    printfn "%A" argv
    0 // return an integer exit code
