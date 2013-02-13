open Titanic.Data
open MachineLearning.AdaBoost
open Titanic.AdaBoostModel

[<EntryPoint>]
let main argv = 

    let examplesPath = @"C:\Users\Mathias\Desktop\titanic.csv"
    
    let learningSample = 
        parseCsv examplesPath
        |> List.tail
        |> List.map readExample

    let adaClassifier = train learningSample

    let submissionPath = @"C:\Users\Mathias\Desktop\submission.csv"
    let submit = @"C:\Users\Mathias\Desktop\result.csv"
//    let model passenger = classify (passengerToArray passenger)
//    create submissionPath submit model

    printfn "Done"
    0 // return an integer exit code
