open Titanic.Data
open MachineLearning.AdaBoost

[<EntryPoint>]
let main argv = 

    let examplesPath = @"C:\Users\Mathias\Desktop\titanic.csv"
    
    let learningSample = 
        parseCsv examplesPath
        |> List.tail
        |> List.map readExample

    let classifier = Titanic.CombinedModel.train learningSample

//    let submissionPath = @"C:\Users\Mathias\Desktop\submission.csv"
//    let submit = @"C:\Users\Mathias\Desktop\result.csv"
//    create submissionPath submit classifier

    printfn "Done"
    0 // return an integer exit code
