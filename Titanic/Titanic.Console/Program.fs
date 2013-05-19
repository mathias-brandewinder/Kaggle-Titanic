open Titanic.Data
open Titanic.DecisionTreeModel

[<EntryPoint>]
let main argv = 

    let dataPath = @"..\..\..\Data\"

    let examplesFile = "training.csv"
    let evaluationFile = "evaluation.csv"
    let submissionFile = "submission.csv"

    let examplesPath = dataPath + examplesFile;
    let learningSample = 
        parseCsv examplesPath
        |> List.tail
        |> List.map readExample

    let model = train learningSample

    let evaluationPath = dataPath + evaluationFile;
    let submissionPath = dataPath + submissionFile;

    create evaluationPath submissionPath model

    printfn "Done"
    0 // return an integer exit code
