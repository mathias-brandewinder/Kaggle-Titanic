open Titanic.Data
open MachineLearning.AdaBoost
open Titanic.AdaBoostModel
open Titanic.DecisionTreeModel

[<EntryPoint>]
let main argv = 

    let examplesPath = @"C:\Users\Mathias\Desktop\titanic.csv"
    
    let learningSample = 
        parseCsv examplesPath
        |> List.tail
        |> List.map readExample

    //let adaClassifier = train learningSample
    let treeClassifier = train learningSample

//    let submissionPath = @"C:\Users\Mathias\Desktop\submission.csv"
//    let submit = @"C:\Users\Mathias\Desktop\result.csv"
//    create submissionPath submit treeClassifier

    printfn "Done"
    0 // return an integer exit code
