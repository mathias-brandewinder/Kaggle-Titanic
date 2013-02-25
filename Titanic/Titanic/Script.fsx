#r "Microsoft.VisualBasic"
#load "Data.fs"
open Titanic.Data
open System.Text.RegularExpressions

let dataPath = @"C:\Users\Mathias\Documents\GitHub\Kaggle-Titanic\Titanic\Data\"

let examplesFile = "training.csv"
let evaluationFile = "evaluation.csv"

let examplesPath = dataPath + examplesFile;
let learningSample = 
    parseCsv examplesPath
    |> List.tail
    |> List.map readExample

let regex = new Regex("\w*\.", RegexOptions.IgnoreCase)
       
let titles = 
    learningSample 
    |> List.map (fun (o, p) -> p.Name) 
    |> List.map (fun n -> 
        let matches = regex.Matches(n)
        if (matches.Count > 0) then matches.[0].Value else "")
    |> List.toSeq
    |> Seq.groupBy (fun t -> t)
    |> Map.ofSeq
    |> Map.map (fun t ts -> Seq.length ts)
    |> Map.iter (fun k c -> printfn "%s %i" k c)