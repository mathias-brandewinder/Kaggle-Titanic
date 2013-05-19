#r "Microsoft.VisualBasic"
#load "Data.fs"
open Titanic
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

let asInt (outcome:Outcome) =
    match outcome with
    | Life -> 1
    | Death -> 0
      
let ageSample = 
    learningSample 
    |> Seq.map (fun (o,p) -> 
        p.Age,
        (asInt o))

let fareSample = 
    learningSample 
    |> Seq.map (fun (o,p) -> 
        p.Fare,
        (asInt o))
   
let classSample = 
    learningSample 
    |> Seq.map (fun (o,p) -> 
        (float)p.Class,
        (asInt o))