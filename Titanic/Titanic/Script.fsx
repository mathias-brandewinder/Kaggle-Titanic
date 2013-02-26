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

let h vector =
        let size = vector |> Array.length
        vector 
        |> Seq.groupBy (fun e -> e)
        |> Seq.sumBy (fun e ->
            let count = e |> snd |> Seq.length
            let p = (float)count / (float)size
            - p * log p)    

// Find binning values to partitions inverval in an entropy-minimizing way
let entropyBinner data grain =

    let size = Array.length data
    let values = Array.map fst data
    let min, max = Array.min values, Array.max values
    let step = (max - min) / (float)grain
    let initial = h (data |> Array.map snd)

    seq { for s in 1 .. (grain - 1) -> min + (max - min) * ((float)s / (float)grain) }
    |> Seq.map (fun bin -> bin, Array.partition (fun x -> (fst x) <= bin) data)
    |> Seq.map (fun (bin, (v1, v2)) -> 
           let d1 = v1 |> Array.map snd
           let d2 = v2 |> Array.map snd 
           let h1 = (h d1) * (float)(Array.length v1) / (float)size
           let h2 = (h d2) * (float)(Array.length v2) / (float)size
           bin, (h1 + h2))      
    |> Seq.minBy snd
         