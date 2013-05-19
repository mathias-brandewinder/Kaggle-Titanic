open System

let logb n b = log n / log b

let entropy (data:(float*int)seq) =
    let bins = Seq.countBy snd data
    let size = Seq.length data
    let N = (float)size
    bins 
    |> Seq.sumBy (fun (_,count) -> 
        let p = (float)count/N
        - p * log p)

type Block = 
    { Data:(float*int)seq }
    member this.S = this.Data |> Seq.length
    member this.K = this.Data |> Seq.map snd |> Seq.distinct |> Seq.length
    member this.H = entropy (this.Data)

let faster (data:Block) =
    let candidates = data.Data |> Seq.map fst |> Seq.distinct
    let walls = seq { 
        for value in candidates do
            let g1, g2 = 
                data.Data 
                |> Seq.toList 
                |> List.partition (fun (v,c) -> v <= value)
            let block1 = { Data = g1 }
            let block2 = { Data = g2 }
           
            let gain = data.H - (((float)block1.S/(float)data.S)*block1.H + ((float)block2.S/(float)data.S)*block2.H)
            let delta = logb (pown 3. data.K - 2.) 2. - ((float)data.K * data.H - (float)block1.K * block1.H - (float)block2.K * block2.H)
            let threshold = ((log ((float)data.S-1.)) / (float)data.S) + (delta / (float)data.S)

            if gain >= threshold then yield (value, gain, block1, block2) }

    if Seq.isEmpty walls then None
    else 
        walls 
        |> Seq.maxBy (fun (value, gain, b1, b2) -> gain) 
        |> Some
    
let rec recursiveBinner (walls:float list) (data:Block) =
    match (faster data) with
    | None -> walls
    | Some(value, gain, b1, b2) ->
        let walls = value::walls
        let walls = recursiveBinner walls b1
        recursiveBinner walls b2

let binner (data:(float*int)seq) =
    recursiveBinner [] { Data = data }

let rng = System.Random()
let test2 = [ 
    for i in 1..1000 -> (rng.NextDouble(), rng.Next(0,2))
    for i in 1..1000 -> (rng.NextDouble() + 0.5, rng.Next(1,3))
    for i in 1..1000 -> (rng.NextDouble() + 1.0, rng.Next(2,4))
    for i in 1..1000 -> (rng.NextDouble() + 1.5, rng.Next(3,5)) ]
let b2 = { Data = test2 }

