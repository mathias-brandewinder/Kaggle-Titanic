namespace Titanic

module Transforms =

    let outcomeToText label =
        match label with
        | Life  -> "Life"
        | Death -> "Death"

    let textToOutcome label =
        match label with
        | "Life"  -> Life
        | "Death" -> Death
        | _ -> failwith "kaboom"

    let genderAsNumber g =
        if g = "male" then 1. else 0.
    
    let embark s = 
        if s   = "Q" then 0.55 
        elif s = "C" then 0.39 
        elif s = "S" then 0.34
        else 0.38

    let ticket (s: string) =        
        if s.Length = 0 then 0.38 
        else
            let first = s.[0]
            match first with
            | '9' -> 1.00
            | 'P' -> 0.65
            | '1' -> 0.63
            | 'F' -> 0.57
            | '2' -> 0.46
            | 'C' -> 0.34
            | 'S' -> 0.32
            | 'L' -> 0.25
            | '3' -> 0.24
            | '4' -> 0.2
            | '6' -> 0.17
            | 'W' -> 0.15
            | '7' -> 0.11
            | 'A' -> 0.07
            | _   -> 0.38

    let cabinSection (c: string) =
        if c.Length = 0 then "Unknown"
        else c.[0].ToString()

    let cabin (c: string) =
        if c.Length = 0 then 0.30 
        else
            let first = c.[0]
            match first with
            | 'D' 
            | 'E' 
            | 'B' -> 0.75
            | 'F' 
            | 'C' -> 0.60
            | 'G' 
            | 'A' -> 0.50
            | _   -> 0.38

    let genderDetails (name: string) =
        if name.Contains("Mr.") then "Mr"
        elif name.Contains("Mrs.") then "Mrs"
        elif name.Contains("Miss.") then "Miss"
        else "Unknown"

    let job (name: string) =
        if name.Contains("Dr.") then "Dr"
        elif name.Contains("Master") then "Master"
        elif name.Contains("Rev.") then "Rev"
        else "Unknown"