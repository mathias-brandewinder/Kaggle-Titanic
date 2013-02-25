namespace Titanic

open System
open Microsoft.VisualBasic.FileIO    

type Passenger = 
    { Class: int;
      Name: string;
      Gender: string;
      Age: float;
      SiblingsOrSpouses: int;
      ParentsOrChildren: int;
      Ticket: string;
      Fare: float;
      Cabin: string;
      Embarked: string }

type Outcome = Life | Death
type Model = Passenger -> Outcome

module Data =

    open System
    open System.IO
    open System.Text.RegularExpressions
    open Microsoft.VisualBasic.FileIO    

    let parseCsv (filePath: string) =

        use reader = new TextFieldParser(filePath)
        reader.TextFieldType <- FieldType.Delimited
        reader.SetDelimiters(",")
        [ while (not reader.EndOfData) do yield reader.ReadFields() ]

    let labelToOutcome (l: string) =
        match Convert.ToInt32(l) with
        | 1 -> Life
        | 0 -> Death
        | _ -> failwith "Unrecognized label"

    let outcomeToLabel outcome = 
        match outcome with 
        | Life  -> "1" 
        | Death -> "0"
                
    // Convert to a double, replacing with default in case of failure
    let maybeDouble def text =
        match System.Double.TryParse(text) with 
        | true, v  -> v
        | false, _ -> def

    let medianAge = 28.
    let age = maybeDouble medianAge

    let medianFare = 14.45
    let fare = maybeDouble medianFare

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

    let readExample (line: string []) =
        labelToOutcome line.[0],
        { Class             = Convert.ToInt32(line.[1]);
          Name              = line.[2];
          Gender            = line.[3];
          Age               = age line.[4];
          SiblingsOrSpouses = Convert.ToInt32(line.[5]);
          ParentsOrChildren = Convert.ToInt32(line.[6]);
          Ticket            = line.[7];
          Fare              = Convert.ToDouble(line.[8]);
          Cabin             = line.[9];
          Embarked          = line.[10] }

    let readValidation (line: string []) =
        { Class             = Convert.ToInt32(line.[0]);
          Name              = line.[1];
          Gender            = line.[2];
          Age               = age line.[3];
          SiblingsOrSpouses = Convert.ToInt32(line.[4]);
          ParentsOrChildren = Convert.ToInt32(line.[5]);
          Ticket            = line.[6];
          Fare              = fare line.[7];
          Cabin             = line.[8];
          Embarked          = line.[9] }
          
    // create submission file
    let create sourceFile 
               resultFile 
               (model: Passenger -> Outcome) =
        let data = 
            parseCsv sourceFile 
            |> List.tail
            |> List.map readValidation
            |> List.map (fun e -> outcomeToLabel (model e))
            |> List.toArray
        File.WriteAllLines(resultFile, data)