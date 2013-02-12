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
    
    let maybeDouble text def =
        match System.Double.TryParse(text) with 
        | true, v  -> v
        | false, _ -> def

    let age text = 
        match System.Double.TryParse(text) with 
        | true, age -> Some(age) 
        | false, _  -> None

    let genderAsNumber g =
        if g = "male" then 1. else 0.
    
    let embark s = 
        if s = "Q" then 0.55 
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

    let readPassenger (line: string []) =
        Convert.ToInt32(line.[0]),
        { Class             = Convert.ToInt32(line.[1]);
          Name              = line.[2];
          Gender            = line.[3];
          Age               = maybeDouble line.[4] 28.;
          SiblingsOrSpouses = Convert.ToInt32(line.[5]);
          ParentsOrChildren = Convert.ToInt32(line.[6]);
          Ticket            = line.[7];
          Fare              = Convert.ToDouble(line.[8]);
          Cabin             = line.[9];
          Embarked          = line.[10] }

    let readPassenger2 (line: string []) =
        { Class             = Convert.ToInt32(line.[0]);
          Name              = line.[1];
          Gender            = line.[2];
          Age               = maybeDouble line.[3] 28.;
          SiblingsOrSpouses = Convert.ToInt32(line.[4]);
          ParentsOrChildren = Convert.ToInt32(line.[5]);
          Ticket            = line.[6];
          Fare              = maybeDouble line.[7] 14.45;
          Cabin             = line.[8];
          Embarked          = line.[9] }

    let passengerToArray p =
        [| (float)p.Class;
           p.Age;
           genderAsNumber p.Gender;
           (float)p.SiblingsOrSpouses;
           (float)p.ParentsOrChildren;
           (float)p.Fare;
           ticket p.Ticket;
           cabin p.Cabin;
           embark p.Embarked |]

    let prepare (pass: int * Passenger) =
            let surv, p = pass
            passengerToArray p,
            if surv = 1 then 1.0 else -1.0 
          
    // create submission file
    let create sourceFile 
               resultFile 
               (model: Passenger -> float) =

        let data = 
            parseCsv sourceFile 
            |> List.tail
            |> List.map readPassenger2
            |> List.map (fun e -> if (model e) > 0. then "1" else "0")
            |> List.toArray
        File.WriteAllLines(resultFile, data)