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
    
    let maybeDouble text =
        match System.Double.TryParse(text) with 
        | true, v  -> v
        | false, _ -> 0.

    let age text = 
        match System.Double.TryParse(text) with 
        | true, age -> Some(age) 
        | false, _  -> None

    let genderAsNumber g =
        if g = "male" then 1. else 0.

    let readPassenger (line: string []) =
        Convert.ToInt32(line.[0]),
        { Class             = Convert.ToInt32(line.[1]);
          Name              = line.[2];
          Gender            = line.[3];
          Age               = maybeDouble line.[4];
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
          Age               = maybeDouble line.[3];
          SiblingsOrSpouses = Convert.ToInt32(line.[4]);
          ParentsOrChildren = Convert.ToInt32(line.[5]);
          Ticket            = line.[6];
          Fare              = maybeDouble line.[7];
          Cabin             = line.[8];
          Embarked          = line.[9] }

    let passengerToArray p =
        [| (float)p.Class;
           p.Age;
           genderAsNumber p.Gender;
           (float)p.SiblingsOrSpouses;
           (float)p.ParentsOrChildren;
           (float)p.Fare |]

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