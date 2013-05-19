namespace Titanic

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
                
    // Convert to a double, 
    // replacing with default in case of failure
    let maybeDouble def text =
        match System.Double.TryParse(text) with 
        | true, v  -> v
        | false, _ -> def

    let medianAge = 28.
    let age = maybeDouble medianAge

    let medianFare = 14.45
    let fare = maybeDouble medianFare

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