
open System.IO
open FParsec.Primitives
open FParsec.CharParsers
open Newtonsoft.Json.Linq

[<EntryPoint>]
let main _ =

    let input = File.ReadAllText "input.txt"

    let pNumberSum = many (pint32 <|> (anyChar |>> fun _ -> 0)) |>> List.sum

    let part1 = match run pNumberSum input with Success (r, _, _) -> r | Failure (ex, _, _) -> failwith ex
    printfn "part 1: %i" part1

    let json = JArray.Parse input

    let rec countJson (json: JToken) = 
        match json.Type with
        | JTokenType.Integer -> int json
        | JTokenType.Array ->
            let array = json :?> JArray
            array |> Seq.sumBy countJson
        | JTokenType.Object ->
            let obj = json :?> JObject
            let count = 
                obj.Properties() 
                |> Seq.fold (fun count (p: JProperty) -> 
                    match count with 
                    | None -> None 
                    | Some c ->
                        if p.Value.Type = JTokenType.String && string p.Value = "red" then None
                        else Some (c + countJson p.Value)) 
                    (Some 0)
            match count with Some c -> c | _ -> 0
        | _ -> 0

    let part2 = countJson json
    printfn "part 2: %i" part2

    0
