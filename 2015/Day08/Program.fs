open System
open System.IO
open System.Text
open FParsec.CharParsers
open FParsec.Primitives

[<EntryPoint>]
let main _ =

    let input = File.ReadAllLines "input.txt"

    let pQuote = pchar '"'
    let pEscapedSlash = pstring "\\\\" |>> fun _ -> '\\'
    let pEscapedQuote = pstring "\\\"" |>> fun _ -> '"'
    let pCharCode = 
        pstring "\\x" >>. hex .>>. hex 
        |>> fun (c1, c2) -> 
            new System.String ([|c1;c2|]) 
            |> fun s -> [|Convert.ToInt32(s, 16)|] 
            |> Array.map byte 
            |> Encoding.ASCII.GetString
            |> Seq.head

    let pLine = pQuote >>. manyTill (pEscapedSlash <|> pEscapedQuote <|> pCharCode <|> anyChar) pQuote
    let parse line = match run pLine line with Success (r, _, _) -> r | Failure (ex, _, _) -> failwith ex

    let part1 = input |> Array.sumBy (fun s -> s.Length - (parse s |> List.length))
    printfn "part 1: %i" part1

    let encode (s:string) =
        "\"" + s.Replace("\\", "\\\\").Replace("\"", "\\\"") + "\""
    let part2 = input |> Array.sumBy (fun s -> (encode s).Length - s.Length)
    printfn "part 2: %i" part2

    0
