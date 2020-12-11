module Day11

open System.IO

let input = File.ReadAllLines "./inputs/day11.txt"

let processed () =
    input |> Array.map (fun line -> line |> Seq.toArray)

let aval y x (a: char [][]) = 
    if y < 0 || x < 0 || y >= a.Length || x >= a.[0].Length then ' ' else a.[y].[x]

let cardinals = 
    [|
        -1,-1;-1,0;-1,1;
        0,-1;0,1;
        1,-1;1,0;1,1;
    |]

let part1 () =
    let state y x c a =
        let occupied = 
            cardinals 
            |> Array.filter (fun (dy, dx) -> aval (y + dy) (x + dx) a = '#') 
            |> Array.length
        match c, occupied with
        | 'L', 0 -> '#'
        | '#', n when n >= 4 -> 'L'
        | c, _ -> c

    let rec iter a =
        let n = Array.mapi (fun y line -> Array.mapi (fun x c -> state y x c a) line) a
        if n = a then 
            n |> Seq.collect id |> Seq.filter ((=) '#') |> Seq.length
        else
            iter n

    let start = processed ()
    iter start

let part2 () =
    let state y x c a =
        let occupied = 
            cardinals 
            |> Array.filter (fun (dy, dx) -> 
                let v = 
                    Seq.initInfinite ((+) 1)
                    |> Seq.pick (fun n ->
                        let v = aval (y + n * dy) (x + n * dx) a
                        if v = ' ' || v <> '.' then Some v else None)
                v = '#') 
            |> Array.length
        match c, occupied with
        | 'L', 0 -> '#'
        | '#', n when n >= 5 -> 'L'
        | c, _ -> c

    let rec iter a =
        let n = Array.mapi (fun y line -> Array.mapi (fun x c -> state y x c a) line) a
        //n |> Array.map Common.asString |> String.concat Common.newline |> printfn "%s%s" Common.newline
        //System.Threading.Thread.Sleep 1000 // <- and ^ this code will print out the map then wait, for debugging
        if n = a then 
            n |> Seq.collect id |> Seq.filter ((=) '#') |> Seq.length
        else
            iter n

    let start = processed ()
    iter start