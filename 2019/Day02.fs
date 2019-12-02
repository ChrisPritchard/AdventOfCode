module Day02

open System.IO

let input = File.ReadAllText ("./inputs/day02.txt") |> fun s -> s.Split (',') |> Array.map int

let part1 () =

    let part1Data = Array.copy input
    part1Data.[1] <- 12
    part1Data.[2] <- 2

    let rec processor index =
        let op = input.[index]
        if op = 99 || (op <> 1 && op <> 2) then ()
        else
            let v1 = part1Data.[index + 1]
            let v2 = part1Data.[index + 2]
            let o = part1Data.[index + 3]
            if op = 1 then
                part1Data.[o] <- part1Data.[v1] + part1Data.[v2]
            else
                part1Data.[o] <- part1Data.[v1] * part1Data.[v2]
            processor (index + 4)

    processor 0
    part1Data.[0]

let part2 () =
    
    let test noun verb =
        let part2Data = Array.copy input
        part2Data.[1] <- noun
        part2Data.[2] <- verb

        let rec processor index =
            let op = input.[index]
            if op = 99 || (op <> 1 && op <> 2) then ()
            else
                let v1 = part2Data.[index + 1]
                let v2 = part2Data.[index + 2]
                let o = part2Data.[index + 3]
                if op = 1 then
                    part2Data.[o] <- part2Data.[v1] + part2Data.[v2]
                else
                    part2Data.[o] <- part2Data.[v1] * part2Data.[v2]
                processor (index + 4)

        processor 0
        if part2Data.[0] = 19690720 then Some (100 * noun + verb) else None
    
    [0..99] |> Seq.collect (fun noun ->
        [0..99] |> Seq.map (fun verb -> test noun verb)) |> Seq.pick id