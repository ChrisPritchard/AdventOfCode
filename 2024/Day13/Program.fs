let input = System.IO.File.ReadAllLines "input.txt"

let filter_string = "Button AB:X+,YPrize=".ToCharArray()

let split (s: string) =
    let res = s.Split(filter_string, System.StringSplitOptions.RemoveEmptyEntries)
    int64 res[0], int64 res[1]

let parsed =
    Array.chunkBySize 4 input
    |> Array.map (fun lines -> split lines[0], split lines[1], split lines[2])

let test (ax, ay) (bx, by) (tx, ty) =
    [ 100L .. (-1L) .. 0L ]
    |> List.tryPick (fun b ->
        [ 0L .. 100L ]
        |> List.tryPick (fun a ->
            if a * ax + b * bx = tx && a * ay + b * by = ty then
                Some(a * 3L + b)
            else
                None))

let sum =
    parsed
    |> Array.map (fun (a, b, t) -> test a b t)
    |> Array.choose id
    |> Array.sum

printfn "Part 1: %d" sum

// using cramers rule, following this blog post that solves this challenge: https://kyle.so/writing/aoc-2024

let cramers_test (ax, ay) (bx, by) (tx, ty) =
    let d = (ax * by) - (ay * bx)
    let d1 = ((tx + 10000000000000L) * by) - ((ty + 10000000000000L) * bx)
    let d2 = ((ty + 10000000000000L) * ax) - ((tx + 10000000000000L) * ay)

    if d1 % d <> 0 || d2 % d <> 0 then
        None
    else
        Some((d1 / d) * 3L + (d2 / d))

let new_sum =
    parsed
    |> Array.map (fun (a, b, t) -> cramers_test a b t)
    |> Array.choose id
    |> Array.sum

printfn "Part 2: %d" new_sum
