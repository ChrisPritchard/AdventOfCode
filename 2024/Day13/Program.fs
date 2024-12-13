let input = System.IO.File.ReadAllLines "input.txt"

let filter_string = "Button AB:X+,YPrize=".ToCharArray()

let split (s: string) =
    let res = s.Split(filter_string, System.StringSplitOptions.RemoveEmptyEntries)
    int res[0], int res[1]

let parsed =
    Array.chunkBySize 4 input
    |> Array.map (fun lines -> split lines[0], split lines[1], split lines[2])

let test (ax, ay) (bx, by) (tx, ty) =
    [ 100 .. (-1) .. 0 ]
    |> List.tryPick (fun b ->
        [ 0..100 ]
        |> List.tryPick (fun a ->
            if a * ax + b * bx = tx && a * ay + b * by = ty then
                Some(a * 3 + b)
            else
                None))

let sum =
    parsed
    |> Array.map (fun (a, b, t) -> test a b t)
    |> Array.choose id
    |> Array.sum

printfn "Part 1: %d" sum
