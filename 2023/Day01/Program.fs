
let input = System.IO.File.ReadAllLines "input.txt"

let mutable sum = 0;

for line in input do
    let digits = line.ToCharArray() |> Array.filter System.Char.IsAsciiDigit
    let s = sprintf "%c%c" digits[0] digits[digits.Length - 1]
    sum <- sum + int32 s

printfn "Part 1: %A" sum

sum <- 0;

let words = [|("one","1");("two","2");("three","3");("four","4");("five","5");("six","6");("seven","7");("eight","8");("nine","9")|]

let rec replaceNumbers (line: string) = 
    let firstWord = words |> Array.choose (fun (w, n) -> let index = line.IndexOf w in if index >= 0 then Some (index, (w, n)) else None)
    if Array.isEmpty firstWord then
        line
    else
        let (pos, (w, n)) = firstWord |> Array.minBy fst
        let pre = line[..pos] + n
        pre + replaceNumbers line[pos+1..]

for line in input do
    let line = replaceNumbers line
    let digits = line.ToCharArray() |> Array.filter System.Char.IsAsciiDigit
    let s = sprintf "%c%c" digits[0] digits[digits.Length - 1]
    sum <- sum + int32 s

printfn "Part 2: %A" sum