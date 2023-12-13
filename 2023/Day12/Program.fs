let input = Input.value

let springs = 
    input.Split [|'\n'|] |> Array.map (fun line -> 
        let parts = line.Split [|' '|]

        parts[0].ToCharArray (), (parts[1].Split [|','|] |> Array.map int))

printfn "%A" <| (springs |> Array.maxBy (fst >> Array.length) |> fst)

// max length is 20, which is covered within a 32bit number
let test1 = 0b011101100001
let test2 = 0b111010100001

let integrity = [|3;2;1|]

// to test a number is valid, it must match the mask
let mask = 0b011100000000
printfn "test 1 %d passes the mask: %b" test1 (mask &&& test1 = mask)
printfn "test 2 %d passes the mask: %b" test2 (mask &&& test2 = mask)

// right shift chops off the end
printfn "%d ends with 1: %b" 0b101001 (1 &&& 0b101001 = 1)
printfn "%d ends with 1: %b" 0b101000 (1 &&& 0b101000 = 1)

// testing integrity
// 1. take final val in integrity
// 2. create mask for it: 1 <<< +1 <<< +1
// 3. test if number matches this, if not, <<<
// 4a. if end of num and no match, false
// 4b. if match, >>> num
// 5. if integrity.length > 1 then test at least one gap
// 6. repeat until 5 is false

let rec test_integrity number integrity =
    if Array.length integrity = 0 && number = 0 then true
    else if integrity.Length = 0 || number = 0 then false
    else
        let mutable to_test = integrity[integrity.Length - 1]
        while to_test < number && to_test &&& number <> to_test do
            to_test <- to_test <<< 1
        if to_test > number then false
        else
            let trimmed = number >>> integrity[integrity.Length - 1]
            if integrity.Length > 1 && 1 &&& trimmed = 1 then false
            else
                test_integrity trimmed integrity[0..integrity.Length - 2]

printfn "test 1 %d passes integrity: %b" test1 (test_integrity test1 integrity)
printfn "test 2 %d passes integrity: %b" test2 (test_integrity test2 integrity)