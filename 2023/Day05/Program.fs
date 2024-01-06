let input = System.IO.File.ReadAllText "input.txt"

let splitOn (a: char[]) (s: System.String) = s.Split (a, System.StringSplitOptions.RemoveEmptyEntries)

// dest source length
let numbersFrom s = splitOn [|' '|] s |> Array.map int64
let conversionMap s = 
    splitOn [|'\n'|] s
    |> Array.filter (fun s -> s.Length > 0 && System.Char.IsAsciiDigit s[0])
    |> Array.map (fun line -> let ns = numbersFrom line in ns[0], ns[1], ns[2])

let parts = splitOn [|':'|] input |> Array.skip 1 |> Array.map (fun s -> s.Trim())

let mutable seeds = parts[0] |> splitOn [|'\n'|] |> Array.head |> numbersFrom

let seeds_to_soil = conversionMap parts[1]
let soil_to_fertilizer = conversionMap parts[2]
let fertilizer_to_water = conversionMap parts[3]
let water_to_light = conversionMap parts[4]
let light_to_temperature = conversionMap parts[5]
let temperature_to_humidity = conversionMap parts[6]
let humidity_to_location = conversionMap parts[7]

let steps = [|seeds_to_soil; soil_to_fertilizer; fertilizer_to_water; water_to_light; light_to_temperature; temperature_to_humidity; humidity_to_location|]

for step in steps do
    seeds <- seeds |> Array.map (fun seed -> 
        let next = step |> Array.tryPick (fun (dest, source, range) -> 
                if seed >= source && seed < source + range then
                    Some (dest + (seed - source))
                else
                    None)
        match next with Some n -> n | _ -> seed)

let smallest = Array.min seeds
printfn "Part 1: %d" smallest

let test_range_against (r1, r2) (dest, source, length) =
    if r1 >= source && r2 < source + length then
        [|dest + (r1 - source), dest + (r2 - source)|], [||]
    else if r1 < source && r2 >= source && r2 < source + length then
        [|dest, dest + (r2 - source)|], [|r1, source - 1L|]
    else if r1 >= source && r1 < source + length && r2 >= source + length then
        [|dest + (r1 - source), dest + length - 1L|],[|source + length, r2|]
    else if r1 < source && r2 >= source + length then
        [|dest, dest + length - 1L|],[|r1, source - 1L; source + length, r2|]
    else
        [||], [|r1, r2|]

let mutable seed_ranges = parts[0] |> splitOn [|'\n'|] |> Array.head |> numbersFrom |> Array.chunkBySize 2 |> Array.map (fun a -> a[0], a[0] + a[1] - 1L)

// whats the flaw here? we have a range, x-y, and a given transform might extract a chunk of that for the next layer.
// the remainder from the original range can be extracted by additional transforms - the issue is that presumably the other transforms wont touch the extracted chunk but...
// because the extracted chunk is still presented to those additional transforms, it ends up returned in its original form
// so to fix this, as the seed range is tested if there are new chunks, they are removed from further consideration and only the remainders continue to be compared

for step in steps do
    seed_ranges <- seed_ranges |> Array.collect (fun seed_range ->
        let mutable ranges_to_test = [|seed_range|]
        let mutable all_new_ranges = Array.empty
        for step_range in step do
            for given_range in ranges_to_test do
                let (new_ranges, leftovers) = test_range_against given_range step_range
                all_new_ranges <- Array.append all_new_ranges new_ranges
                ranges_to_test <- leftovers
        Array.append all_new_ranges ranges_to_test)

let smallest_from_ranges = seed_ranges |> Array.minBy fst |> fst

printfn "Part 2: %d" smallest_from_ranges

// // testing logic

// let range = (10L, 19L)
// printfn "test 1: %A" <| test_range_against range (100L, 5L, 20L) // result should be 105-114 
// printfn "test 2: %A" <| test_range_against range (100L, 15, 10L) // result should be 10-14, 100-104
// printfn "test 3: %A" <| test_range_against range (100L, 5, 10L) // result should be 105-109, 15-19 
// printfn "test 4: %A" <| test_range_against range (100L, 15, 2) // result should be 10-14, 100-101, 17-19
// printfn "test 5: %A" <| test_range_against range (100L, 0, 5L) // should be blank
