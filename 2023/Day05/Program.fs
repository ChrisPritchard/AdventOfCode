let input = Input.value

let splitOn (a: char[]) (s: System.String) = s.Split (a, System.StringSplitOptions.RemoveEmptyEntries)

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

let steps = [|seeds_to_soil;soil_to_fertilizer;fertilizer_to_water;water_to_light;light_to_temperature;temperature_to_humidity;humidity_to_location|]

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

// dest source length
// start with num, find range where num sits (assume no overlap), translate