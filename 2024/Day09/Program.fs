let input = System.IO.File.ReadAllLines "input.txt"

printfn "%A %d" (input[0] |> Seq.sumBy int) input[0].Length

// too big to make an array and do manually... or is it... million strong anyway
// but len is only 19999 with every % 2 num a file, so possible to fold calculate

// three tracked values: position in input, reverse file being considered, current index of output

// certain values can be calculated, e.g. the position in the output would be the sum of inputs to date.