let input = System.IO.File.ReadAllLines "input.txt"

let connections = 
    input |> Array.collect (fun line -> 
        let parts = line.Split (": ".ToCharArray(), System.StringSplitOptions.RemoveEmptyEntries)
        Array.append [|parts[0], parts[1..]|] (parts[1..] |> Array.map (fun op -> op, [|parts[0]|])))
    |> Array.groupBy fst
    |> Array.map (fun (k, a) -> 
        let connected = a |> Array.collect (snd >> id) |> Array.distinct
        k, connected)

printfn "%A" connections

let tarjan = 
    let mutable index = 0
    let mutable SCC = []
    let mutable S = []

    let E = connections |> Map.ofArray
    let V = connections |> Array.map fst
    let indexes = Array.create V.Length -1
    let low_link = Array.create V.Length -1
    let on_stack = Array.create V.Length false

    let rec strong_connect iv = 
        indexes[iv] <- index
        low_link[iv] <- index
        index <- index + 1
        S <- V[iv]::S
        on_stack[iv] <- true

        for w in E[V[iv]] do
            let iw = Array.findIndex ((=) w) V
            if indexes[iw] = -1 then
                strong_connect iw
                low_link[iv] <- min low_link[iv] low_link[iw]
            else if on_stack[iw] then
                low_link[iv] <- min low_link[iv] indexes[iw]

        if low_link[iv] = indexes[iv] then
            let mutable new_scc = []
            let mutable cont = true
            while not (List.isEmpty S) && cont do
                let w = List.head S
                let iw = Array.findIndex ((=) w) V
                on_stack[iw] <- false
                new_scc <- w::new_scc
                S <- List.tail S
                if w = V[iv] then cont <- false
            SCC <- (List.toArray new_scc)::SCC

    for iv in 0..V.Length - 1 do
        if indexes[iv] = -1 then
            strong_connect iv

    List.toArray SCC

printfn "%A" tarjan
