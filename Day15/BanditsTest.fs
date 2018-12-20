module BanditsTest

open Bandits
open FsUnit.Xunit
open Xunit

[<Fact>]
let ``Part 1 First example`` () =
    let map = [|
        "#######"   
        "#.G...#"
        "#...EG#"
        "#.#.#G#"
        "#..G#E#"
        "#.....#"
        "#######"
    |]

    let expected = [|
        "#######"   
        "#G....#"
        "#.G...#"
        "#.#.#G#"
        "#...#.#"
        "#....G#"
        "#######"
    |]

    match runGame map 3 false with
    | Victory (result, turn, health) ->
        result |> should equal expected 
        turn |> should equal 47
        health |> should equal 590
    | _ -> failwith "Unexpected result"

[<Fact>]
let ``Part 1 Second example`` () =
    let map = [|
        "#######"
        "#G..#E#"
        "#E#E.E#"
        "#G.##.#"
        "#...#E#"
        "#...E.#"
        "#######"
    |]

    let expected = [|
        "#######"   
        "#...#E#"
        "#E#...#"
        "#.E##.#"
        "#E..#E#"
        "#.....#"
        "#######"
    |]

    match runGame map 3 false with
    | Victory (result, turn, health) ->
        result |> should equal expected 
        turn |> should equal 37
        health |> should equal 982
    | _ -> failwith "Unexpected result"

[<Fact>]
let ``Part 1 Third example`` () =
    let map = [|
        "#######"
        "#E..EG#"
        "#.#G.E#"
        "#E.##E#"
        "#G..#.#"
        "#..E#.#"
        "#######"
    |]

    let expected = [|
        "#######"  
        "#.E.E.#"
        "#.#E..#"
        "#E.##.#"
        "#.E.#.#"
        "#...#.#"  
        "#######"  
    |]

    match runGame map 3 false with
    | Victory (result, turn, health) ->
        result |> should equal expected 
        turn |> should equal 46
        health |> should equal 859
    | _ -> failwith "Unexpected result"

[<Fact>]
let ``Part 1 Fourth example`` () =
    let map = [|
        "#######"
        "#E.G#.#"
        "#.#G..#"
        "#G.#.G#"
        "#G..#.#"
        "#...E.#"
        "#######"
    |]

    let expected = [|
        "#######"   
        "#G.G#.#"
        "#.#G..#"
        "#..#..#"
        "#...#G#"
        "#...G.#"
        "#######"
    |]

    match runGame map 3 false with
    | Victory (result, turn, health) ->
        result |> should equal expected 
        turn |> should equal 35
        health |> should equal 793
    | _ -> failwith "Unexpected result"

[<Fact>]
let ``Part 1 Fifth example`` () =
    let map = [|
        "#######"   
        "#.E...#"
        "#.#..G#"
        "#.###.#"
        "#E#G#G#"
        "#...#G#"
        "#######"
    |]

    let expected = [|
        "#######"   
        "#.....#"
        "#.#G..#"
        "#.###.#"
        "#.#.#.#"
        "#G.G#G#"
        "#######"
    |]

    match runGame map 3 false with
    | Victory (result, turn, health) ->
        result |> should equal expected 
        turn |> should equal 54
        health |> should equal 536
    | _ -> failwith "Unexpected result"

[<Fact>]
let ``Part 1 Sixth example`` () =
    let map = [|
        "#########"
        "#G......#"
        "#.E.#...#"
        "#..##..G#"
        "#...##..#"
        "#...#...#"
        "#.G...G.#"
        "#.....G.#"
        "#########"
    |]

    let expected = [|
        "#########"   
        "#.G.....#"
        "#G.G#...#"
        "#.G##...#"
        "#...##..#"
        "#.G.#...#"
        "#.......#"
        "#.......#"
        "#########" 
    |]

    match runGame map 3 false with
    | Victory (result, turn, health) ->
        result |> should equal expected 
        turn |> should equal 20
        health |> should equal 937
    | _ -> failwith "Unexpected result"

[<Fact>]
let ``Part 2 First example`` () =
    let map = [|
        "#######"   
        "#.G...#"
        "#...EG#"
        "#.#.#G#"
        "#..G#E#"
        "#.....#"
        "#######"
    |]

    let expected = [|
        "#######"   
        "#..E..#"
        "#...E.#"
        "#.#.#.#"
        "#...#.#"
        "#.....#"
        "#######"
    |]

    match findMinElfAttack map with
    | elfAttack, Victory (result, turn, health) ->
        elfAttack |> should equal 15
        result |> should equal expected 
        turn |> should equal 29
        health |> should equal 172
    | _ -> failwith "Unexpected result"

[<Fact>]
let ``Part 2 Third example`` () =
    let map = [|
        "#######"
        "#E..EG#"
        "#.#G.E#"
        "#E.##E#"
        "#G..#.#"
        "#..E#.#"
        "#######"
    |]

    let expected = [|
        "#######"  
        "#.E.E.#"
        "#.#E..#"
        "#E.##E#"
        "#.E.#.#"
        "#...#.#"  
        "#######"  
    |]

    match findMinElfAttack map with
    | elfAttack, Victory (result, turn, health) ->
        elfAttack |> should equal 4
        result |> should equal expected 
        turn |> should equal 33
        health |> should equal 948
    | _ -> failwith "Unexpected result"

[<Fact>]
let ``Part 2 Fourth example`` () =
    let map = [|
        "#######"
        "#E.G#.#"
        "#.#G..#"
        "#G.#.G#"
        "#G..#.#"
        "#...E.#"
        "#######"
    |]

    let expected = [|
        "#######"   
        "#.E.#.#"
        "#.#E..#"
        "#..#..#"
        "#...#.#"
        "#.....#"
        "#######"
    |]

    match findMinElfAttack map with
    | elfAttack, Victory (result, turn, health) ->
        elfAttack |> should equal 15
        result |> should equal expected 
        turn |> should equal 37
        health |> should equal 94
    | _ -> failwith "Unexpected result"

[<Fact>]
let ``Part 2 Fifth example`` () =
    let map = [|
        "#######"   
        "#.E...#"
        "#.#..G#"
        "#.###.#"
        "#E#G#G#"
        "#...#G#"
        "#######"
    |]

    let expected = [|
        "#######"   
        "#...E.#"
        "#.#..E#"
        "#.###.#"
        "#.#.#.#"
        "#...#.#"
        "#######"
    |]

    match findMinElfAttack map with
    | elfAttack, Victory (result, turn, health) ->
        elfAttack |> should equal 12
        result |> should equal expected 
        turn |> should equal 39
        health |> should equal 166
    | _ -> failwith "Unexpected result"

[<Fact>]
let ``Part 2 Sixth example`` () =
    let map = [|
        "#########"
        "#G......#"
        "#.E.#...#"
        "#..##..G#"
        "#...##..#"
        "#...#...#"
        "#.G...G.#"
        "#.....G.#"
        "#########"
    |]

    let expected = [|
        "#########"   
        "#.......#"
        "#.E.#...#"
        "#..##...#"
        "#...##..#"
        "#...#...#"
        "#.......#"
        "#.......#"
        "#########" 
    |]

    match findMinElfAttack map with
    | elfAttack, Victory (result, turn, health) ->
        elfAttack |> should equal 34
        result |> should equal expected 
        turn |> should equal 30
        health |> should equal 38
    | _ -> failwith "Unexpected result"