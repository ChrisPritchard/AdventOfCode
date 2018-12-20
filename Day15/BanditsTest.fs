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
    | _ -> failwith "Unexcepted loss"

[<Fact>]
let ``Part 2 Second example`` () =
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
    | _ -> failwith "Unexcepted loss"