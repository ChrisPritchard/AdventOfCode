module BanditsTest

open Bandits
open FsUnit.Xunit
open Xunit

[<Fact>]
let ``First example`` () =
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