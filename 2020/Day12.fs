module Day12

open System
open System.IO

let input = File.ReadAllLines "./inputs/day12.txt"

let processed () = 
    input
    |> Array.map (fun l -> l.[0], float l.[1..])

let atAngle degrees dist =
     let dy = dist * Math.Sin (float degrees * Math.PI/180.)
     let dx = dist * Math.Cos (float degrees * Math.PI/180.)
     dx, dy

let part1 () =
    let (x, y, _) =
        ((0., 0., 0.), processed ())
        ||> Array.fold (fun (x, y, d) (i, m) ->
            match i with
            | 'L' -> x, y, d - m
            | 'R' -> x, y, d + m
            | 'N' -> x, y - m, d
            | 'S' -> x, y + m, d
            | 'E' -> x + m, y, d
            | 'W' -> x - m, y, d
            | _ -> 
                let dx, dy = atAngle d m
                x + dx, y + dy, d)
    int (abs x + abs y)

let part2 () =
    let angleAt x y = Math.Atan2 (y, x) * (180./Math.PI)
    let h o a = Math.Sqrt (o ** 2. + a ** 2.)
    let (x, y, _, _) =
        ((0., 0., 10., -1.), processed ())
        ||> Array.fold (fun (sx, sy, x, y) (i, m) ->
            match i with
            | 'N' -> sx, sy, x, y - m
            | 'S' -> sx, sy, x, y + m
            | 'E' -> sx, sy, x + m, y
            | 'W' -> sx, sy, x - m, y
            | 'L' -> 
                let d = angleAt x y - m
                let x, y = atAngle d (h x y)
                sx, sy, x, y
            | 'R' -> 
                let d = angleAt x y + m
                let x, y = atAngle d (h x y)
                sx, sy, x, y
            | _ -> 
                sx + m * x, sy + m * y, x, y)
    int (abs x + abs y)

// initially I got the wrong answer for part 2, and as part of debugging, I rendered the waypoint
// using SDL2, as below:

(*

open SDL
open System.Runtime.InteropServices

let part2 () =

    SDL_Init(SDL_INIT_VIDEO) |> ignore

    let mutable window, renderer = IntPtr.Zero, IntPtr.Zero
    let windowFlags = SDL_WindowFlags.SDL_WINDOW_SHOWN ||| SDL_WindowFlags.SDL_WINDOW_INPUT_FOCUS
    SDL_CreateWindowAndRenderer(1000, 1000, windowFlags, &window, &renderer) |> ignore

    let asUint32 (r, g, b) = BitConverter.ToUInt32 (ReadOnlySpan [|b; g; r; 255uy|])
    let black = asUint32 (0uy, 0uy, 0uy)
    let white = asUint32 (255uy, 255uy, 255uy)
    let red = asUint32 (255uy, 0uy, 0uy)
    let texture = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING, 1000, 1000)
    let frameBuffer = Array.create (1000 * 1000) black
    let bufferPtr = IntPtr ((Marshal.UnsafeAddrOfPinnedArrayElement (frameBuffer, 0)).ToPointer ())
    let mutable keyEvent = Unchecked.defaultof<SDL_KeyboardEvent>

    let drawRect x y w h colour =
        for dy = y to y + h - 1 do
            let pos = (dy * 1000) + x
            Array.fill frameBuffer pos w colour

    let (x, y, _, _) =
        ((0., 0., 10., -1.), processed ())
        ||> Array.fold (fun (sx, sy, x, y) (i, m) ->
            
            printfn "%c %f" i m
            let (sx, sy, x, y) = 
                match i with
                | 'N' -> sx, sy, x, y - m
                | 'S' -> sx, sy, x, y + m
                | 'E' -> sx, sy, x + m, y
                | 'W' -> sx, sy, x - m, y
                | 'L' -> 
                    let d1 = Math.Atan2 (y, x) * (180./Math.PI)
                    //let d1 = if d1 < 0. then 180. + d1 else d1
                    let d2 = d1 - m
                    printfn "%f -> %f" d1 d2
                    let m = Math.Sqrt (x ** 2. + y ** 2.)
                    let y = m * Math.Sin (float d2 * Math.PI/180.)
                    let x = m * Math.Cos (float d2 * Math.PI/180.)
                    sx, sy, x, y
                | 'R' -> 
                    let d1 = Math.Atan2 (y, x) * (180./Math.PI)
                    //let d1 = if d1 < 0. then 180. + d1 else d1
                    let d2 = d1 + m
                    printfn "%f -> %f" d1 d2
                    let m = Math.Sqrt (x ** 2. + y ** 2.)
                    let y = m * Math.Sin (float d2 * Math.PI/180.)
                    let x = m * Math.Cos (float d2 * Math.PI/180.)
                    sx, sy, x, y
                | _ -> 
                    sx + m * x, sy + m * y, x, y

            drawRect 0 0 1000 1000 black
            drawRect 500 500 2 2 white
            drawRect (500 + (int x * 10)) (500 + (int y * 10)) 2 2 red

            SDL_UpdateTexture(texture, IntPtr.Zero, bufferPtr, 1000 * 4) |> ignore
            SDL_RenderClear(renderer) |> ignore
            SDL_RenderCopy(renderer, texture, IntPtr.Zero, IntPtr.Zero) |> ignore
            SDL_RenderPresent(renderer) |> ignore
            System.Threading.Thread.Sleep 3000
            sx, sy, x, y)
    int (abs x + abs y)    
*)