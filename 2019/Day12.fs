module Day12

open Common
open System.IO
open SDL
open System
open System.Runtime.InteropServices

let input = 
    File.ReadAllLines ("./inputs/day12.txt") 
    |> Array.map (fun line -> 
        let parts = split "<x=, yz>" line
        (int parts.[0], int parts.[1], int parts.[2]))

let moons = input |> Array.map (fun p -> p, (0, 0, 0))

let timeStep moons =
    let appliedGravity = 
        moons
        |> Array.map (fun ((x, y, z), v) ->
            let nv = 
                (v, moons) ||> Array.fold (fun (vx, vy, vz) ((ox, oy, oz), _) ->
                    (if ox > x then vx + 1 elif ox < x then vx - 1 else vx),
                    (if oy > y then vy + 1 elif oy < y then vy - 1 else vy),
                    (if oz > z then vz + 1 elif oz < z then vz - 1 else vz))
            (x, y, z), nv)
        
    appliedGravity
    |> Array.map (fun ((x, y, z), (vx, vy, vz)) ->
        (x + vx, y + vy, z + vz), (vx, vy, vz))

let energy moons = 
    moons 
    |> Array.sumBy (fun ((x, y, z), (vx, vy, vz)) -> 
        (abs x + abs y + abs z) * (abs vx + abs vy + abs vz))

let part1 () =    

    let rec runSteps rem moons =
        if rem = 0 then moons
        else
            runSteps (rem - 1) (timeStep moons)

    runSteps 1000 moons |> energy

let part2 () =

    let viewWidth, viewHeight = 1800, 1000
    let asUint32 (r, g, b) = BitConverter.ToUInt32 (ReadOnlySpan [|b; g; r; 255uy|])
    
    let moonColours = 
        [|
            asUint32 (255uy, 0uy, 0uy)
            asUint32 (0uy, 255uy, 0uy)
            asUint32 (0uy, 0uy, 255uy)
            asUint32 (255uy, 255uy, 255uy)
        |]
    let black = asUint32 (0uy, 0uy, 0uy)

    SDL_Init(SDL_INIT_VIDEO) |> ignore

    let mutable window, renderer = IntPtr.Zero, IntPtr.Zero
    let windowFlags = SDL_WindowFlags.SDL_WINDOW_SHOWN ||| SDL_WindowFlags.SDL_WINDOW_INPUT_FOCUS
    SDL_CreateWindowAndRenderer(viewWidth, viewHeight, windowFlags, &window, &renderer) |> ignore

    let texture = SDL_CreateTexture(renderer, SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING, viewWidth, viewHeight)
    let frameBuffer = Array.create (viewWidth * viewHeight) black
    let bufferPtr = IntPtr ((Marshal.UnsafeAddrOfPinnedArrayElement (frameBuffer, 0)).ToPointer ())
    let mutable keyEvent = Unchecked.defaultof<SDL_KeyboardEvent>

    let drawRect x y w h colour =
        for dy = y to y + h - 1 do
            let pos = (dy * viewWidth) + x
            Array.fill frameBuffer pos w colour

    let rec drawLoop moons =

        let moons = timeStep moons
        drawRect 0 0 viewWidth viewHeight black
        moons |> Array.iteri (fun i ((x, y, _), _) ->
            drawRect (x + viewWidth/2 - 1) (y + viewHeight/2 - 1) 3 3 moonColours.[i])

        SDL_UpdateTexture(texture, IntPtr.Zero, bufferPtr, viewWidth * 4) |> ignore
        SDL_RenderClear(renderer) |> ignore
        SDL_RenderCopy(renderer, texture, IntPtr.Zero, IntPtr.Zero) |> ignore
        SDL_RenderPresent(renderer) |> ignore

        if SDL_PollEvent(&keyEvent) = 0 || (keyEvent.``type`` <> SDL_KEYDOWN && keyEvent.``type`` <> SDL_KEYUP) then
            SDL_Delay 20ul
            drawLoop moons
        else if keyEvent.keysym.sym = SDLK_ESCAPE then 
            ()

    drawLoop moons

    SDL_DestroyTexture(texture)
    SDL_DestroyRenderer(renderer)
    SDL_DestroyWindow(window)
    SDL_Quit()

    0
    