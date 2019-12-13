module Day13

open Common
open System.IO
open System.Collections.Generic
open SDL
open System
open System.Runtime.InteropServices

// type specification : this allows the system to be switched from int to int64 to bigint or float if necessary

type T = int64
type TQueue = Queue<T>
let stot (o: string) = int64 o
let itot (o: int) = int64 o

let t1m, t0, t1, t2, t3, t4, t100, t1000, t10000, t100000 = 
    itot -1, itot 0, itot 1, itot 2, itot 3, itot 4, 
    itot 100, itot 1000, itot 10000, itot 100000

let input = (File.ReadAllText ("./inputs/day13.txt")).Split ',' |> Array.map stot

// generic intcode vm code

let read (queue: TQueue) = 
    fun () -> if queue.Count > 0 then true, queue.Dequeue () else false, t0
    
type State = Running | Halted | Blocked

let intcodeRun opcodes ip rb (memory: Dictionary<T, T>) read write =
    let parseOp code = 
        let op = code % t100
        op, [|code % t1000 / t100; code % t10000 / t1000; code % t100000 / t10000|]
    let rec processor ip rb =
        let opcode, modes = parseOp memory.[ip]
        let value i = if memory.ContainsKey i then memory.[i] else t0
        let v1 () = 
            let v = memory.[ip + t1] 
            if modes.[0] = t0 then value v
            elif modes.[0] = t2 then value (rb + v)
            else v
        let v2 () = 
            let v = memory.[ip + t2] 
            if modes.[1] = t0 then value v
            elif modes.[1] = t2 then value (rb + v)
            else v
        let set i v = 
            let o = memory.[ip + i]
            let sidx = if modes.[int i - 1] = t2 then rb + o else o
            memory.[sidx] <- v
        let op = Map.find opcode opcodes
        let nextIp, nextRb, nextState = op ip rb v1 v2 set read write
        if nextState = Running then processor nextIp nextRb else nextState, nextIp, nextRb
    let finalState, finalIp, finalRb = processor ip rb
    finalState, finalIp, finalRb, memory

let ops = Map.ofList [
    itot 99, (fun ip rb _ _ _ _ _ -> // halt
        ip, rb, Halted)

    itot 1, (fun ip rb v1 v2 set _ _ -> // Add
        v1() + v2() |> set t3
        ip + t4, rb, Running)

    itot 2, (fun ip rb v1 v2 set _ _ -> // Mul
        v1() * v2() |> set t3
        ip + t4, rb, Running)

    itot 3, (fun ip rb _ _ set read _-> // Read or block
        let (canRead, value) = read ()
        if canRead then
            set t1 value
            ip + t2, rb, Running
        else
            ip, rb, Blocked)

    itot 4, (fun ip rb v1 _ _ _ write -> // Write
       write <| v1()
       ip + t2, rb, Running)

    itot 5, (fun ip rb v1 v2 _ _ _ -> // jump if greater
        let nextIp = if v1() > t0 then v2() else ip + t3
        nextIp, rb, Running)

    itot 6, (fun ip rb v1 v2 _ _ _ -> // jump if equal
        let nextIp = if v1() = t0 then v2() else ip + t3
        nextIp, rb, Running)

    itot 7, (fun ip rb v1 v2 set _ _ -> // set if less
        set t3 <| if v1() < v2() then t1 else t0
        ip + t4, rb, Running)

    itot 8, (fun ip rb v1 v2 set _ _ -> // set if equal
        set t3 <| if v1() = v2() then t1 else t0
        ip + t4, rb, Running)

    itot 9, (fun ip rb v1 _ _ _ _ -> // alter relative base
        ip + t2, rb + v1(), Running)
    ]

// painter code for day 11

let inputStream = TQueue()
let outputStream = TQueue()

// individual parts

let part1 () =    
    
    let mem =
        Array.indexed input 
        |> Array.map (fun (k, v) -> itot k, v) 
        |> dict 
        |> Dictionary<T, T>
    
    let res = intcodeRun ops t0 t0 mem (read inputStream) (outputStream.Enqueue)

    Seq.chunkBySize 3 outputStream
    |> Seq.map Seq.toArray
    |> Seq.filter (fun a -> a.[2] = t2)
    |> Seq.length

let part2 () =

    let startMem =
        Array.indexed input 
        |> Array.map (fun (k, v) -> itot k, v) 
        |> dict 
        |> Dictionary<T, T>

    startMem.[t0] <- t2
    
    let viewWidth, viewHeight = 1800, 1000
    let asUint32 (r, g, b) = BitConverter.ToUInt32 (ReadOnlySpan [|b; g; r; 255uy|])
    
    let colours = 
        [|
            asUint32 (0uy, 0uy, 0uy)
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

    let mutable score = 0

    let rec drawLoop ip rb mem =

        let (state, ip, rb, mem) = intcodeRun ops ip rb mem (read inputStream) (outputStream.Enqueue)
        if state = Halted then 
            printfn "Score: %i" score
            drawLoop t0 t0 startMem
            ()
        else
            drawRect 0 0 viewWidth viewHeight black
            Seq.chunkBySize 3 outputStream
            |> Seq.map Seq.toArray
            |> Seq.iter (fun a ->
                if a.[0] = t1m && a.[1] = t0 then score <- int a.[2]
                else drawRect (int a.[0] * 30) (int a.[1] * 30) 30 30 colours.[int a.[2]])
        
            SDL_UpdateTexture(texture, IntPtr.Zero, bufferPtr, viewWidth * 4) |> ignore
            SDL_RenderClear(renderer) |> ignore
            SDL_RenderCopy(renderer, texture, IntPtr.Zero, IntPtr.Zero) |> ignore
            SDL_RenderPresent(renderer) |> ignore
        
            inputStream.Enqueue t1m
            drawLoop ip rb mem
            //if SDL_PollEvent(&keyEvent) = 0 || (keyEvent.``type`` <> SDL_KEYDOWN && keyEvent.``type`` <> SDL_KEYUP) then
            //    //SDL_Delay 50ul
            //    drawLoop ip rb mem
            //else if keyEvent.keysym.sym = SDLK_ESCAPE then 
            //    () // quit the game by exiting the loop
            //else
            //    match keyEvent.keysym.sym with
            //    | c when c = uint32 'a' -> inputStream.Enqueue t1m
            //    | c when c = uint32 's' -> inputStream.Enqueue t0
            //    | c when c = uint32 'd' -> inputStream.Enqueue t1
            //    | _ -> ()
            //    //SDL_Delay 50ul
            //    drawLoop ip rb mem

    drawLoop t0 t0 startMem

    SDL_DestroyTexture(texture)
    SDL_DestroyRenderer(renderer)
    SDL_DestroyWindow(window)
    SDL_Quit()

    0