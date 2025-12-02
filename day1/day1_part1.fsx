let START_POSITION = 50
let UPPER_BOUND = 100

type Direction =
    | Left
    | Right

    static member Parse =
        function
        | 'L' -> Left
        | 'R' -> Right
        | char -> failwithf "Character '%c' is an invalid direction" char

let stepPosition currentPosition steps direction =
    match direction with
    | Left ->
        (currentPosition + UPPER_BOUND
         - steps % UPPER_BOUND) % UPPER_BOUND
    | Right -> (currentPosition + steps) % UPPER_BOUND


let instructions =
    System.IO.File.ReadAllLines "input.txt"
    |> Seq.map (fun line -> Direction.Parse line[0], int line[1..])

let (zeroesCount, _) =
    instructions
    |> Seq.fold
        (fun (currentCount, currentPosition) (direction, steps) ->
            let newPosition = stepPosition currentPosition steps direction
            currentCount + (if newPosition = 0 then 1 else 0), newPosition)
        (0, START_POSITION)

printfn "%d" zeroesCount
