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

let stepPositionAndCountZeroes currentPosition steps direction =
    match direction with
    | Left ->
        // Extra logic to prevent a duplicate count when the current position is 0
        let numberOfZeroesPassed =
            (((UPPER_BOUND - (currentPosition % UPPER_BOUND)) % UPPER_BOUND)
             + steps)
            / UPPER_BOUND

        let newPosition =
            (currentPosition + UPPER_BOUND
             - steps % UPPER_BOUND) % UPPER_BOUND

        newPosition, numberOfZeroesPassed
    | Right ->
        let numberOfZeroesPassed =
            (currentPosition % UPPER_BOUND + steps)
            / UPPER_BOUND

        let newPosition = (currentPosition + steps) % UPPER_BOUND
        newPosition, numberOfZeroesPassed


let instructions =
    System.IO.File.ReadAllLines "input.txt"
    |> Seq.map (fun line -> Direction.Parse line[0], int line[1..])

let (zeroesCount, _) =
    instructions
    |> Seq.fold
        (fun (currentCount, currentPosition) (direction, steps) ->
            let newPosition, numberOfZeroesPassed =
                stepPositionAndCountZeroes currentPosition steps direction

            currentCount + numberOfZeroesPassed, newPosition)
        (0, START_POSITION)

printfn "%d" zeroesCount
