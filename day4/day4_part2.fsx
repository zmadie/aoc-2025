let mutableGrid =
    System.IO.File.ReadAllLines "input.txt"
    |> Array.map _.ToCharArray()

let rows = mutableGrid.Length
let cols = mutableGrid[0].Length

let inline isRoll row col =
    row >= 0
    && row < rows
    && col >= 0
    && col < cols
    && mutableGrid[row][col] = '@'

let countAdjacentRolls row col =
    Seq.allPairs [ -1 .. 1 ] [ -1 .. 1 ]
    |> Seq.fold
        (fun count (directionRow, directionCol) ->
            if (directionRow <> 0 || directionCol <> 0)
               && isRoll (row + directionRow) (col + directionCol) then
                count + 1
            else
                count)
        0

let removeRolls () =
    let rec loop totalRemoved =
        let removedThisRound =
            Seq.allPairs [ 0 .. rows - 1 ] [
                0 .. cols - 1
            ]
            |> Seq.fold
                (fun count (row, col) ->
                    if mutableGrid[row][col] = '@'
                       && countAdjacentRolls row col < 4 then
                        mutableGrid[row][col] <- '.'
                        count + 1
                    else
                        count)
                0

        if removedThisRound = 0 then
            totalRemoved
        else
            loop (totalRemoved + removedThisRound)

    loop 0

let result = removeRolls ()

printfn "%d" result
