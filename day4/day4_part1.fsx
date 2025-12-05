let grid = System.IO.File.ReadAllLines "input.txt"

let rows = grid.Length
let cols = grid[0].Length

let inline isRoll row col =
    row >= 0
    && row < rows
    && col >= 0
    && col < cols
    && grid[row][col] = '@'

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

let accessibleRolls =
    Seq.allPairs [ 0 .. rows - 1 ] [
        0 .. cols - 1
    ]
    |> Seq.fold
        (fun count (row, col) ->
            if grid[row][col] = '@'
               && countAdjacentRolls row col < 4 then
                count + 1
            else
                count)
        0

printfn "%d" accessibleRolls
