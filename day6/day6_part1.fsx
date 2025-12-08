let input = System.IO.File.ReadAllLines "input.txt"

let operationsLine =
    input[input.Length - 1].Split ' '
    |> Array.filter (System.String.IsNullOrWhiteSpace >> not)

let numberLines = input |> Array.removeAt (input.Length - 1)

// Transpose the input while parsing the numbers in each column
let transposeToNumbers (matrix: string array) =
    let trimmedLines =
        matrix
        |> Array.map (
            _.Split(' ')
            >> Array.filter (System.String.IsNullOrWhiteSpace >> not)
        )

    let numCols = trimmedLines[0].Length
    let numRows = trimmedLines.Length
    Seq.init numCols (fun colIdx -> Seq.init numRows (fun rowIdx -> uint64 (trimmedLines[rowIdx][colIdx])))

let transposedInput = transposeToNumbers numberLines

// Compute the final result by matching the corresponding numbers of a column to its operation
let result =
    transposedInput
    |> Seq.mapi (fun colIdx operands ->
        let operation = operationsLine[colIdx]

        match operation with
        | "+" -> Seq.sum operands
        | "*" -> Seq.fold (*) 1UL operands
        | _ -> failwithf "Unknown operation: %s" operation)
    |> Seq.sum

printfn "%d" result
