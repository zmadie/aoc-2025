open System

let input = IO.File.ReadAllLines "input.txt"

let operationsLine =
    input[ input.Length - 1 ].Split ' '
    |> Array.filter (String.IsNullOrWhiteSpace >> not)

let numberLines = input |> Array.removeAt (input.Length - 1)

// Transpose the input so the first dimension represents all the columns of the input
let transpose (matrix: string array) =
    let numCols = matrix[0].Length
    let numRows = matrix.Length

    Seq.init numCols (fun colIdx -> Seq.init numRows (fun rowIdx -> matrix[rowIdx][colIdx]))

let transposedInput = transpose numberLines

let parseOperandGroups (input: char seq seq) =
    // Separate groups of operands by empty columns
    let operandGroups, lastOperandGroup =
        input
        |> Seq.fold
            (fun (operandGroups, currentOperandGroup) currentColumn ->
                let isEmptyColumn = currentColumn |> Seq.forall ((=) ' ')

                if
                    isEmptyColumn
                    && not (Seq.isEmpty currentOperandGroup)
                then
                    (currentOperandGroup :: operandGroups, [])
                elif not isEmptyColumn then
                    let currentOperand = currentColumn |> Seq.toArray |> String |> uint64

                    (operandGroups, currentOperand :: currentOperandGroup)
                else
                    (operandGroups, currentOperandGroup))
            ([], [])

    // Can reverse the groups of operands here, or index the corresponding operations in reverse
    lastOperandGroup :: operandGroups |> Seq.rev

let operandGroups = parseOperandGroups transposedInput

// Compute the final result by matching the corresponding numbers of a column to its operation
let result =
    operandGroups
    |> Seq.mapi (fun columnIndex operands ->
        let operation = operationsLine[columnIndex]

        match operation with
        | "+" -> operands |> List.sum
        | "*" -> operands |> List.fold (*) 1UL
        | _ -> failwithf "Unknown operation: %s" operation)
    |> Seq.sum

printfn "%d" result
