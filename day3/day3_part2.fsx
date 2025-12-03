let banks =
    System.IO.File.ReadAllLines "input.txt"
    |> Seq.map (
        _.ToCharArray()
        >> Seq.map (System.Char.GetNumericValue >> int64)
    )

let result =
    banks
    |> Seq.map (fun bank ->
        let bankArr = bank |> Seq.toArray
        let n = bankArr.Length // Number of digits in the bank
        let k = 12 // Number of digits to select

        let _, result =
            [ 1..k ]
            |> Seq.fold
                (fun (pos, result: int64 list) _ ->
                    // Calculate search window
                    let remaining = k - result.Length
                    let searchWindow = n - pos - remaining + 1

                    // Find the max digit and its index in the search window
                    let maxIdx, maxDigit =
                        [ pos .. pos + searchWindow - 1 ]
                        |> Seq.map (fun i -> i, bankArr[i])
                        |> Seq.maxBy snd

                    (maxIdx + 1, maxDigit :: result))
                (0, [])

        result
        |> Seq.rev
        |> Seq.fold (fun acc d -> acc * 10L + d) 0L)

printfn "%d" (Seq.sum result)
