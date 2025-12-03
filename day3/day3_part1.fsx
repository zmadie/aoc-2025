let banks =
    System.IO.File.ReadAllLines "input.txt"
    |> Seq.map (
        _.ToCharArray()
        >> Seq.map (System.Char.GetNumericValue >> int)
    )

let result =
    banks
    |> Seq.map (fun bank ->
        // Get the max value of the bank, which cannot be the last digit, and the second max value following it
        // let maxIndex, maxValue =
        //     bank
        //     |> Seq.take (Seq.length bank - 1)
        //     |> Seq.mapi (fun i v -> i, v)
        //     |> Seq.maxBy snd

        // let secondMax = bank |> Seq.skip (maxIndex + 1) |> Seq.max
        // let maxPair = maxValue * 10 + secondMax

        // Less cleaner, but single pass fold
        let maxPair, _, _ =
            bank
            |> Seq.fold
                (fun (maxPair, maxFirstDigit, maxSecondDigit) digit ->
                    let newMaxPair = max maxPair (maxFirstDigit * 10 + digit)
                    let newMaxSecondDigit = max maxSecondDigit digit

                    if digit > maxFirstDigit then
                        (newMaxPair, digit, 0)
                    else
                        (newMaxPair, maxFirstDigit, newMaxSecondDigit))
                (0, 0, 0)

        maxPair)

printfn "%d" (Seq.sum result)
