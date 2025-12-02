let ranges =
    System.IO.File.ReadAllText "input.txt"
    |> _.Split(',')
    |> Seq.map (fun range ->
        let parts = range.Split '-'
        parts[0], parts[1])

let invalidIds =
    ranges
    |> Seq.collect (fun (startStr, endStr) ->
        let startId = uint64 startStr
        let endId = uint64 endStr

        [ startId..endId ]
        |> Seq.filter (fun id ->
            let idStr = id.ToString()
            let len = idStr.Length

            if len % 2 = 0 then
                let halfLen = len / 2
                let firstHalf = idStr.Substring(0, halfLen)
                let secondHalf = idStr.Substring(halfLen, halfLen)
                firstHalf = secondHalf
            else
                false))

printfn "%d" (Seq.sum invalidIds)
