module DayTwo

open System.IO

let sumValidGameIds(filePathRoot: string) = 

    // Game 1: 1 green, 4 blue; 1 blue, 2 green, 1 red; 1 red, 1 green, 2 blue; 1 green, 1 red; 1 green; 1 green, 1 blue, 1 red
    let redMax = 12
    let greenMax = 13
    let blueMax = 14
    
    let selectionIsValid(selection: string) : bool =
        // A selection is '1 green, 4 blue'
        // or '1 blue'
        // or '1 red, 1 green, 2 blue'
        selection.Split(",")
            |> Array.toSeq // to allow for lazy eval
            |> Seq.map(fun colourCountAsString -> colourCountAsString.Trim())
            |> Seq.map(fun colourCountAsString -> 
                    let count = int(colourCountAsString.[..colourCountAsString.Trim().IndexOf(' ')])
                    match colourCountAsString with
                        | _ when colourCountAsString.EndsWith("red") -> count <= redMax
                        | _ when colourCountAsString.EndsWith("green") -> count <= greenMax
                        | _ when colourCountAsString.EndsWith("blue") -> count <= blueMax
                        | _ -> false
                )
            |> Seq.forall(fun countIsValid -> countIsValid)


    // Run through each line in the input. and accumulate a score.
    let foldLine(input: string) : int = 
        
        // Parse the sequence into the selections
        let sequenceStart = input.IndexOf(':') + 2
        let sequenceAsString = input.[sequenceStart..] 
        let allSectionsAreValid = 
            sequenceAsString.Split(";")
            |> Array.toSeq // To sequence to allow for lazy evaluation.
            |> Seq.map(fun selection -> selectionIsValid(selection))
            |> Seq.forall(fun validSelection -> validSelection) // If any values are false, return.
        
        if (not allSectionsAreValid) then
            0
        else
        
        let gameId = int input.[input.IndexOf(' ') + 1..sequenceStart - 3]
        gameId

    let score = 
        File.ReadAllLines($"{filePathRoot}/DayTwo.txt") 
        |> Array.fold (fun accumulator line -> accumulator + foldLine(line)) (0)

    printf($"{score}")


type maxColourTracker = 
    {
        maxRed: int
        maxBlue: int
        maxGreen: int
    }

let sumMinimumPowerScores (filePathRoot: string) = 

    let foldColourCount (maxColourTracker: maxColourTracker) (colourCountAsString: string) : maxColourTracker =
        let count = int(colourCountAsString.[..colourCountAsString.Trim().IndexOf(' ')])
        if colourCountAsString.EndsWith("red") && count > maxColourTracker.maxRed then
            { maxColourTracker with maxRed = count }
        elif colourCountAsString.EndsWith("green") && count > maxColourTracker.maxGreen then
            { maxColourTracker with maxGreen = count }
        elif colourCountAsString.EndsWith("blue") && count > maxColourTracker.maxBlue then
            { maxColourTracker with maxBlue = count }
        else
            maxColourTracker 
        
    let foldSelection(maxColourTracker: maxColourTracker) (line: string) : maxColourTracker =

        // A selection is '1 green, 4 blue'
        // or '1 blue'
        // or '1 red, 1 green, 2 blue'
        line.Split(",")
            |> Array.map(fun colourCountAsString -> colourCountAsString.Trim())
            |> Array.fold(foldColourCount) (maxColourTracker)


    // Run through each line in the input. and accumulate a score.
    let foldLine(input: string) : int = 
        
        // Parse the sequence into the selections
        let sequenceStart = input.IndexOf(':') + 2
        let sequenceAsString = input.[sequenceStart..] 
        let powerScore : maxColourTracker = 
            sequenceAsString.Split(";")
            |> Array.fold (foldSelection) ( {maxRed = 0; maxBlue = 0; maxGreen = 0} )
        
        [|powerScore.maxBlue; powerScore.maxRed; powerScore.maxGreen |]
         |> Array.filter(fun x -> x <> 0)
         |> Array.reduce (fun a b -> a * b)

    let score = 
        File.ReadAllLines($"{filePathRoot}/DayTwo.txt") 
        |> Array.fold (fun accumulator line -> accumulator + foldLine(line)) (0)

    printf($"{score}")