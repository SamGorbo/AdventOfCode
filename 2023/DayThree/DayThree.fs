module DayThree
open System.IO
open System.Text.RegularExpressions
open System

let PartOne_sumPartNumbers() =
    
    let input = File.ReadAllLines("./2023/DayThree/DayThree.txt")

    // Loop through row by row, and identify spans of numbers.
    // For that span of numbers, from index X_1 to X_2, 
    // look at the 'border' of the number.
    // ie: Locations without duplicates
    //      [X_1, X_2][Y - 1], (TOP ROW)
    //      [X_1 - 1][Y - 1, Y + 1], (LEFT COLUMN)
    //      [X_2 + 1][Y - 1, Y + 1], (RIGHT COLUMN)
    //      [X_1, X_2][Y + 1], (BOTTOM ROW)
    
    let getMatchValue(index: int)(line: string)(matchStart: int)(matchEnd: int) = 
        let topRow = 
            if index <> 0 then
                seq { for character in input[index - 1][matchStart..matchEnd] -> character} // top row
            else 
                Seq.empty

        let leftColumn = 
            if matchStart <> 0 then
                let minY = max(index - 1)(0)
                let maxY = min(index + 1)(input.Length - 1)
                seq { 
                    for yCoOrd in minY .. maxY -> input[yCoOrd][matchStart - 1] 
                } // left column
            else 
                Seq.empty
        
        let rightColumn = 
            if matchEnd < line.Length - 1 then
                let minY = max(index - 1)(0)
                let maxY = min(index + 1)(input.Length - 1)
                seq { 
                    for yCoOrd in minY .. maxY -> input[yCoOrd][matchEnd + 1]
                } // right column    
            else 
                Seq.empty

        let bottomRow =
            if index < input.Length - 1 then
                seq { for character in input[index + 1][matchStart..matchEnd] -> character} // top row
            else 
                Seq.empty
        
        let borderDoesNotContainPunctuation = 
            topRow
            |> Seq.append leftColumn
            |> Seq.append rightColumn
            |> Seq.append bottomRow
            |> Seq.forall (fun x -> x.Equals('.') || Char.IsDigit(x) )
        
        if borderDoesNotContainPunctuation then
            0
        else 
            let number = line[matchStart..matchEnd]
            int (number)


    let sumCodesOnLine(index: int)(line: string) = 
        let digitsMatcher = Regex(@"(\d+)", RegexOptions.Compiled)

        let values = 
            [for numericMatch in digitsMatcher.Matches(line) -> numericMatch]
            |> List.fold( fun acc numericMatch -> 
                    let matchValue = getMatchValue index line numericMatch.Index (numericMatch.Index + numericMatch.Length - 1)
                    acc + matchValue
                ) (0)   

        values

    let sum = 
        input 
        |> Array.indexed
        |> Array.fold (fun (acc: int) (index: int, line: string) -> acc + sumCodesOnLine(index)(line) ) 0 

    printf($"{sum}")

let PartTwo_sumGears() =
    
    let input = File.ReadAllLines("./2023/DayThree/DayThree.txt")

    // Loop through row by row, and identify spans of numbers.
    // For that span of numbers, from index X_1 to X_2, 
    // look at the 'border' of the number.
    // ie: Locations without duplicates
    //      [X_1, X_2][Y - 1], (TOP ROW)
    //      [X_1 - 1][Y - 1, Y + 1], (LEFT COLUMN)
    //      [X_2 + 1][Y - 1, Y + 1], (RIGHT COLUMN)
    //      [X_1, X_2][Y + 1], (BOTTOM ROW)
    let getFullDigits (rowNumber: int) (rowIndex: int) =
        let digitsMatcher = Regex(@"(\d+)", RegexOptions.Compiled)

        let matchedDigits = 
            seq { for numericMatch in digitsMatcher.Matches(input[rowNumber]) -> numericMatch}
            |> Seq.find (fun potentialMatch -> potentialMatch.Index <= rowIndex && rowIndex <= potentialMatch.Index + potentialMatch.Length - 1)

        (rowNumber, matchedDigits.Index, matchedDigits.Index + matchedDigits.Length - 1)
    
    let getGearValue(index: int)(line: string)(gearLocation: int) = 

        let topRow = 
            if index <> 0 then
                seq { (index - 1, gearLocation, input[index - 1][gearLocation]) } // top row
            else 
                Seq.empty

        let leftColumn = 
            if gearLocation <> 0 then
                let minY = max(index - 1)(0)
                let maxY = min(index + 1)(input.Length - 1)
                seq { 
                    for yCoOrd in minY .. maxY -> (yCoOrd, gearLocation - 1, input[yCoOrd][gearLocation - 1])
                } // left column
            else 
                Seq.empty
        
        let rightColumn = 
            if gearLocation < line.Length - 1 then
                let minY = max(index - 1)(0)
                let maxY = min(index + 1)(input.Length - 1)
                seq { 
                    for yCoOrd in minY .. maxY -> (yCoOrd, gearLocation + 1, input[yCoOrd][gearLocation + 1])
                } // right column    
            else 
                Seq.empty

        let bottomRow =
            if index < input.Length - 1 then
                seq { (index + 1, gearLocation, input[index + 1][gearLocation]) } // bottom row
            else 
                Seq.empty
        
        let gearOptions = 
            topRow
            |> Seq.append leftColumn
            |> Seq.append rightColumn
            |> Seq.append bottomRow
            |> Seq.filter (fun (rowNumber, rowIndex, character) -> Char.IsDigit(character))
            |> Seq.map (fun (rowNumber, rowIndex, character) -> getFullDigits (rowNumber) (rowIndex) )
            |> Seq.distinct
        
        let isValidGear = 
            gearOptions 
            |> Seq.length = 2
        

        if isValidGear then
            gearOptions 
            |> Seq.map (fun (rowNumber, rowIndexStart, rowIndexEnd) -> int (input[rowNumber][rowIndexStart..rowIndexEnd]) )
            |> Seq.reduce (fun a b -> a * b) // Take the product of the two elements
        else 
            0

    let sumGearsOnLine(index: int)(line: string) = 
        let digitsMatcher = Regex(@"(\*)", RegexOptions.Compiled)

        let values = 
            [for numericMatch in digitsMatcher.Matches(line) -> numericMatch]
            |> List.fold( fun acc numericMatch -> 
                    let matchValue = getGearValue index line numericMatch.Index
                    acc + matchValue
                ) (0)   

        values


    let sum = 
        input 
        |> Array.indexed
        |> Array.fold (fun (acc: int) (index: int, line: string) -> acc + sumGearsOnLine(index)(line) ) 0 

    printf($"{sum}")
