namespace AdventOfCode.TwentyTwentyThree
open System.IO
open System

// For more information see https://aka.ms/fsharp-console-apps
module DayOne = 
    let run(rootPath: string) = 
    
        let convertToDigits(line: string) =

            let charToNumber(ch : char) = int ch - int '0'
            let digits = line |> Seq.where(fun x -> System.Char.IsDigit(x))
            let firstDigit = digits |> Seq.head
            let lastDigit = digits |> Seq.last

            charToNumber(firstDigit) * 10 + charToNumber(lastDigit)

        let convertWordedDigitToNumericDigit(line: string) =
            let conversions = [
                ("one", "1");
                ("two", "2");
                ("three", "3");
                ("four", "4");
                ("five", "5");
                ("six", "6");
                ("seven", "7");
                ("eight", "8");
                ("nine", "9");
            ]
            let mutable outputValue  = line
            let mutable filteredValues = 
                conversions |> Seq.filter(fun (key, value) -> outputValue.Contains(key))
            
            while not(Seq.isEmpty(filteredValues)) do
                let (keyToReplace, valueToReplace, _) = 
                    filteredValues
                        |> Seq.map(fun (key, value) -> (key, value, line.IndexOf(key)))
                        |> Seq.minBy(fun (_, _, index) -> index)

                outputValue <- outputValue.Replace(keyToReplace, valueToReplace)
                filteredValues <- conversions |> Seq.filter(fun (key, value) -> outputValue.Contains(key))

            outputValue

        let lines = File.ReadAllLines($"{rootPath}/DayOne.txt")
        let sum = lines
                |> Array.map(convertWordedDigitToNumericDigit)
                |> Array.map(convertToDigits)
                |> Array.sum
                
        printf($"{sum}")


