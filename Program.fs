open System.IO
open System
// For more information see https://aka.ms/fsharp-console-apps

let convertToDigits(line: string) =

    let charToNumber(ch : char) = int ch - int '0'
    let digits = line |> Seq.where(fun x -> System.Char.IsDigit(x))
    let firstDigit = digits |> Seq.head
    let lastDigit = digits |> Seq.last

    charToNumber(firstDigit) * 10 + charToNumber(lastDigit)

let lines = File.ReadAllLines("./2023/Day1.txt")
let sum = lines
        |> Array.map(convertToDigits)
        |> Array.sum
        
printf($"{sum}")


