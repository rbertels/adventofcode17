open System
open System.IO

let input = File.ReadAllLines(__SOURCE_DIRECTORY__ + "\\input_4.txt")


let checkForAnagrams (words : string array) =
    words |> Array.allPairs words

let checkPassPhrase (phrase : string) =
    let words = phrase.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
    let count = words |> Array.length
    let uniqueCount = words |> Array.distinct |> Array.length
    count = uniqueCount

let checkPassPhraseWithAnagrams (phrase : string) =
    let words = 
        phrase.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (Seq.sort >> Seq.toArray >> String)
    let count = words |> Array.length
    let uniqueCount = words |> Array.distinct |> Array.length
    count = uniqueCount

let phrases = input

let result1 = phrases |> Array.filter checkPassPhrase |> Array.length

let result2 = phrases |> Array.filter checkPassPhraseWithAnagrams |> Array.length


