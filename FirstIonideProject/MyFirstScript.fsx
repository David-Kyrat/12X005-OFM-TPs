let toPigLatin (word: string) =
    let isVowel (c: char) =
    let x = 2

        match c with
        | 'a' | 'e' | 'i' |'o' |'u'
        | 'A' | 'E' | 'I' | 'O' | 'U' -> true
        |_ -> false
    
    if isVowel word[0] then
        word + "yay"
    else
        word[1..] + string(word[0]) + "ay"

//(toPigLatin >> printfn "%s") "banana"

//"banana" |> toPigLatin |> printfn "%s"