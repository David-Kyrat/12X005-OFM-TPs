let isWoman (name: string) : bool =
    match name with
    | "Aline"
    | "Cynthia" -> true
    | _ -> false

let people =
    [ "Aline"; "Bernard"; "Cynthia" ]

printfn $"There are women in the list of people: {List.exists isWoman people}"

printfn $"There are only women in the list of people: {List.forall isWoman people}"
