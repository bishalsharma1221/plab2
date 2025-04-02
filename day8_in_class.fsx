open System

type Coffee = {
    Decaf: bool
    DarkRoast: bool
    Cream: int
    Sugar: int
}

type Tea = 
    | OrangePekoe
    | EarlGrey
    | Chamomile

type Beverage =
    | Espresso of int * int  // (shots, sugars)
    | HouseCoffee of Coffee
    | Tea of Tea
    | Water

let bill beverage =
    match beverage with
    | Espresso (shots, sugars) -> (float shots * 1.50) + (float sugars * 0.20)
    | HouseCoffee coffee ->
        let basePrice = 1.25
        let decafCharge = if coffee.Decaf then 0.25 else 0.0
        let darkRoastCharge = if coffee.DarkRoast then 0.25 else 0.0
        let creamCharge = float coffee.Cream * 0.20
        let sugarCharge = float coffee.Sugar * 0.20
        basePrice + decafCharge + darkRoastCharge + creamCharge + sugarCharge
    | Tea teaType ->
        match teaType with
        | OrangePekoe -> 2.00
        | EarlGrey | Chamomile -> 1.75
    | Water -> 2.50

let getUserInput () =
    printfn "Choose a beverage: 1) Espresso 2) House Coffee 3) Tea 4) Water"
    let choice = Console.ReadLine()
    match choice with
    | "1" -> 
        printf "Enter number of shots: "
        let shots = int (Console.ReadLine())
        printf "Enter number of sugars: "
        let sugars = int (Console.ReadLine())
        Espresso (shots, sugars)
    | "2" -> 
        printf "Is it decaf? (true/false): "
        let decaf = Boolean.Parse(Console.ReadLine())
        printf "Is it dark roast? (true/false): "
        let darkRoast = Boolean.Parse(Console.ReadLine())
        printf "Enter number of creams: "
        let cream = int (Console.ReadLine())
        printf "Enter number of sugars: "
        let sugar = int (Console.ReadLine())
        HouseCoffee { Decaf = decaf; DarkRoast = darkRoast; Cream = cream; Sugar = sugar }
    | "3" -> 
        printfn "Choose tea type: 1) Orange Pekoe 2) Earl Grey 3) Chamomile"
        let teaChoice = Console.ReadLine()
        let teaType =
            match teaChoice with
            | "1" -> OrangePekoe
            | "2" -> EarlGrey
            | "3" -> Chamomile
            | _ -> failwith "Invalid choice"
        Tea teaType
    | "4" -> Water
    | _ -> failwith "Invalid choice"

let main () =
    let beverage = getUserInput()
    let price = bill beverage
    printfn "Total Price: $%.2f" price

main ()