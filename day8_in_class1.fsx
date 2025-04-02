type Player = A | B

type PlayerPoints = Zero | Fifteen | Thirty | Forty 

type Score = 
    | Points of PlayerPoints * PlayerPoints 
    | Advantage of Player 
    | Deuce 
    | Game of Player

let nextPointScore = function 
    | Zero -> Fifteen
    | Fifteen -> Thirty
    | Thirty -> Forty
    | Forty -> failwith "Invalid state"

let normalize = function 
    | Points(Forty,Forty) -> Deuce
    | s -> s

let scorePoint score point =
    match score, point with 
    | Advantage player1, player2 when player1 = player2 -> Game player1
    | Advantage _, _ -> Deuce
    | Deuce, player -> Advantage player
    | Points(Forty, _), A -> Game A
    | Points(_, Forty), B -> Game B
    | Points(a, b), A -> normalize (Points (nextPointScore a, b))
    | Points(a, b), B -> normalize (Points (a, nextPointScore b))
    | Game _ , _ -> score

let scoreGame points = 
    Seq.scan scorePoint (Points(Zero,Zero)) points

// Sample games
let game1 = seq { yield A; yield A; yield A; yield A }
let game2 = seq { yield A; yield B; yield A; yield B; yield A; yield B }
let game3 = seq { yield A; yield B; yield A; yield A; yield A }

// Print results
printfn "Game 1 results:"
scoreGame game1 |> Seq.toList |> printfn "%A"

printfn "\nGame 2 results:"
scoreGame game2 |> Seq.toList |> printfn "%A"

printfn "\nGame 3 results:"
scoreGame game3 |> Seq.toList |> printfn "%A"

// Random game
let randomGame = 
    let rnd = System.Random()
    seq { for _ in 1..10 do yield if rnd.NextDouble() < 0.5 then A else B }

printfn "\nRandom game results:"
scoreGame randomGame |> Seq.toList |> printfn "%A"
