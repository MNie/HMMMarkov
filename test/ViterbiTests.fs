module Tests.Viterbi

open Expecto
open markov.core

[<Tests>]
let baseTests =
  let O = [|0;1;2|]
  let S = [|0;1|]
  let pi = [|0.6;0.4|]
  let Y = [|0;1;0;2|]
  let A = [|[|0.7; 0.3|];[|0.4; 0.6|]|]
  let B = [|[|0.1;0.4;0.5|]; [|0.7; 0.2; 0.1|]|]
  let result = (O, S, pi, Y, A, B) |> Viterbi.Calculate

  testList "Forward Backward from Lecture" [
    testCase "for first character" <| fun _ ->
      System.Console.WriteLine(sprintf "%A" result)
  ]