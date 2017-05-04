module Tests.Viterbi

open Expecto
open markov.core

module Lecture = 
  let S = [|0;1|]
  let pi = [|0.6;0.4|]
  let Y = [|0;1;0;2|]
  let A = [|[|0.7; 0.3|];[|0.4; 0.6|]|]
  let B = [|[|0.1;0.4;0.5|]; [|0.7; 0.2; 0.1|]|]
  let mapping = dict [(0, "H"); (1, "C")]
  [<Tests>]
  let calculation =
    let result = (S, pi, Y, A, B) |> Viterbi.Calculate
    
    testList "Viterbi from Lecture" [
      testCase "for calculation" <| fun _ ->
        Expect.sequenceEqual result [|1;1;1;0|] "should equal to sequence 1 1 1 0"
    ]

  [<Tests>]
  let prediction =
    let result = (S, pi, Y, A, B, mapping) |> Viterbi.Predict
    
    testList "Viterbi from Lecture" [
      testCase "for prediction" <| fun _ ->
        Expect.equal result "CCCH" "should equal to CCCH"
    ]

module Lechia = 
  let S = [|0;1;2|]
  let A = [| [|0.35; 0.3; 0.35|]; [|0.2; 0.2; 0.6|]; [|0.36; 0.23; 0.41|]|]
  let B = [| [|0.62; 0.38|]; [|0.625; 0.375|]; [|0.37; 0.63|]|]
  
  let mapping = dict [(0, "W"); (1, "D"); (2, "L")]

  [<Tests>]
  let calculation =
    let Y = [|0; 1; 0; 0; 1; 0; 1|]
    let pi = [|0.57;0.29;0.17|]
    let result = (S, pi, Y, A, B) |> Viterbi.Calculate
    
    testList "Viterbi from exercise" [
      testCase "for calculation" <| fun _ ->
        Expect.sequenceEqual result [|0;0;0;1;0;1;2|] "should equal to sequence 1 1 1 0"
    ]

  [<Tests>]
  let prediction =
    let Y = [|0; 1; 0; 0; 1; 0; 1|]
    let pi = [|0.57;0.29;0.17|]
    let result = (S, pi, Y, A, B, mapping) |> Viterbi.Predict
    
    testList "Viterbi from exercise" [
      testCase "for prediction" <| fun _ ->
        Expect.equal result "WWWDWDL" "should equal to WWWDWDL"
    ]

  [<Tests>]
  let predictionFor2016 =
    let Y = [|1; 0; 1; 1; 0; 0; 1|]
    let pi = [|0.4;0.4;0.2|]
    let result = (S, pi, Y, A, B, mapping) |> Viterbi.Predict
    
    testList "Viterbi from exercise" [
      testCase "for prediction for 2016" <| fun _ ->
        Expect.equal result "DWDDDWL" "should equal to DWDDDWL" // in fact was WWLDWWL 4/7
    ]

  [<Tests>]
  let predictionFor2015 =
    let Y = [|1; 0; 1; 0; 1; 0; 1|]
    let pi = [|0.25;0.5;0.25|]
    let result = (S, pi, Y, A, B, mapping) |> Viterbi.Predict
    
    testList "Viterbi from exercise" [
      testCase "for prediction for 2015" <| fun _ ->
        Expect.equal result "WDDWDWL" "should equal to WDDWDWL" // in fact was WDWLLDL 3/7
    ]

  [<Tests>]
  let predictionFor2014 =
    let Y = [|1; 0; 1; 0; 1; 0; 1|]
    let pi = [|0.33;0.33;0.33|]
    let result = (S, pi, Y, A, B, mapping) |> Viterbi.Predict
    
    testList "Viterbi from exercise" [
      testCase "for prediction for 2014" <| fun _ ->
        Expect.equal result "DWDWDWL" "should equal to DWDWDWL" // in fact was WLWWDDW 2/7
    ]