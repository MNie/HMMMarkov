module Tests.ForwardBackward

open Expecto
open markov.core

module Lecture =
  let A = [|[|0.7; 0.3|];[|0.4; 0.6|]|]
  let B = [|[|0.1;0.4;0.5|]; [|0.7; 0.2; 0.1|]|]
  let pi = [|0.6;0.4|]
  let observations = [|0;1;0;2|]  
  let mapping = dict [(0, "H"); (1, "C")]
  
  [<Tests>]
  let calculationTests =
    let result = (pi, A, B, observations) |> ForwardBackward.calculate
    
    testList "Forward Backward from Lecture for calculations" [
      testCase "for first character" <| fun _ ->
        Expect.floatClose Accuracy.high result.[0, 0] 0.1881698098 "c expectation should be close to 0.1881698"
        Expect.floatClose Accuracy.high result.[0, 1] 0.8118301902 "h expectation should be close to 0.81183"

      testCase "for second character" <| fun _ ->
        Expect.floatClose Accuracy.high result.[1, 0] 0.5194317521 "c expectation should be close to 0.51943"
        Expect.floatClose Accuracy.high result.[1, 1] 0.4805682479 "h expectation should be close to 0.480568"

      testCase "for third character" <| fun _ ->
        Expect.floatClose Accuracy.high result.[2, 0] 0.2288776273 "c expectation should be close to 0.2288776"
        Expect.floatClose Accuracy.high result.[2, 1] 0.7711223727 "h expectation should be close to 0.771122"

      testCase "for fourth character" <| fun _ ->
        Expect.floatClose Accuracy.high result.[3, 0] 0.8039793969 "c expectation should be close to 0.804"
        Expect.floatClose Accuracy.high result.[3, 1] 0.1960206031 "h expectation should be close to 0.19602"
    ]

  [<Tests>]
  let predictionTests =
    let prediction = (pi, A, B, observations, mapping) |> ForwardBackward.predict
    
    testList "Forward Backward from Lecture for prediction" [
      testCase "for prediction" <| fun _ ->
        Expect.equal prediction "CHCH" "prediction should be equal to CHHC"
    ]

module Lechia =
  let A = [| [|0.35; 0.3; 0.35|]; [|0.2; 0.2; 0.6|]; [|0.36; 0.23; 0.41|]|]
  let B = [| [|0.62; 0.38|]; [|0.625; 0.375|]; [|0.37; 0.63|]|]
  let mapping = dict [(0, "W"); (1, "D"); (2, "L")]
  
  [<Tests>]
  let calculationTests =
    let pi = [|0.57;0.29;0.17|]
    let observations = [|0; 1; 0; 0; 1; 0; 1|]
    let result = (pi, A, B, observations) |> ForwardBackward.calculate

    testList "Forward Backward from exercise for calulations" [
      testCase "for first match" <| fun _ ->
        Expect.floatClose Accuracy.high result.[0, 0] 0.5646381316 "win expectation should be close to 0.56"
        Expect.floatClose Accuracy.high result.[0, 1] 0.3309213701 "draw expectation should be close to 0.33"
        Expect.floatClose Accuracy.high result.[0, 2] 0.1044404983 "lose expectation should be close to 0.1"

      testCase "for second match" <| fun _ ->
        Expect.floatClose Accuracy.high result.[1, 0] 0.2473306564 "win expectation should be close to 0.25"
        Expect.floatClose Accuracy.high result.[1, 1] 0.1851950237 "draw expectation should be close to 0.19"
        Expect.floatClose Accuracy.high result.[1, 2] 0.5674743198 "lose expectation should be close to 0.57"

      testCase "for third match" <| fun _ ->
        Expect.floatClose Accuracy.high result.[2, 0] 0.4126717886 "win expectation should be close to 0.41"
        Expect.floatClose Accuracy.high result.[2, 1] 0.2706488666 "draw expectation should be close to 0.27"
        Expect.floatClose Accuracy.high result.[2, 2] 0.3166793448 "lose expectation should be close to 0.32"

      testCase "for fourth match" <| fun _ ->
        Expect.floatClose Accuracy.high result.[3, 0] 0.3558127401 "win expectation should be close to 0.36"
        Expect.floatClose Accuracy.high result.[3, 1] 0.3287579673 "draw expectation should be close to 0.33"
        Expect.floatClose Accuracy.high result.[3, 2] 0.3154292926 "lose expectation should be close to 0.32"

      testCase "for fifth match" <| fun _ ->
        Expect.floatClose Accuracy.high result.[4, 0] 0.2493814096 "win expectation should be close to 0.25"
        Expect.floatClose Accuracy.high result.[4, 1] 0.1737614537 "draw expectation should be close to 0.17"
        Expect.floatClose Accuracy.high result.[4, 2] 0.5768571366 "lose expectation should be close to 0.58"

      testCase "for sixth match" <| fun _ ->
        Expect.floatClose Accuracy.high result.[5, 0] 0.3766066854 "win expectation should be close to 0.38"
        Expect.floatClose Accuracy.high result.[5, 1] 0.3173017846 "draw expectation should be close to 0.32"
        Expect.floatClose Accuracy.high result.[5, 2] 0.3060915299 "lose expectation should be close to 0.31"
      
      testCase "for seventh match" <| fun _ ->
        Expect.floatClose Accuracy.high result.[6, 0] 0.2400637697 "win expectation should be close to 0.24"
        Expect.floatClose Accuracy.high result.[6, 1] 0.1907516371 "draw expectation should be close to 0.19"
        Expect.floatClose Accuracy.high result.[6, 2] 0.5691845932 "lose expectation should be close to 0.57"
    ]

  [<Tests>]
  let predictionTests =
    let pi = [|0.57;0.29;0.17|]
    let observations = [|0; 1; 0; 0; 1; 0; 1|]
    let prediction = (pi, A, B, observations, mapping) |> ForwardBackward.predict

    testList "Forward Backward from exercise for prediction" [
      testCase "for prediction" <| fun _ ->
        Expect.equal prediction "WLWWLWL" "prediction should be equal to WLWWLWL"
    ]

  [<Tests>]
  let predictionTests2016 =
    let pi = [|0.4;0.4;0.2|]
    let observations = [|1; 0; 1; 1; 0; 0; 1|]
    let prediction = (pi, A, B, observations, mapping) |> ForwardBackward.predict

    testList "Forward Backward from exercise for prediction" [
      testCase "for prediction for 2016" <| fun _ ->
        Expect.equal prediction "WWLLWWL" "prediction should be equal to WWLLWWL" // in fact was WWLDWWL 6/7
    ]

  [<Tests>]
  let predictionTests2015 =
    let pi = [|0.25;0.5;0.25|]
    let observations = [|1; 0; 1; 0; 1; 0; 1|]
    let prediction = (pi, A, B, observations, mapping) |> ForwardBackward.predict

    testList "Forward Backward from exercise for prediction" [
      testCase "for prediction for 2015" <| fun _ ->
        Expect.equal prediction "DLLWLWL" "prediction should be equal to DLLWLWL" // in fact was WDWLLDL 2/7
    ]

  [<Tests>]
  let predictionTests2014 =
    let pi = [|0.33;0.33;0.33|]
    let observations = [|1; 0; 1; 0; 1; 0; 1|]
    let prediction = (pi, A, B, observations, mapping) |> ForwardBackward.predict

    testList "Forward Backward from exercise for prediction" [
      testCase "for prediction for 2014" <| fun _ ->
        Expect.equal prediction "LWLWLWL" "prediction should be equal to LWLWLWL" // in fact was WLWWDDW 1/7
    ]

