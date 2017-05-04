module Tests

open Expecto
open markov.core

[<Tests>]
let baseTests =
  let A = [|[|0.7; 0.3|];[|0.4; 0.6|]|]
  let B = [|[|0.1;0.4;0.5|]; [|0.7; 0.2; 0.1|]|]
  let pi = [|0.6;0.4|]
  let observations = [|0;1;0;2|]
  let result = (pi, A, B, observations) |> ForwardBackward.calculate

  testList "Forward Backward from Lecture" [
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
      Expect.floatClose Accuracy.high result.[3, 0] 0.0 "c expectation should be close to 0.0"
      Expect.floatClose Accuracy.high result.[3, 1] 0.0 "h expectation should be close to 0.0"
  ]

[<Tests>]
let lechiaTests =
  let A = [| [|0.35; 0.3; 0.35|]; [|0.2; 0.2; 0.6|]; [|0.36; 0.23; 0.41|]|]
  let B = [| [|0.62; 0.38|]; [|0.625; 0.375|]; [|0.37; 0.63|]|]
  let pi = [|0.57;0.29;0.17|]
  let observations = [|0; 1; 0; 0; 1; 0; 1|]
  let result = (pi, A, B, observations) |> ForwardBackward.calculate

  testList "Forward Backward from exercise" [
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
      Expect.floatClose Accuracy.high result.[6, 0] 0.0 "win expectation should be close to 0.0"
      Expect.floatClose Accuracy.high result.[6, 1] 0.0 "draw expectation should be close to 0.0"
      Expect.floatClose Accuracy.high result.[6, 2] 0.0 "lose expectation should be close to 0.0"
  ]

