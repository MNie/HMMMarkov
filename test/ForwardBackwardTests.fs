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

