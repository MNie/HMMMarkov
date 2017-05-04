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
  let B = [| [|0.4; 0.26|]; [|0.3; 0.19|]; [|0.3; 0.55|]|]
  let pi = [|0.33;0.33;0.33|]
  let observations = [|0; 1; 0; 0; 1; 0; 1|]
  let result = (pi, A, B, observations) |> ForwardBackward.calculate

  testList "Forward Backward from exercise" [
    testCase "for first match" <| fun _ ->
      Expect.floatClose Accuracy.high result.[0, 0] 0.3662587179 "win expectation should be close to 0.3662687"
      Expect.floatClose Accuracy.high result.[0, 1] 0.3403724666 "draw expectation should be close to 0.34037"
      Expect.floatClose Accuracy.high result.[0, 2] 0.2933688155 "lose expectation should be close to 0.2933688"

    testCase "for second match" <| fun _ ->
      Expect.floatClose Accuracy.high result.[1, 0] 0.2162136539 "win expectation should be close to 0.21621"
      Expect.floatClose Accuracy.high result.[1, 1] 0.1222543195 "draw expectation should be close to 0.12225"
      Expect.floatClose Accuracy.high result.[1, 2] 0.6615320266 "lose expectation should be close to 0.66153"

    testCase "for third match" <| fun _ ->
      Expect.floatClose Accuracy.high result.[2, 0] 0.4097649843 "win expectation should be close to 0.40976"
      Expect.floatClose Accuracy.high result.[2, 1] 0.209452514 "draw expectation should be close to 0.20945"
      Expect.floatClose Accuracy.high result.[2, 2] 0.3807825018 "lose expectation should be close to 0.38078"

    testCase "for fourth match" <| fun _ ->
      Expect.floatClose Accuracy.high result.[3, 0] 0.3583581489 "win expectation should be close to 0.358358"
      Expect.floatClose Accuracy.high result.[3, 1] 0.260368854 "draw expectation should be close to 0.260"
      Expect.floatClose Accuracy.high result.[3, 2] 0.3812729971 "lose expectation should be close to 0.381"

    testCase "for fifth match" <| fun _ ->
      Expect.floatClose Accuracy.high result.[4, 0] 0.2289091441 "win expectation should be close to 0.2289"
      Expect.floatClose Accuracy.high result.[4, 1] 0.1247629533 "draw expectation should be close to 0.12576"
      Expect.floatClose Accuracy.high result.[4, 2] 0.6463279026 "lose expectation should be close to 0.6463"

    testCase "for sixth match" <| fun _ ->
      Expect.floatClose Accuracy.high result.[5, 0] 0.3755874055 "win expectation should be close to 0.375587"
      Expect.floatClose Accuracy.high result.[5, 1] 0.2494614647 "draw expectation should be close to 0.24946"
      Expect.floatClose Accuracy.high result.[5, 2] 0.3749511297 "lose expectation should be close to 0.37495"
    
    testCase "for seventh match" <| fun _ ->
      Expect.floatClose Accuracy.high result.[6, 0] 0.0 "win expectation should be close to 0.0"
      Expect.floatClose Accuracy.high result.[6, 1] 0.0 "draw expectation should be close to 0.0"
      Expect.floatClose Accuracy.high result.[6, 2] 0.0 "lose expectation should be close to 0.0"
  ]

