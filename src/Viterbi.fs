namespace markov.core
    open System
    open System.Collections.Generic

    module Viterbi =
        let private getMaxes(T1: double[,], i: int, selector) =
            T1.[0.., i]
            |> Array.mapi selector
            |> Array.maxBy snd

        let private getMax(T1: double[,], A: double[][], i: int, j: int) =
            let selector = fun index elem -> (index, elem * A.[index].[j])
            getMaxes(T1, i - 1, selector)

        let private getMaxIndex(T1: double[,], n: int) =
            let selector = fun index elem -> (index, elem)
            getMaxes(T1, n, selector) |> fst

        let Calculate(states: int[], probabilities: double[], observations: int[], A: double[][], B: double[][]) =
            let t1: double[,] = Array2D.create states.Length observations.Length 0.0
            let t2: int[,] = Array2D.create states.Length observations.Length 0
            let z = Array.zeroCreate observations.Length
            let x = Array.zeroCreate observations.Length

            for i in [0..states.Length - 1] do
                t1.[i, 0] <- probabilities.[i] * B.[i].[observations.[0]]

            for i in [1..observations.Length - 1] do
                for j in [0..states.Length - 1] do
                    let maxes = getMax(t1, A, i, j)
                    t1.[j, i] <- B.[j].[observations.[i]] * snd maxes
                    t2.[j, i] <- fst maxes
            z.[observations.Length - 1] <- getMaxIndex(t1, observations.Length - 1)
            x.[observations.Length - 1] <- states.[z.[observations.Length - 1]]
            for i = observations.Length - 1 downto 1 do
                z.[i-1] <- t2.[z.[i], i]
                x.[i - 1] <- states.[z.[i-1]]
            x

        let Predict(states: int[], probabilities: double[], observations: int[], A: double[][], B: double[][], mapping: IDictionary<int, string>) =
            (states, probabilities, observations, A, B)
            |> Calculate
            |> Array.map (fun x -> mapping.[x])
            |> String.concat ""
            
