namespace markov.core

    open System

    module Viterbi =
        let Calculate(space: int[], states: int[], probabilities: double[], observations: int[], transition: int[,], emission: double[,]) =
            let T1: double[,] = Array2D.create states.Length observations.Length 0.0
            let T2: double[,] = Array2D.create states.Length observations.Length 0.0
            for i in [0..states.Length] do
                T1.[i, 1] <- probabilities.[i] * emission.[0, 0]

            for i in [1..observations.Length] do
                for j in [0..states.Length] do
                    T1.[j, i] <- emission.[j, i] * 1.0 //max(T1[transitions.Length, i -1] * transitions[transitions.Lengt-1, j]
                    T2.[j, i] <- 0.0 // argmax (T1[transitions.Length, i - 1] * transitions[transitions.Lengt-1, j]
            let z = [||] // argmax (T1[transitions.Length, emission.Length]
            let x = [||] //szt ?!?!?!?
            for i in [1..T1.Length] do
                z.[i-1] <- T2.[z.[i], i]
                x.[i - 1] <- space.[z.[i-1]]
            x

            
