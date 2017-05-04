namespace markov.core
    open System
    open System.Collections.Generic

    module ForwardBackward =
        let private alfaPass(pi: double[], A: double[][], B: double[][], observations: int[]) =
            let c = Array.zeroCreate observations.Length
            let alfa = Array2D.create observations.Length A.Length 0.0
            for i in [0..A.Length - 1] do
                alfa.[0, i] <- pi.[i] * B.[i].[observations.[0]]
                c.[0] <- c.[0] + alfa.[0, i]
            
            c.[0] <- 1.0/c.[0]
            for i in 0..A.Length - 1 do
                alfa.[0, i] <- c.[0] * alfa.[0, i]
            
            for t in 1..observations.Length - 1 do
                for i in 0..A.Length - 1 do
                    for j in 0..A.Length - 1 do
                        alfa.[t, i] <- alfa.[t, i] + alfa.[t - 1, j] * A.[j].[i]
                    alfa.[t, i] <- alfa.[t, i] * B.[i].[observations.[t]]
                    c.[t] <- c.[t] + alfa.[t, i]
                
                c.[t] <- 1.0/c.[t]
                for i in 0..A.Length - 1 do
                    alfa.[t, i] <- c.[t] * alfa.[t, i]
            (c, alfa)

        let private betaPass (pi: double[], A: double[][], B: double[][], observations: int[]) (c: double[]) =
            let beta = Array2D.create observations.Length A.Length 0.0
            let lastT = observations.Length - 1
            for i in 0..A.Length - 1 do
                beta.[lastT, i] <- c.[lastT]
            
            for t = lastT - 1 downto 0 do
                for i in 0..A.Length - 1 do
                    for j in 0..A.Length - 1 do
                        beta.[t, i] <- beta.[t, i] + A.[i].[j] * B.[j].[observations.[t + 1]] * beta.[t + 1, j]
                    beta.[t, i] <- c.[t] * beta.[t, i]
            beta

        let private gamma (pi: double[], A: double[][], B: double[][], observations: int[]) (alfa: double[,], beta: double[,]) =
            let gamma = Array2D.create observations.Length A.Length 0.0
            for t in 0..observations.Length - 2 do
                let denom = 
                    [0..A.Length - 1]
                    |> List.sumBy (fun i -> 
                            [0..A.Length - 1]
                            |> List.sumBy (fun j -> alfa.[t, i] * A.[i].[j] * B.[j].[observations.[t + 1]] * beta.[t + 1, j])
                        )
                for i in 0..A.Length - 1 do
                    for j in 0..A.Length - 1 do
                        let localGamma = (alfa.[t, i] * A.[i].[j] * B.[j].[observations.[t + 1]] * beta.[t + 1, j])/denom
                        gamma.[t, i] <- gamma.[t, i] + localGamma
            gamma.[observations.Length - 1, 0..] <- alfa.[observations.Length - 1, 0..]
            gamma

        let calculate(pi: double[], A: double[][], B: double[][], observations: int[]) =
            let parameters = (pi, A, B, observations)
            let c, alfa = alfaPass parameters
            let beta = c |> betaPass(parameters)
            (alfa, beta) |> gamma(parameters)

        let private getIndexOfMax(tab: double[]) =
            tab
            |> Array.mapi (fun index elem -> (index, elem))
            |> Array.maxBy snd
            |> fst

        let predict(pi: double[], A: double[][], B: double[][], observations: int[], mapping: IDictionary<int, string>) =
            let calculations = (pi, A, B, observations) |> calculate
            let mappedCalculations = 
                seq { for i in 0..observations.Length - 1 do
                    yield mapping.[getIndexOfMax(calculations.[i, 0..])]
                }
            mappedCalculations |> String.Concat


        
        
        
