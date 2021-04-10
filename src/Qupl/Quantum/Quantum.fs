namespace Quantum

module State =

    type _State = State of Matrix

    let fromMatrix (m: Matrix) =
        if (Matrix.columns m) = 5 then
            Some(State m)
        else
            None

    let toMatrix (State s) = s

    let entangle states =
        states
        |> Seq.map toMatrix
        |> Matrix.tensorAll
        |> State

    let private measuremap divisor r x =
        if (r / divisor) % 2 = 0 then
            Complex.probability x
        else
            0.

    let measure bit (State s) =
        let divisor = (Matrix.rows s) >>> (bit + 1)
        let mutable total = double 0.

        s
        |> Array2D.iteri (fun r _ x -> total <- total + measuremap divisor r x)

        Matrix.columns s |> double |> (/) total

    let debug = pown 2 >> Matrix.identity >> State

    let zero =
        array2D [ [ Complex.one ]
                  [ Complex.zero ] ]
        |> State

    let one =
        array2D [ [ Complex.zero ]
                  [ Complex.one ] ]
        |> State

module Operator =

    type _Operator = Operator of Matrix

    let fromMatrix m =
        let (rows, columns) = (Matrix.rows m, Matrix.columns m)
        let square = (rows = columns)
        let pow2 = rows &&& (rows - 1)

        if square && pow2 = 0 then
            Some(Operator m)
        else
            None

    let toMatrix (Operator o) = o

    let private fromSeq2D = array2D >> Operator

    let fromFunction bits f =
        let arrSize = (1 <<< (bits * 2))

        let arr =
            Array2D.create arrSize arrSize Complex.zero

        for i in 0 .. (arrSize - 1) do
            arr.[f i, i] <- Complex.one

        arr

    let combine operators =
        Seq.map toMatrix operators
        |> Matrix.tensorAll
        |> Operator

    let apply operator (State.State s) =
        operator |> toMatrix |> Matrix.multiply <| s
        |> State.State

    let chain second (Operator first) =
        combine second |> toMatrix |> Matrix.multiply
        <| first
        |> Operator

    let chainAll (operators: _Operator list list) =
        operators
        |> List.map (combine >> toMatrix)
        |> List.reduce Matrix.multiply
        |> Operator

    let private sqrt2 : double = sqrt 2.

    let private sqrt2' : double = 1. / sqrt2

    let IDENTITY =
        fromSeq2D [ [ Complex.one; Complex.zero ]
                    [ Complex.zero; Complex.one ] ]

    let HADAMARD =
        fromSeq2D [ [ (sqrt2', 0.); (sqrt2', 0.) ]
                    [ (sqrt2', 0.); (-sqrt2', 0.) ] ]

    let NOT =
        fromSeq2D [ [ Complex.zero; Complex.one ]
                    [ Complex.one; Complex.zero ] ]

    let CNOT =
        fromSeq2D [ [ Complex.one
                      Complex.zero
                      Complex.zero
                      Complex.zero ]
                    [ Complex.zero
                      Complex.one
                      Complex.zero
                      Complex.zero ]
                    [ Complex.zero
                      Complex.zero
                      Complex.zero
                      Complex.one ]
                    [ Complex.zero
                      Complex.zero
                      Complex.one
                      Complex.zero ] ]

    let ADD bits =
        fromFunction
            (bits * 2)
            (fun x ->
                let max = 1 <<< bits
                let mask = max - 1
                let num1 = x &&& mask
                let num2 = x >>> bits
                ((num1 + num2) % max) + (num2 <<< bits))

module Core =

    let zero = State.zero

    let one = State.one

    let apply operators =
        operators |> Operator.combine |> Operator.apply

    let combine = Operator.combine

    let entangle = State.entangle

    let chain = Operator.chain

    let measure = State.measure

    let tostring = State.toMatrix >> Matrix.tostring

    let IDENTITY = Operator.IDENTITY

    let HADAMARD = Operator.HADAMARD

    let NOT = Operator.NOT

    let CNOT = Operator.CNOT

    let ADD = Operator.ADD
