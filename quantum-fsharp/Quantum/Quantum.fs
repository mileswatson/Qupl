namespace Quantum

type State = Matrix

module State =

    let zero = 
        array2D [[Complex.one];
                 [Complex.zero]]

    let one = 
        array2D [[Complex.zero];
                 [Complex.one]]

    let private measuremap divisor r x =
        if (r / divisor) % 2 = 0 then
            Complex.probability x
        else 0.


    let measure bit (s : State) =
        let divisor = (Matrix.rows s) >>> (bit + 1)
        let mutable total = double 0.
        s
        |> Array2D.iteri (fun r _ x ->
            total <- total + measuremap divisor r x)
        Matrix.columns s
        |> double
        |> (/) total


type Operator = Matrix

module Operator =
    
    let fromSeq2D matrix : Operator =
        array2D matrix

    let fromFunction qbits funct: Operator =
        pown 2 qbits
        |> Matrix.identity
        |> funct

module Core =

    let debugstate qbits : State =
        pown 2 qbits
        |> Matrix.identity

    let combine (operators : seq<Operator>) : Operator =
        if Seq.length operators = 1 then Seq.head operators
        else Operator.fromSeq2D [[(1., 0.)]]
            |> Seq.fold Matrix.tensor <| operators
    
    let apply (operators: seq<Operator>) : State -> State =
        combine operators
        |> Matrix.multiply
    
    let chain (second: seq<Operator>) (first: Operator) : Operator =
        combine second
        |> Matrix.multiply <| first
    
    let measure = State.measure
    
    let tostring = Matrix.tostring

    let sqrt2 : double = sqrt 2.

    let sqrt2' : double = 1. / sqrt2

    let IDENTITY =
        Operator.fromSeq2D
            [[ Complex.one; Complex.zero ];
             [ Complex.zero; Complex.one ]]

    let NOT =
        Operator.fromSeq2D
            [[ Complex.zero; Complex.one ];
             [ Complex.one; Complex.zero ]]

    let HADAMARD =
        Operator.fromSeq2D [[ (sqrt2', 0.); (sqrt2', 0.) ];
                         [ (sqrt2', 0.); (-sqrt2', 0.) ]]
    
    let CNOT =
        Operator.fromSeq2D
            [[ Complex.one; Complex.zero; Complex.zero; Complex.zero ];
             [ Complex.zero; Complex.one; Complex.zero; Complex.zero ];
             [ Complex.zero; Complex.zero; Complex.zero; Complex.one ];
             [ Complex.zero; Complex.zero; Complex.one; Complex.zero ];] 
