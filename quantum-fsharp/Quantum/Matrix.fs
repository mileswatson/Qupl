namespace Quantum

type Matrix = Complex[,]

module Matrix =

    let rows (m: Matrix) = m.GetLength 0
    let columns (m: Matrix) = m.GetLength 1

    let identity size : Matrix =
        Array2D.init size size (fun r c -> if r = c then Complex.one else Complex.zero)

    let add a b : Matrix =
        if (rows a) = (rows b) && (columns a) = (columns b) then
            Array2D.init (rows a) (columns a) (fun r c -> Complex.add a.[r, c] b.[r, c])
        else invalidArg "a, b" "Matrices must share their inner dimension!"

    let multiply a b : Matrix =
        if (columns a) = (rows b) then
            Array2D.init (rows a) (columns b) (fun r c ->
                seq { for k in 0 .. (columns a)-1 -> Complex.multiply a.[r, k] b.[k, c] }
                |> Seq.fold Complex.add Complex.zero)
        else invalidArg "a, b" "Matrices must share their inner dimension!"

    let tensor (a: Matrix) (b: Matrix) : Matrix =
        fun r c -> Complex.multiply a.[r / rows b, c / columns b] b.[r % rows b, c % columns b]
        |> Array2D.init ((rows a) * (rows b)) ((columns a) * (columns b))
    
    let tensorAll matrices : Matrix = 
        if Seq.length matrices = 1 then Seq.head matrices
        else [[(1., 0.)]]
            |> array2D
            |> Seq.fold tensor <| matrices

    let tostring matrix =
        Array.init (rows matrix)
            <| fun r ->
                seq { for c in 0 .. columns matrix - 1 -> matrix.[r, c] }
                |> Seq.map Complex.tostring
                |> String.concat ", "
        |> String.concat "\n"
