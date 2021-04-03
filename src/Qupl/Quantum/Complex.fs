namespace Quantum

type Complex = double * double

module Complex =

    let add a b : Complex =
        (fst a + fst b, snd a + snd b)

    let multiply (a: Complex) (b: Complex) : Complex =
        (fst a * fst b - snd a * snd b,
         fst a * snd b + snd a * fst b)
    
    let private round3 x =
        round (x * 1000.) / 1000.

    let probability (x, y) : double =
        x * x + y * y

    let tostring (r, i) =
        string (round3 r) + "+" + string (round3 i) + "i"
    
    let zero : Complex = (0., 0.)

    let one : Complex = (1., 0.)

    let i : Complex = (0., 1.)


