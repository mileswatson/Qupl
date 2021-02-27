using System;
using QSharp;

class MainClass {
    public static void Main (string[] args) {

        Operator blackbox;
        // uncomment one of these

        // CONSTANT - should give 11 at the end
        //blackbox = Operator.IDENTITY + Operator.IDENTITY;
        //blackbox = Operator.IDENTITY + Operator.NOT;

        // VARIABLE - should give 01 at the end
        //blackbox = Operator.CNOT;
        //blackbox = (Operator.IDENTITY + Operator.NOT) * Operator.CNOT;
        
        Console.WriteLine(blackbox);

        Ket zeros = Ket.FromBits(0, 0);
        Console.WriteLine(zeros);

        Ket ones = (Operator.NOT+Operator.NOT) * zeros;
        Console.WriteLine(ones);

        Ket superposition = (Operator.HADAMARD+Operator.HADAMARD) * ones;
        Console.WriteLine(superposition);

        Ket processed = blackbox * superposition;
        Console.WriteLine(processed);

        Ket result = (Operator.HADAMARD + Operator.HADAMARD) * processed;
        Console.WriteLine(result);
    
        Console.WriteLine(string.Join("", result.Collapse()));


    }

}

