using System;
using System.Text;

namespace Quantum {

    class Operator {

        protected int _r;
        protected int _c;

        public int R {
            get {
                return _r;
            }
            set {}
        }

        public int C {
            get {
                return _c;
            }
            set {}
        }

        public double SumSquaredMagnitudes {
            get {
                double total = 0;
                for (int r = 0; r < R; r++) {
                    for (int c = 0; c < C; c++) {
                        total += values[r, c].SquaredMagnitude();
                    }
                }
                return total;
            }
        }

        protected Complex[,] values;

        public Complex this[int index1, int index2] { 
            get { return values[index1, index2]; } 
            set { values[index1, index2] = value; } 
        }

        public Operator() {
            _r = 0;
            _c = 0;
            values = new Complex[_r, _c];
        }

        public Operator(int rows, int columns) {
            _r = rows;
            _c = columns;
            values = new Complex[_r,_c];
        }

        public Operator(int[,] values) {
            _r = values.GetLength(0);
            _c = values.GetLength(1);
            this.values = new Complex[_r, _c];
            for (int r = 0; r < _r; r++) {
                for (int c = 0; c < _c; c++) {
                    this[r, c] = new Complex(values[r, c]);
                }
            }
        }

        public Operator(double[,] values) {
            _r = values.GetLength(0);
            _c = values.GetLength(1);
            this.values = new Complex[_r, _c];
            for (int r = 0; r < _r; r++) {
                for (int c = 0; c < _c; c++) {
                    this[r, c] = new Complex(values[r, c]);
                }
            }
        }

        public Operator(Complex[,] values) {
            _r = values.GetLength(0);
            _c = values.GetLength(1);
            this.values = new Complex[_r, _c];
            for (int r = 0; r < _r; r++) {
                for (int c = 0; c < _c; c++) {
                    this[r, c] = values[r, c];
                }
            }
        }

        public static Operator operator +(Operator a, Operator b) {
            Operator tensorProduct = new Operator(a.R*b.R, a.C*b.C);
            for (int ar = 0; ar < a.R; ar++) {
                for (int ac = 0; ac < a.C; ac++) {
                    for (int br = 0; br < b.R; br++) {
                        for (int bc = 0; bc < b.C; bc++) {
                            tensorProduct[ar*b.R+br, ac*b.C+bc] = a[ar, ac] * b[br, bc];
                        }
                    }
                }
            }
            return tensorProduct;
        }

        public static Operator operator *(Operator a, Operator b) {
            Operator c = new Operator(a.R, b.C);
            for (int row = 0; row < a.R; row++) {
                for (int column = 0; column < b.C; column++) {
                    Complex total = Complex.ZERO;
                    for (int shared = 0; shared < a.C; shared++) {
                        total += a[row, shared] * b[shared, column];
                    }
                    c[row, column] = total;
                }
            }
            return c;
        }

        public static Operator operator *(Operator a, Complex b) {
            Operator matrix = new Operator(a.R, a.C);
            for (int r = 0; r < a.R; r++) {
                for (int c = 0; c < a.C; c++) {
                    matrix[r, c] = a[r, c] * b;
                }
            }
            return matrix;
        }

        public static Operator operator *(Operator a, double b) {
            return a * new Complex(b);
        }

        public override string ToString() {
            StringBuilder sb = new StringBuilder();
            for (int r = 0; r < _r; r++) {
                for (int c = 0; c < _c; c++) {
                    sb.Append(values[r, c].ToString()).Append(" ");
                }
                sb.Append("\n");
            }
            return sb.ToString();
        }

        public void Clear() {
            values = new Complex[_r, _c];
        }

        public static Operator FOURIER(int bits) {
            int n = 1 << bits;
            var op = new Operator(n, n);
            double angle = 2*Math.PI / (double)n;
            Complex w = new Complex(Math.Cos(angle), Math.Sin(angle));
            for (int row = 0; row < op.R; row++)
            {
                for (int column = 0; column < op.C; column++) {
                    op[row, column] = Complex.Pow(w, row*column) / Math.Sqrt((double)n);
                }
            }
            return op;
        }

        public static Operator HADAMARD {
            get {
                return new Operator(
                    new double[2,2] {
                        { 1/Math.Sqrt(2), 1/Math.Sqrt(2) },
                        { 1/Math.Sqrt(2), -1/Math.Sqrt(2) }
                    }
                );
            }
        }

        public static Operator CNOT {
            get {
                return new Operator(
                    new double[4,4] {
                        { 1, 0, 0, 0 },
                        { 0, 1, 0, 0 },
                        { 0, 0, 0, 1 },
                        { 0, 0, 1, 0 }
                    }
                );
            }
        }

        public static Operator SWAP {
            get {
                return new Operator(
                    new double[4,4] {
                        { 1, 0, 0, 0 },
                        { 0, 0, 1, 0 },
                        { 0, 1, 0, 0 },
                        { 0, 0, 0, 1 }
                    }
                );
            }
        }

        public static Operator IDENTITY {
            get {
                return new Operator(
                    new double[2,2] {
                        { 1, 0 },
                        { 0, 1 }
                    }
                );
            }
        }

        public static Operator NOT {
            get {
                return new Operator(
                    new double[2,2] {
                        { 0, 1 },
                        { 1, 0 }
                    }
                );
            }
        }

        public static Operator ZERO {
            get {
                return new Operator(
                    new double[2,2] {
                        { 1, 1 },
                        { 0, 0 }
                    }
                );
            }
        }

        public static Operator ONE {
            get {
                return new Operator(
                    new double[2,2] {
                        { 1, 1 },
                        { 0, 0 }
                    }
                );
            }
        }


    }

}
