using System;
using System.Collections;

namespace QSharp {
    
    class Ket : Operator {

        public Ket SwapBits(int a, int b) {
            Ket k = new Ket(R);
            for (int r = 0; r < R; r++) {
                BitArray bits = new BitArray(new int[]{r});
                bool buffer = bits[a];
                bits[a] = bits[b];
                bits[b] = buffer;
                int[] array = new int[1];
                bits.CopyTo(array, 0);
                int paired = array[0];
                k[r, 0] = this[paired, 0];
                k[paired, 0] = this[r, 0];
            }
            return k;
        }

        public Bra Bra {
            get {
                Bra b = new Bra(order);
                for (int i = 0; i < _r; i++) {
                    b[0, i] = !this[i, 0];
                }
                return b;
            }
        }

        public int order {
            get { return _r; }
            set {}
        }

        public Ket Normalised {
            get {
                double scale = Math.Sqrt(SumSquaredMagnitudes);  
                Ket x = new Ket(R);
                for (int r = 0; r < R; r++) {
                    x[r, 0] = values[r, 0] / scale;
                }
                return x;
            }
        }

        public void Normalise() {
            double scale = Math.Sqrt(SumSquaredMagnitudes);
            for (int r = 0; r < R; r++) {
                values[r, 0] /= scale;
            }
        }

        public Ket(bool bitStatus) {
            _r = 2;
            _c = 1;
            values = new Complex[_r, _c];
            if (bitStatus) {
                values[1, 0] = Complex.ONE;
            } else {
                values[0, 0] = Complex.ONE;
            }
        }

        public Ket(int order) {
            _r = order;
            _c = 1;
            values = new Complex[_r, _c];
        }

        public Ket(params int[] values) {
            _r = values.Length;
            _c = 1;
            this.values = new Complex[_r, _c];
            for (int i = 0; i < _r; i++) {
                this[i, 0] = new Complex(values[i]);
            }
        }

        public Ket(params double[] values) {
            _r = values.Length;
            _c = 1;
            this.values = new Complex[_r, _c];
            for (int i = 0; i < _r; i++) {
                this[i, 0] = new Complex(values[i]);
            }
        }

        public Ket(params Complex[] values) {
            _r = values.Length;
            _c = 1;
            this.values = new Complex[_r, _c];
            for (int i = 0; i < _r; i++) {
                this[i, 0] = values[i];
            }
        }

        public int Measure(int bitNum) {
            Normalise();
            int divisor = R >> (bitNum+1);
            //Console.WriteLine(divisor);
            double probabilityOfZero = 0;
            for (int r = 0; r < R; r++) {
                //Console.WriteLine(values[r, 0].SquaredMagnitude());
                if ((r / divisor) % 2 == 0) {
                    probabilityOfZero += values[r, 0].SquaredMagnitude();
                }
            }
            Random rng = new Random();
            int measured = rng.NextDouble() < probabilityOfZero ? 0 : 1;
            for (int r = 0; r < R; r++) {
                if ((r / divisor) % 2 == 0 ^ measured == 0) {
                    values[r, 0] = Complex.ZERO;
                }
            }
            Normalise();
            return measured;
        }

        public int[] Collapse() {
            int totalBits = 0;
            int order = this.order;
            while (order != 1) {
                order >>= 1;
                totalBits++;
            }
            int[] bits = new int[totalBits];
            for (int i = 0; i < totalBits; i++) {
                bits[i] = Measure(i);
            }
            return bits;
        }

        /*
        public static Ket ONE {
            get { return new Ket(true); }
            set {}
        }

        public static Ket ZERO {
            get { return new Ket(false); }
            set {}
        }*/

        public static Ket FromBits(params int[] x) {
            Ket k = (x[0] == 0) ? new Ket(false) : new Ket(true);
            for (int i = 1; i < x.Length; i++) {
                k = k + (x[i] == 0 ? new Ket(false) : new Ket(true));
            }
            return k;
        }

        public static Ket operator +(Ket a, Ket b) {
            Ket c = new Ket(a.order * b.order);
            for (int i = 0; i < a.order; i++) {
                for (int j = 0; j < b.order; j++) {
                    c[i*b.order+j, 0] = a[i,0] * b[j,0];
                }
            }
            return c;
        }

        public static Ket operator *(Operator a, Ket b) {
            Ket ket = new Ket(b.R);
            for (int r = 0; r < a.R; r++) {
                Complex total = Complex.ZERO;
                for (int x = 0; x < b.R; x++) {
                    total += a[r, x] * b[x, 0];
                }
                ket[r, 0] = total;
            }
            return ket;
        }

        public static Ket operator ^(Ket a, Ket b) {
            Ket c = new Ket(a.order);
            for (int i = 0; i < a.order; i++) {
                c[i, 0] = a[i, 0] + b[i, 0];
            }
            return c;
        }

    }

}
