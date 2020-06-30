

namespace QSharp {
    
    class Bra : Operator {

        public int order {
            get { return _c; }
            set {}
        }

        /*
        public static Bra ONE {
            get { return new Bra(true); }
            set {}
        }

        public static Bra ZERO {
            get { return new Bra(false); }
            set {}
        }

        public Bra(bool bitStatus) {
            _r = 1;
            _c = 2;
            values = new Complex[_r, _c];
            if (bitStatus) {
                values[0, 1] = Complex.ONE;
            } else {
                values[0, 0] = Complex.ONE;
            }
        }*/

        public Bra(int order) {
            _r = 1;
            _c = order;
            values = new Complex[_r, _c];
        }

        public Bra(int[] values) {
            _r = 1;
            _c = values.Length;
            this.values = new Complex[_r, _c];
            for (int i = 0; i < _c; i++) {
                this[0, i] = new Complex(values[i]);
            }
        }

        public Bra(float[] values) {
            _r = values.Length;
            _c = 1;
            this.values = new Complex[_r, _c];
            for (int i = 0; i < _c; i++) {
                this[0, i] = new Complex(values[i]);
            }
        }

        public Bra(Complex[] values) {
            _r = values.Length;
            _c = 1;
            this.values = new Complex[_r, _c];
            for (int i = 0; i < _c; i++) {
                this[0, i] = values[i];
            }
        }

        public static Complex operator *(Bra b, Ket k) {
            Complex total = Complex.ZERO;
            for (int i = 0; i < b.order; i++) {
                total += b[0, i] * k[i, 0];
            }
            return total;
        }

        public Ket Ket {
            get {
                Ket k = new Ket(order);
                for (int i = 0; i < _c; i++) {
                    k[i, 0] = !this[0, i];
                }
                return k; 
            }
            
        }

    }

}