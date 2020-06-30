using System;
using System.Text;

namespace QSharp {

    struct Complex {
        double r;
        double i;

        public Complex(double real, double imaginary) {
            r = real;
            i = imaginary;
        }

        public Complex(double real) {
            r = real;
            i = 0;
        }

        public double SquaredMagnitude() {
            return r*r+i*i;
        }

        public static Complex ONE {
            get { return new Complex(1); }
            set {}
        }

        public static Complex ZERO {
            get { return new Complex(0); }
            set {}
        }

        public static Complex I {
            get { return new Complex(0, 1); }
            set {}
        }

        public static Complex Pow(Complex a, int b) {
            Complex answer = Complex.ONE;
            for (int i = 0; i < b; i++) {
                answer *= a;
            }
            return answer;
        }

        public static Complex operator +(Complex a, Complex b) {
            return new Complex(a.r+b.r, a.i+b.i);
        }

        public static Complex operator *(Complex a, Complex b) {
            return new Complex(a.r*b.r-a.i*b.i, a.r*b.i+a.i*b.r);
        }

        public static Complex operator /(Complex a, double b) {
            return new Complex(a.r/b, a.i/b);
        }

        public static Complex operator !(Complex a) {
            return new Complex(a.r, -a.i);
        }

        public static bool operator ==(Complex a, Complex b) {
            return a.r == b.r && a.i == b.i;
        }

        public static bool operator !=(Complex a, Complex b) {
            return a.r != b.r || a.i != b.i;
        }


        public override bool Equals(object o) {
            if (o is Complex) {
                return this==(Complex)o;
            }
            return false;
        }

        public override int GetHashCode() {
            return HashCode.Combine(12837468236458732, r, i);
        }

        public override string ToString() {
            StringBuilder sb = new StringBuilder();
            sb.Append(r.ToString("F2"))
                .Append("+")
                .Append(i.ToString("F2"))
                .Append("i");
            for (int i = 0; i < 12-sb.Length; i++) {
                sb.Append(" ");
            }
            return sb.ToString();
        }

    }
}