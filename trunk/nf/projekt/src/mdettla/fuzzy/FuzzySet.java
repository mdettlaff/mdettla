package mdettla.fuzzy;

public class FuzzySet {

	private final double a;
	private final double b;
	private final double c;

	public FuzzySet(double a, double b, double c) {
		this.a = a;
		this.b = b;
		this.c = c;
	}

	public double membership(double x) {
		if (a <= x && x <= b) {
			if (a == Double.NEGATIVE_INFINITY) {
				return 1;
			} else {
				return (x - a) / (b - a);
			}
		} else if (b <= x && x <= c) {
			if (c == Double.POSITIVE_INFINITY) {
				return 1;
			} else {
				return (c - x) / (c - b);
			}
		} else {
			return 0;
		}
	}

	@Override
	public String toString() {
		return this.getClass().getSimpleName() +
		"(" + a + ", " + b + ", " + c + ")";
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		long temp;
		temp = Double.doubleToLongBits(a);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		temp = Double.doubleToLongBits(b);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		temp = Double.doubleToLongBits(c);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		FuzzySet other = (FuzzySet) obj;
		if (Double.doubleToLongBits(a) != Double.doubleToLongBits(other.a))
			return false;
		if (Double.doubleToLongBits(b) != Double.doubleToLongBits(other.b))
			return false;
		if (Double.doubleToLongBits(c) != Double.doubleToLongBits(other.c))
			return false;
		return true;
	}
}
