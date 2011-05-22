package mdettla.fuzzy;

public class FuzzySet {

	private double a;
	private double b;
	private double c;

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
}
