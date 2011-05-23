package mdettla.fuzzy;

import java.util.ArrayList;
import java.util.List;

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

	public static List<FuzzySet> coverRangeWithFuzzySets(
			double begin, double end, int fuzzySetsCount) {
		if (begin > end) {
			throw new IllegalArgumentException("Invalid range.");
		}
		double rangeWidth = end - begin;
		double halfFuzzySetWidth = rangeWidth / (fuzzySetsCount - 1);
		List<FuzzySet> fuzzySets = new ArrayList<FuzzySet>();
		fuzzySets.add(new FuzzySet(
				Double.NEGATIVE_INFINITY, begin, begin + halfFuzzySetWidth));
		for (double a = begin; a + 2 * halfFuzzySetWidth <= end; a += halfFuzzySetWidth) {
			double b = a + halfFuzzySetWidth;
			double c = b + halfFuzzySetWidth;
			fuzzySets.add(new FuzzySet(a, b, c));
		}
		fuzzySets.add(new FuzzySet(
				end - halfFuzzySetWidth, end, Double.POSITIVE_INFINITY));
		return fuzzySets;
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
