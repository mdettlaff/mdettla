package mdettla.fuzzy;

public class Range {

	private final double begin;
	private final double end;

	public Range(double begin, double end) {
		if (begin > end) {
			throw new IllegalArgumentException("Invalid range");
		}
		this.begin = begin;
		this.end = end;
	}

	public double getBegin() {
		return begin;
	}

	public double getEnd() {
		return end;
	}

	public double getWidth() {
		return end - begin;
	}
}
