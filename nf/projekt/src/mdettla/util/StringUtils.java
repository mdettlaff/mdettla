package mdettla.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public final class StringUtils {

	private StringUtils() {}

	public static <T> String join(final List<T> input, final String separator) {
		final StringBuilder out = new StringBuilder("");
		if (!"".equals(input)) {
			for (int i = 0; i < input.size() - 1; i++) {
				out.append(input.get(i));
				out.append(separator);
			}
			out.append(input.get(input.size() - 1));
		}
		return out.toString();
	}

	public static <T> String join(final T[] input, final String separator) {
		return StringUtils.join(Arrays.asList(input), separator);
	}

	public static String join(final double[] input, final String separator) {
		List<Double> list = new ArrayList<Double>();
		for (double x : input) {
			list.add(x);
		}
		return StringUtils.join(list, separator);
	}
}
