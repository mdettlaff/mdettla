package mdettla.jga.core;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class Utils {

	public static <T> List<T> randomSample(List<T> population, int k) {
		Random random = new Random();
		List<Integer> indexes = new ArrayList<Integer>(population.size());
		for (int i = 0; i < population.size(); i++) {
			indexes.add(i);
		}
		List<T> sample = new ArrayList<T>();
		for (int i = 0; i < k; i++) {
			Integer index = indexes.get(random.nextInt(indexes.size()));
			indexes.remove(index);
			sample.add(population.get(index));
		}
		return sample;
	}

	public static List<Integer> range(int n) {
		List<Integer> range = new ArrayList<Integer>(n);
		for (int i = 0; i < n; i++) {
			range.add(i);
		}
		return range;
	}

	public static List<Integer> range(int a, int b) {
		List<Integer> range = new ArrayList<Integer>(b - a);
		for (int i = a; i < b; i++) {
			range.add(i);
		}
		return range;
	}

	public static <T extends Number> double vectorDiff(List<T> v1, List<T> v2) {
		if (v1.size() != v2.size()) {
			throw new IllegalArgumentException(
					"Vectors must be of equal sizes: v1 = " + v1 + ", v2 = " + v2);
		}
		double diff = 0;
		for (int i = 0; i < v1.size(); i++) {
			diff += Math.pow(v1.get(i).doubleValue() - v2.get(i).doubleValue(), 2);
		}
		return diff;
	}

	public static Character replacePolishChar(char c) {
		String plChars = "ąęćłóśńżźĄĘĆŁÓŚŃŻŹ";
		String latinChars = "aeclosnzzAECLOSNZZ";
		int plCharsIndex = plChars.indexOf(c);
		return plCharsIndex != -1 ? latinChars.charAt(plCharsIndex) : c;
	}

	static String replacePolishChars(String text) {
		StringBuilder builder = new StringBuilder();
		for (int i = 0; i < text.length(); i++) {
			builder.append(replacePolishChar(text.charAt(i)));
		}
		return builder.toString();
	}

	public static <T> List<List<T>> partition(List<T> list, int partitionsCount) {
		int partitionSize = list.size() / partitionsCount;
		List<List<T>> partitions = new ArrayList<List<T>>(partitionsCount);
		for (int i = 0; i < partitionsCount - 1; i++) {
			int start = i * partitionSize;
			partitions.add(list.subList(start, start + partitionSize));
		}
		int start = (partitionsCount - 1) * partitionSize;
		partitions.add(list.subList(start, list.size()));
		return partitions;
	}
}
