package mdettla.jga.core;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class Utils {

	public synchronized static <T> List<T> randomSample(List<T> population, int k) {
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

	public synchronized static List<Integer> range(int n) {
		List<Integer> range = new ArrayList<Integer>(n);
		for (int i = 0; i < n; i++) {
			range.add(i);
		}
		return range;
	}

	public synchronized static List<Integer> range(int a, int b) {
		List<Integer> range = new ArrayList<Integer>(b - a);
		for (int i = a; i < b; i++) {
			range.add(i);
		}
		return range;
	}
}
