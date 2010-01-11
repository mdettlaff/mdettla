package mdettla.ea.zadanie2;

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
}
