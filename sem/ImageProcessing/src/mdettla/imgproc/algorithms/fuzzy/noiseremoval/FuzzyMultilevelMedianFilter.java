package mdettla.imgproc.algorithms.fuzzy.noiseremoval;

import java.awt.Color;
import java.awt.image.BufferedImage;
import java.util.Arrays;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;

import mdettla.imgproc.Util;
import mdettla.imgproc.algorithms.noiseremoval.MultilevelMedianFilter;

public class FuzzyMultilevelMedianFilter extends MultilevelMedianFilter {

	@Override
	public BufferedImage processImage(BufferedImage inputImage) {
		BufferedImage processedImage =
			new BufferedImage(inputImage.getWidth(), inputImage.getHeight(),
					BufferedImage.TYPE_INT_RGB);

		for (int y = MARGIN; y < inputImage.getHeight() - MARGIN; y++) {
			System.out.println("processing pixel row " + y);
			for (int x = MARGIN; x < inputImage.getWidth() - MARGIN; x++) {
				int q = Util.q(x, y, inputImage);
				Map<Integer, Double> s = s(W, x, y, inputImage);
				Iterator<Integer> it = s.keySet().iterator();
				int sMax1 = it.next();
				int sMax2 = it.hasNext() ? it.next() : sMax1;
				q = median(Arrays.asList(
						medMax(x, y, inputImage),
						medMin(x, y, inputImage),
						q,
						sMax1,
						sMax2));
				processedImage.setRGB(x, y, new Color(q, q, q).getRGB());
			}
		}
		return processedImage;
	}

	/**
	 * Funkcja przynależności zbioru rozmytego oznaczającego wiarygodność
	 * mediany.
	 */
	private static double credibilityMed(double u) {
		int a = 0;
		int b = 10;
		int c = 20;
		int d = 30;
		if (u < a || u > d) {
			return 0;
		} else if (a <= u && u <= b) {
			return (u - a) / (b - a);
		} else if (b <= u && u <= c) {
			return 1;
		} else if (c <= u && u <= d) {
			return (d - u) / (d - c);
		} else {
			throw new AssertionError("nieprawidłowa wartość u: " + u);
		}
	}

	private static double s(int[][] w, int med, int x, int y,
			BufferedImage image) {
		double cMedSum = 0;
		for (int i = 0; i < w.length; i++) {
			for (int j = 0; j < w.length; j++) {
				if (w[i][j] == 1) {
					int q = Util.q(
							x - w.length / 2 + j, y - w.length / 2 + i, image);
					cMedSum += credibilityMed((int)Math.abs(med - q));
				}
			}
		}
		return cMedSum;
	}

	private static Map<Integer, Double> s(int[][][] windows, int x, int y,
			BufferedImage image) {
		Map<Integer, Double> s = new TreeMap<Integer, Double>();
		for (int i = 0; i < windows.length; i++) {
			int med = med(windows[i], x, y, image);
			s.put(med, s(windows[i], med, x, y, image));
		}
		return s;
	}
}
