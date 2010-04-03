package mdettla.imgproc.algorithms.noiseremoval;

import java.awt.Color;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import mdettla.imgproc.ImageProcessingAlgorithm;
import mdettla.imgproc.Util;

public class MultilevelMedianFilter implements ImageProcessingAlgorithm {

	private static final int MARGIN = 2;

	private static int[][][] W = {
		{
			{0, 0, 1, 0, 0},
			{0, 0, 1, 0, 0},
			{0, 0, 1, 0, 0},
			{0, 0, 1, 0, 0},
			{0, 0, 1, 0, 0},
		},
		{
			{1, 0, 0, 0, 0},
			{0, 1, 0, 0, 0},
			{0, 0, 1, 0, 0},
			{0, 0, 0, 1, 0},
			{0, 0, 0, 0, 1},
		},
		{
			{0, 0, 0, 0, 0},
			{0, 0, 0, 0, 0},
			{1, 1, 1, 1, 1},
			{0, 0, 0, 0, 0},
			{0, 0, 0, 0, 0},
		},
		{
			{0, 0, 0, 0, 1},
			{0, 0, 0, 1, 0},
			{0, 0, 1, 0, 0},
			{0, 1, 0, 0, 0},
			{1, 0, 0, 0, 0},
		}
	};

	@Override
	public BufferedImage processImage(BufferedImage inputImage) {
		BufferedImage processedImage =
			new BufferedImage(inputImage.getWidth(), inputImage.getHeight(),
					BufferedImage.TYPE_INT_RGB);

		for (int y = MARGIN; y < inputImage.getHeight() - MARGIN; y++) {
			for (int x = MARGIN; x < inputImage.getWidth() - MARGIN; x++) {
				int q = Util.q(x, y, inputImage);
				q = median(Arrays.asList(
						medMax(x, y, inputImage),
						medMin(x, y, inputImage),
						q));
				processedImage.setRGB(x, y, new Color(q, q, q).getRGB());
			}
		}
		return processedImage;
	}

	private static Integer median(List<Integer> list) {
		Collections.sort(list);
		return list.get(list.size() / 2);
	}

	/**
	 * Mediana z pikseli z podanego okna.
	 */
	private static int med(int[][] w, int x, int y, BufferedImage image) {
		List<Integer> windowPixels = new ArrayList<Integer>();
		for (int i = 0; i < w.length; i++) {
			for (int j = 0; j < w.length; j++) {
				if (w[i][j] == 1) {
					windowPixels.add(Util.q(
							x - w.length / 2 + j,
							y - w.length / 2 + i,
							image));
				}
			}
		}
		return median(windowPixels);
	}

	private static int medMin(int x, int y, BufferedImage image) {
		List<Integer> medians = new ArrayList<Integer>();
		for (int i = 0; i < W.length; i++) {
			medians.add(med(W[i], x, y, image));
		}
		return Collections.min(medians);
	}

	private static int medMax(int x, int y, BufferedImage image) {
		List<Integer> medians = new ArrayList<Integer>();
		for (int i = 0; i < W.length; i++) {
			medians.add(med(W[i], x, y, image));
		}
		return Collections.max(medians);
	}
}
