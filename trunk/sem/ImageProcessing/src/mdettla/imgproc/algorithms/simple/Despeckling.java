package mdettla.imgproc.algorithms.simple;

import java.awt.Color;
import java.awt.image.BufferedImage;

import mdettla.imgproc.ImageProcessingAlgorithm;
import mdettla.imgproc.Util;

/**
 * Negatyw.
 */
public class Despeckling implements ImageProcessingAlgorithm {

	@Override
	public BufferedImage processImage(BufferedImage inputImage) {
		BufferedImage processedImage =
			new BufferedImage(inputImage.getWidth(), inputImage.getHeight(),
				BufferedImage.TYPE_INT_RGB);

		for (int y = 1; y < inputImage.getHeight() - 1; y++) {
			for (int x = 1; x < inputImage.getWidth() - 1; x++) {
				int q = Util.q(x, y, inputImage);
				if (q == Util.COLOR_BLACK
						&& blackNeighborsCount(x, y, inputImage) == 0) {
					q = Util.COLOR_WHITE;
				}
				processedImage.setRGB(x, y, new Color(q, q, q).getRGB());
			}
		}
		return processedImage;
	}

	private static int blackNeighborsCount(int x, int y, BufferedImage image) {
		int[] neighbors = new int[] {
				Util.q(x - 1, y - 1, image),
				Util.q(x, y - 1, image),
				Util.q(x + 1, y - 1, image),
				Util.q(x - 1, y, image),
				Util.q(x + 1, y, image),
				Util.q(x - 1, y + 1, image),
				Util.q(x, y + 1, image),
				Util.q(x + 1, y + 1, image),
		};
		int blackNeighborsCount = 0;
		for (int i = 0; i < neighbors.length; i++) {
			if (neighbors[i] == Util.COLOR_BLACK) {
				blackNeighborsCount++;
			}
		}
		return blackNeighborsCount;
	}
}
