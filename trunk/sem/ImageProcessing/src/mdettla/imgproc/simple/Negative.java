package mdettla.imgproc.simple;

import java.awt.Color;
import java.awt.image.BufferedImage;

import mdettla.imgproc.ImageProcessingAlgorithm;
import mdettla.imgproc.Util;

/**
 * Negatyw.
 */
public class Negative implements ImageProcessingAlgorithm {

	@Override
	public BufferedImage processImage(BufferedImage inputImage) {
		BufferedImage processedImage =
			new BufferedImage(inputImage.getWidth(), inputImage.getHeight(),
				BufferedImage.TYPE_INT_RGB);

		for (int y = 1; y < inputImage.getHeight() - 1; y++) {
			for (int x = 1; x < inputImage.getWidth() - 1; x++) {
				int gray = 255 - (int)Util.componentY(inputImage.getRGB(x, y));
				processedImage.setRGB(x, y, new Color(gray, gray, gray).getRGB());
			}
		}
		return processedImage;
	}
}
