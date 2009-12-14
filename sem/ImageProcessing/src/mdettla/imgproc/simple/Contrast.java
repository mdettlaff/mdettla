package mdettla.imgproc.simple;

import java.awt.Color;
import java.awt.image.BufferedImage;

import mdettla.imgproc.ImageProcessingAlgorithm;
import mdettla.imgproc.Util;

/**
 * Negatyw.
 */
public class Contrast implements ImageProcessingAlgorithm {

	@Override
	public BufferedImage processImage(BufferedImage inputImage) {
		BufferedImage processedImage =
			new BufferedImage(inputImage.getWidth(), inputImage.getHeight(),
				BufferedImage.TYPE_INT_RGB);

		for (int y = 1; y < inputImage.getHeight() - 1; y++) {
			for (int x = 1; x < inputImage.getWidth() - 1; x++) {
				int gray = (int)(1.5 * Util.componentY(inputImage.getRGB(x, y)));
				gray = Math.max(0, gray);
				gray = Math.min(255, gray);
				processedImage.setRGB(x, y, new Color(gray, gray, gray).getRGB());
			}
		}
		return processedImage;
	}
}
