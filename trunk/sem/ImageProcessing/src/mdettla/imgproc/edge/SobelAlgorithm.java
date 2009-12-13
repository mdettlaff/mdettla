package mdettla.imgproc.edge;

import java.awt.Color;
import java.awt.image.BufferedImage;

import mdettla.imgproc.ImageProcessingAlgorithm;
import mdettla.imgproc.Util;

/**
 * Algorytm wykrywania krawędzi w obrazie metodą Sobela.
 */
public class SobelAlgorithm implements ImageProcessingAlgorithm {

	@Override
	public BufferedImage processImage(BufferedImage inputImage) {
		BufferedImage processedImage =
			new BufferedImage(inputImage.getWidth(), inputImage.getHeight(),
				BufferedImage.TYPE_INT_RGB);

		// maska Sobela dla osi X
		final int[][] GX = {
				{-1, 0, 1},
				{-2, 0, 2},
				{-1, 0, 1}
		};
		// maska Sobela dla osi Y
		final int[][] GY = {
				{1, 2, 1},
				{0, 0, 0},
				{-1, -2, -1},
		};
		for (int y = 1; y < inputImage.getHeight() - 1; y++) {
			for (int x = 1; x < inputImage.getWidth() - 1; x++) {
				int sumX = 0;
				int sumY = 0;
				// obliczamy wartości gradientów X i Y
				for (int j = -1; j <= 1; j++) {
					for (int i = -1; i <= 1; i++) {
						sumX +=
							((int)Util.componentY(inputImage.getRGB(x + i, y + j))) * 
							GX[j + 1][i + 1];
						sumY +=
							((int)Util.componentY(inputImage.getRGB(x + i, y + j))) * 
							GY[j + 1][i + 1];
					}
				}
				int sum = Math.abs(sumX) + Math.abs(sumY);
				sum = Math.max(0, sum);
				sum = Math.min(255, sum);
				sum = 255 - sum;
				int grayLevel = new Color(sum, sum, sum).getRGB();
				processedImage.setRGB(x, y, grayLevel);
			}
		}
		return processedImage;
	}
}
