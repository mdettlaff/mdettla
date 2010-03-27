package mdettla.imgproc.algorithms.fuzzy.noiseremoval;

import java.awt.Color;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import mdettla.imgproc.ImageProcessingAlgorithm;

/**
 * From: Fuzzy Filters for Image Processing, chapter 1, section 2.1.
 */
public class MultipassFuzzyFilter implements ImageProcessingAlgorithm {

	/**
	 * Maximum luminance.
	 */
	private static final int L = 256;

	private static final int WINDOW_SIZE = 8;
	/**
	 * Windows configurations for eliminating impulse noise.
	 * 1 - black, 0 - white.
	 */
	private static final int[][] W = new int[12][];
	static {
		// shape 1
		W[0] = new int[] {
				0, 1, 0,
				1, 0, 0,
				0, 1, 0
		};
		W[1] = new int[] {
				0, 1, 0,
				1, 0, 1,
				0, 0, 0
		};
		W[2] = new int[] {
				0, 1, 0,
				0, 0, 1,
				0, 1, 0
		};
		W[3] = new int[] {
				0, 0, 0,
				1, 0, 1,
				0, 1, 0
		};
		// shape 2
		W[4] = new int[] {
				0, 0, 1,
				1, 0, 0,
				1, 1, 0
		};
		W[5] = new int[] {
				1, 1, 0,
				1, 0, 0,
				0, 0, 1
		};
		W[6] = new int[] {
				0, 1, 1,
				0, 0, 1,
				1, 0, 0
		};
		W[7] = new int[] {
				1, 0, 0,
				0, 0, 1,
				0, 1, 1
		};
		// shape 3
		W[8] = new int[] {
				1, 0, 0,
				1, 0, 0,
				1, 1, 1
		};
		W[9] = new int[] {
				1, 1, 1,
				1, 0, 0,
				1, 0, 0
		};
		W[10] = new int[] {
				1, 1, 1,
				0, 0, 1,
				0, 0, 1
		};
		W[11] = new int[] {
				0, 0, 1,
				0, 0, 1,
				1, 1, 1
		};
	}

	@Override
	public BufferedImage processImage(BufferedImage inputImage) {
		BufferedImage processedImage =
			new BufferedImage(inputImage.getWidth(), inputImage.getHeight(),
				BufferedImage.TYPE_INT_RGB);

		for (int y = 1; y < inputImage.getHeight() - 1; y++) {
			System.out.println("Processing pixel row " + y);
			for (int x = 1; x < inputImage.getWidth() - 1; x++) {
				int q = q(x, y, inputImage);
				int deltaR = (int)deltaR(x, y, inputImage);
				q = q + deltaR;
				q = Math.min(q, L - 1);
				q = Math.max(q, 0);
				processedImage.setRGB(x, y, new Color(q, q, q).getRGB());
			}
		}
		return processedImage;
	}

	/**
	 * Membership function of the fuzzy set LARGE.
	 */
	private static double membershipLarge(int u) {
		final int a = L / 4;
		final int b = L / 2;
		if (-L + 1 <= u && u < a) {
			return 0;
		} else if (a <= u && u < b) {
			return (b * (((double)u) - a)) / ((L - 1) * (b - a));
		} else if (b <= u && u <= L - 1) {
			return ((double)u) / (L - 1);
		} else {
			throw new AssertionError("nieprawidłowa wartość u: " + u);
		}
	}

	private static int q(int x, int y, BufferedImage image) {
		Color color = new Color(image.getRGB(x, y));
		return (int)((color.getRed() + color.getGreen() + color.getBlue()) / 3.0);
	}

	/**
	 * Differences in the luminance between each one of the dark neighbor
	 * pixels in each window and q(x, y).
	 *
	 * @return Array 8x12 of differences of luminance.
	 */
	private static Integer[][] deltaW(int x, int y, BufferedImage image) {
		Integer [][] deltaW = new Integer[WINDOW_SIZE][12];
		for (int i = 0; i < deltaW.length; i++) {
			deltaW[i] = new Integer[12];
			for (int j = 0; j < deltaW[i].length; j++) {
				if (W[j][i] == 1) {
					int deltaX = 0;
					switch (i) {
					case 0: case 6: case 7:
						deltaX = -1;
					case 2: case 3: case 4:
						deltaX = 1;
					}
					int deltaY = 0;
					switch (i) {
					case 0: case 1: case 2:
						deltaY = -1;
					case 4: case 5: case 6:
						deltaY = 1;
					}
					deltaW[i][j] =
						q(x + deltaX, y + deltaY, image) - q(x, y, image);
				}
			}
		}
		return deltaW;
	}

	private static double deltaR(int x, int y, BufferedImage image) {
		List<Double> arr0 = new ArrayList<Double>();
		Integer [][] deltaW = deltaW(x, y, image);
		for (int j = 0; j < W.length; j++) {
			List<Double> arr00 = new ArrayList<Double>();
			for (int i = 0; i < WINDOW_SIZE; i++) {
				if (deltaW[i][j] != null) {
					arr00.add(membershipLarge(deltaW[i][j]));
				}
			}
			arr0.add(Collections.min(arr00));
		}
		List<Double> arr1 = new ArrayList<Double>();
		for (int j = 0; j < W.length; j++) {
			List<Double> arr10 = new ArrayList<Double>();
			for (int i = 0; i < WINDOW_SIZE; i++) {
				if (deltaW[i][j] != null) {
					arr10.add(membershipLarge(-deltaW[i][j]));
				}
			}
			arr1.add(Collections.min(arr10));
		}
		return (L - 1) * (Collections.max(arr0) - Collections.max(arr1));
	}
}
