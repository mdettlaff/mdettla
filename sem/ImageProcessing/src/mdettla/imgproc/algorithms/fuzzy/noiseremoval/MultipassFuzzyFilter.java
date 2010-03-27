package mdettla.imgproc.algorithms.fuzzy.noiseremoval;

import java.awt.Color;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import mdettla.imgproc.ImageProcessingAlgorithm;
import mdettla.imgproc.Util;

/**
 * From: Fuzzy Filters for Image Processing, chapter 1, section 2.1.
 */
public class MultipassFuzzyFilter implements ImageProcessingAlgorithm {

	/**
	 * Maximum luminance.
	 */
	private static final int L = 256;

	@Override
	public BufferedImage processImage(BufferedImage inputImage) {
		BufferedImage processedImage;
		processedImage = new FuzzyBlock1().processImage(inputImage);
		processedImage = new FuzzyBlock2().processImage(processedImage);
		return processedImage;
	}


	private static class FuzzyBlock1 implements ImageProcessingAlgorithm {

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
					int q = Util.q(x, y, inputImage);
					int deltaR = (int)deltaR(x, y, inputImage);
					q = q + deltaR;
					processedImage.setRGB(x, y, new Color(q, q, q).getRGB());
				}
			}
			return processedImage;
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

		/**
		 * Differences in the luminance between each one of the dark neighbor
		 * pixels in each window and q(x, y).
		 *
		 * @return Array 8x12 of differences of luminance.
		 */
		private static Integer[][] deltaW(int x, int y, BufferedImage image) {
			Integer[][] deltaW = new Integer[WINDOW_SIZE][12];
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
							Util.q(x + deltaX, y + deltaY, image) - Util.q(x, y, image);
					}
				}
			}
			return deltaW;
		}
	}

	private static class FuzzyBlock2 implements ImageProcessingAlgorithm {

		private static final int c = L / 6;
		private static final int WINDOW_SIZE = 5;

		@Override
		public BufferedImage processImage(BufferedImage inputImage) {
			BufferedImage processedImage =
				new BufferedImage(inputImage.getWidth(), inputImage.getHeight(),
						BufferedImage.TYPE_INT_RGB);
			for (int y = 2; y < inputImage.getHeight() - 2; y++) {
				System.out.println("Processing pixel row " + y);
				for (int x = 2; x < inputImage.getWidth() - 2; x++) {
					int q = Util.q(x, y, inputImage);
					int deltaS = (int)deltaS(x, y, inputImage);
					q = q + deltaS;
					processedImage.setRGB(x, y, new Color(q, q, q).getRGB());
				}
			}
			return processedImage;
		}

		/**
		 * Membership function of the fuzzy set SMALL.
		 */
		private static double membershipSmall(int u) {
			if (-L + 1 <= u && u < -c) {
				return 0;
			} else if (-c <= u && u < c) {
				return (((double)u) + c) / (2 * c);
			} else if (c <= u && u < 3 * c) {
				return (3 * c - ((double)u)) / (2 * c);
			} else if (3 * c <= u && u <= L - 1) {
				return 0;
			} else {
				throw new AssertionError("nieprawidłowa wartość u: " + u);
			}
		}

		private static Integer[][] deltaW(int x, int y, BufferedImage image) {
			Integer[][] deltaW = new Integer[WINDOW_SIZE][];
			for (int i = 0; i < deltaW.length; i++) {
				deltaW[i] = new Integer[WINDOW_SIZE];
				for (int j = 0; j < deltaW[i].length; j++) {
					int deltaX = j - WINDOW_SIZE / 2;
					int deltaY = i - WINDOW_SIZE / 2;
					deltaW[i][j] =
						Util.q(x + deltaX, y + deltaY, image) - Util.q(x, y, image);
				}
			}
			return deltaW;
		}

		private static double deltaS(int x, int y, BufferedImage image) {
			Integer[][] deltaW = deltaW(x, y, image);
			double sum1 = 0;
			double sum2 = 0;
			for (int i = 0; i < WINDOW_SIZE; i++) {
				for (int j = 0; j < WINDOW_SIZE; j++) {
					sum1 += membershipSmall(deltaW[i][j]);
					sum2 += membershipSmall(-deltaW[i][j]);
				}
			}
			return ((double)c) / 20 * (sum1 - sum2);
		}
	}
}
