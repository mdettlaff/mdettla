package mdettla.imgproc.algorithms.segmentation;

import java.awt.image.BufferedImage;
import java.awt.Color;
import java.awt.Point;

import mdettla.imgproc.ImageProcessingAlgorithm;
import mdettla.imgproc.Util;

public class WatershedSegmentation implements ImageProcessingAlgorithm {

	private enum PixelState {
		SURFACE,
		FLOODED,
		DAM,
		TO_FLOOD; // kandydat do zatopienia
	}

	@Override
	public BufferedImage processImage(BufferedImage inputImage) {
		PixelState[][] C = new PixelState[inputImage.getWidth()][inputImage.getHeight()];
		C = getT(1, inputImage);
		for (int n = 1; n < 255 + 2; n++) {
			System.out.println("flooding level: " + n);
			C = nextLayer(n, C, inputImage);
		}
		return inputImage;
	}

	private PixelState[][] nextLayer(
			int n, PixelState[][] prevC, BufferedImage image) {
		PixelState[][] C = getT(n, image);
		// TODO dobrać kryterium zamiast ustalonej ilości powtórzeń
		for (int i = 0; i < 10; i++) {
			for (int y = 2; y < image.getHeight() - 2; y++) {
				for (int x = 2; x < image.getWidth() - 2; x++) {
					if (prevC[x][y] == PixelState.FLOODED) {
						// najpierw sąsiedzi będący bliżej, następnie dalej
						Point[] neighbors = new Point[] {
								new Point(x - 1, y), new Point(x + 1, y),
								new Point(x, y - 1), new Point(x, y + 1),
								new Point(x - 1, y - 1), new Point(x + 1, y - 1),
								new Point(x - 1, y + 1), new Point(x + 1, y + 1)
						};
						for (Point p : neighbors) {
							if (prevC[p.x][p.y] == PixelState.SURFACE
									&& C[p.x][p.y] == PixelState.FLOODED) {
								if (wouldCauseMerging(p.x, p.y, prevC)) {
									C[p.x][p.y] = PixelState.DAM;
									prevC[p.x][p.y] = PixelState.DAM;
								} else {
									prevC[p.x][p.y] = PixelState.TO_FLOOD;
								}
							}
						}
					}
				}
			}
			for (int y = 2; y < image.getHeight() - 2; y++) {
				for (int x = 2; x < image.getWidth() - 2; x++) {
					if (prevC[x][y] == PixelState.TO_FLOOD) {
						prevC[x][y]	= PixelState.FLOODED;
					}
				}
			}
		}
		for (int y = 2; y < image.getHeight() - 2; y++) {
			for (int x = 2; x < image.getWidth() - 2; x++) {
				if (C[x][y] == PixelState.DAM) {
					C[x][y]	= PixelState.SURFACE;
					image.setRGB(x, y, Color.WHITE.getRGB());
				}
			}
		}
		return C;
	}

	/**
	 * Czy zatopienie piksela na podanej pozycji spowodowałoby złączenie się
	 * dwóch obszarów.
	 */
	private boolean wouldCauseMerging(int x, int y, PixelState[][] C) {
		return isOccupied(C[x - 1][y]) && isOccupied(C[x + 1][y])
			|| isOccupied(C[x][y - 1]) && isOccupied(C[x][y + 1]);
	}

	private boolean isOccupied(PixelState ps) {
		return ps == PixelState.FLOODED || ps == PixelState.TO_FLOOD;
	}

	private PixelState[][] getT(int n, BufferedImage image) {
		PixelState[][] T = new PixelState[image.getWidth()][image.getHeight()];
		for (int y = 0; y < image.getHeight(); y++) {
			for (int x = 0; x < image.getWidth(); x++) {
				int intensity = (int)Util.componentY(image.getRGB(x, y));
				T[x][y] = intensity < n ? PixelState.FLOODED : PixelState.SURFACE;
			}
		}
		return T;
	}
}
