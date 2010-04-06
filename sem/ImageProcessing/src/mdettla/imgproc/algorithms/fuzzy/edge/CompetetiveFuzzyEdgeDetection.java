package mdettla.imgproc.algorithms.fuzzy.edge;

import java.awt.Color;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import mdettla.imgproc.ImageProcessingAlgorithm;
import mdettla.imgproc.Util;
import mdettla.imgproc.algorithms.simple.Despeckling;

/**
 * Z artykułu:
 * Competitive fuzzy edge detection
 * Lily Rui Liang, Carl G. Looney
 */
public class CompetetiveFuzzyEdgeDetection implements ImageProcessingAlgorithm {

	public static final int LO = 5;
	public static final int HI = 30;
	public static final int W = 225;

	public static final int CLASS_BACKGROUND = 0;
	public static final int CLASS_EDGE_1 = 1; // edge diagonal nw-se
	public static final int CLASS_EDGE_2 = 2; // edge vertical
	public static final int CLASS_EDGE_3 = 3; // edge diagonal ne-sw
	public static final int CLASS_EDGE_4 = 4; // edge horizontal
	public static final int CLASS_SPECK_EDGE = 5;
	private static final int FUZZY_CLASSES_COUNT = 6;

	private static final int DIRECTION_NWSE = 0;
	private static final int DIRECTION_VERTICAL = 1;
	private static final int DIRECTION_NESW = 2;
	private static final int DIRECTION_HORIZONTAL = 3;
	private static final int DIRECTIONS_COUNT = 4;

	/**
	 * Feature vectors dla poszczególnych klas.
	 */
	private static final int[][] c = new int[FUZZY_CLASSES_COUNT][];
	static {
		c[CLASS_BACKGROUND] = new int[] {LO, LO, LO, LO};
		c[CLASS_EDGE_1] = new int[] {LO, HI, HI, HI};
		c[CLASS_EDGE_2] = new int[] {HI, LO, HI, HI};
		c[CLASS_EDGE_3] = new int[] {HI, HI, LO, HI};
		c[CLASS_EDGE_4] = new int[] {HI, HI, HI, LO};
		c[CLASS_SPECK_EDGE] = new int[] {HI, HI, HI, HI};
	};

	private static final int MARGIN = 2;

	/**
	 * Klasy (od 0 do 5) przypisane do każdego piksela obrazu.
	 */
	private int[][] pixelClasses;

	@Override
	public BufferedImage processImage(BufferedImage inputImage) {
		BufferedImage processedImage =
			new BufferedImage(inputImage.getWidth(), inputImage.getHeight(),
				BufferedImage.TYPE_INT_RGB);
		pixelClasses = pixelClasses(inputImage);
		for (int y = MARGIN; y < inputImage.getHeight() - MARGIN; y++) {
			for (int x = MARGIN; x < inputImage.getWidth() - MARGIN; x++) {
				int q = Util.q(x, y, inputImage);
				q = compete(pixelClasses, x, y, inputImage);
				processedImage.setRGB(x, y, new Color(q, q, q).getRGB());
			}
		}
		processedImage = new Despeckling().processImage(processedImage);
		return processedImage;           
	}

	private static int[][] pixelClasses(BufferedImage image) {
		int[][]	 pixelClasses = new int[image.getHeight()][];
		for (int y = 1; y < image.getHeight() - 1; y++) {
			pixelClasses[y] = new int[image.getWidth()];
			for (int x = 1; x < image.getWidth() - 1; x++) {
				pixelClasses[y][x] = pixelClass(x, y, image);
			}
		}
		return pixelClasses;
	}

	private static double fuzzyClassMembership(
			int[] featureVector, int[] classFeatureVector) {
		double vectorModule = 0;
		for (int i = 0; i < classFeatureVector.length; i++) {
			vectorModule += Math.pow(
					featureVector[i] - classFeatureVector[i], 2.0);
		}
		vectorModule = Math.sqrt(vectorModule);
		double fuzzyTruth = 1 - Math.pow(vectorModule, 2.0) / Math.pow(W, 2.0);
		fuzzyTruth = Math.max(0, fuzzyTruth);
		return fuzzyTruth;
	}

	private static int pixelClass(int x, int y, BufferedImage image) {
		List<Double> fuzzyTruthValues = new ArrayList<Double>(c.length);
		int[] featureVector = featureVector(x, y, image);
		for (int i = 0; i < c.length; i++) {
			fuzzyTruthValues.add(fuzzyClassMembership(featureVector, c[i]));
		}
		double greatestFuzzyTruth = Collections.max(fuzzyTruthValues);
		int classWithGreatestFuzzyTruth =
			fuzzyTruthValues.indexOf(greatestFuzzyTruth);
		return classWithGreatestFuzzyTruth;
	}

	private static int[] featureVector(int x, int y, BufferedImage image) {
		int[] featureVector = new int[4];
		for (int i = 0; i < DIRECTIONS_COUNT; i++) {
			featureVector[i] = d(i, x, y, image);
		}
		return featureVector;
	}

	/**
	 * @return Difference in magnitude in given direction.
	 */
	private static int d(int direction, int x, int y, BufferedImage image) {
		int q = Util.q(x, y, image);
		switch (direction) {
		case DIRECTION_NWSE:
			return Math.abs(Util.q(x - 1, y - 1, image) - q)
					+ Math.abs(Util.q(x + 1, y + 1, image) - q); // direction 1
		case DIRECTION_VERTICAL:
			return Math.abs(Util.q(x, y - 1, image) - q)
					+ Math.abs(Util.q(x, y + 1, image) - q); // direction 2
		case DIRECTION_NESW:
			return Math.abs(Util.q(x + 1, y - 1, image) - q)
					+ Math.abs(Util.q(x - 1, y + 1, image) - q); // direction 3
		case DIRECTION_HORIZONTAL:
			return Math.abs(Util.q(x - 1, y, image) - q)
					+ Math.abs(Util.q(x + 1, y, image) - q); // direction 4
		default:
			throw new AssertionError(
					"Niedozwolony kierunek (musi być z zakresu 0-3).");
		}
	}

	private static int compete(int direction,
			int x, int y, BufferedImage image) {
		// the rule is: compete dN with neighbor pixels in Direction N
		int magnitudeDiff = d(direction, x, y, image);
		int magnitudeDiffNeighbor0;
		int magnitudeDiffNeighbor1;
		switch (direction) {
		case DIRECTION_NWSE:
			magnitudeDiffNeighbor0 = d(direction, x - 1, y - 1, image);
			magnitudeDiffNeighbor1 = d(direction, x + 1, y + 1, image);
			break;
		case DIRECTION_VERTICAL:
			magnitudeDiffNeighbor0 = d(direction, x, y - 1, image);
			magnitudeDiffNeighbor1 = d(direction, x, y + 1, image);
			break;
		case DIRECTION_NESW:
			magnitudeDiffNeighbor0 = d(direction, x + 1, y - 1, image);
			magnitudeDiffNeighbor1 = d(direction, x - 1, y + 1, image);
			break;
		case DIRECTION_HORIZONTAL:
			magnitudeDiffNeighbor0 = d(direction, x - 1, y, image);
			magnitudeDiffNeighbor1 = d(direction, x + 1, y, image);
			break;
		default:
			throw new AssertionError(
					"Niedozwolony kierunek (musi być z zakresu 0-3).");
		}
		if (magnitudeDiff > magnitudeDiffNeighbor0
				&& magnitudeDiff > magnitudeDiffNeighbor1) {
			return Util.COLOR_BLACK;
		} else {
			return Util.COLOR_WHITE;
		}
	}

	private static int compete(int[][] pixelClasses,
			int x, int y, BufferedImage image) {
		// rules for competition
		switch (pixelClasses[y][x]) {
		case CLASS_BACKGROUND:
			return Util.COLOR_WHITE;
		case CLASS_EDGE_1:
			return compete(DIRECTION_NESW, x, y, image);
		case CLASS_EDGE_2:
			return compete(DIRECTION_HORIZONTAL, x, y, image);
		case CLASS_EDGE_3:
			return compete(DIRECTION_NWSE, x, y, image);
		case CLASS_EDGE_4:
			return compete(DIRECTION_VERTICAL, x, y, image);
		case CLASS_SPECK_EDGE:
			return Util.COLOR_BLACK;
		default:
			throw new AssertionError(
					"Niedozwolona rozmyta klasa (musi być z zakresu 0-5).");
		}
	}
}
