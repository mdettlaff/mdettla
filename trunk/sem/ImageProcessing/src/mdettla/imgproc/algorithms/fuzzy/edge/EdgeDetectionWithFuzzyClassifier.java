package mdettla.imgproc.algorithms.fuzzy.edge;

import java.awt.Color;
import java.awt.image.BufferedImage;

import mdettla.imgproc.ImageProcessingAlgorithm;
import mdettla.imgproc.Util;

public class EdgeDetectionWithFuzzyClassifier implements ImageProcessingAlgorithm{

	public static final int LOW = 6;
	public static final int HIGH = 15;
	public static final int OMEGA = 80000;

	private int[] calculateFeatureVector(BufferedImage image, int x, int y) {
		int[] featureVector = new int[9];
		int pos = 0;
		double gray = Util.componentY(image.getRGB(x, y));                   
		for (int i = -1; i < 2; i++) {
			for (int j = -1; j < 2; j++) {
				if (!(i == 0 && j == 0)) {
					double grayAround = Util.componentY(image.getRGB(x + j, y + i));
					featureVector[pos++] = (int)Math.abs(gray - grayAround);
				}
			}
		}
		return featureVector;
	}    

	private double calculateBackgroundClass(int[] featureVector) {
		double fuzzyTruth;
		if ((featureVector[0] <= LOW) && (featureVector[1] <= LOW)
				&& (featureVector[2] <= LOW) && (featureVector[3] <= LOW)
				&& (featureVector[4] <= LOW) && (featureVector[5] <= LOW)
				&& (featureVector[6] <= LOW) && (featureVector[7] <= LOW)) {
			fuzzyTruth = 1;
		} else {
			int[] ft = new int[8];
			for (int i = 0; i < 8; i++) {
				ft[i] = (int)Math.abs(featureVector[i] - LOW);
			}
			int f = 0;
			for (int i = 0; i < 8; i++) {
				f += (int)Math.pow(ft[i], 2);
			}
			fuzzyTruth = calculateFuzzyTruthValue(Math.sqrt(f));
		}
		return fuzzyTruth;
	}

	private double calculateEdgeClass(int[] featureVector) {
		double fuzzyTruth;
		if ((featureVector[0] >= HIGH) && (featureVector[1] >= HIGH)
				&& (featureVector[2] >= HIGH) && (featureVector[3] >= HIGH)
				&& (featureVector[4] >= HIGH) && (featureVector[5] >= HIGH)
				&& (featureVector[6] >= HIGH) && (featureVector[7] >= HIGH)) {
			fuzzyTruth = 1;
		} else {
			int[] ft = new int[8];
			for (int i = 0; i < 8; i++) {
				ft[i] = (int)Math.abs(featureVector[i] - HIGH);
			}
			int f = 0;
			for (int i = 0; i < 8; i++) {
				f += (int)Math.pow(ft[i], 2);
			}
			fuzzyTruth = calculateFuzzyTruthValue(Math.sqrt(f));
		}
		return fuzzyTruth;
	}

	private double calculateFuzzyTruthValue(double X) {
		double fuzzyTruth = 1 - X / Math.pow(OMEGA, 2.0);
		if (fuzzyTruth > 0) {
			return fuzzyTruth;
		} else {
			return 0;
		}
	}

	public BufferedImage processImage(BufferedImage inputImage) {
		BufferedImage processedImage =
			new BufferedImage(inputImage.getWidth(), inputImage.getHeight(),
				BufferedImage.TYPE_INT_RGB);
		for (int y = 1; y < inputImage.getHeight() - 1; y++) {
			for (int x = 1; x < inputImage.getWidth() - 1; x++) {
				int[] featureVector =  calculateFeatureVector(inputImage, x, y);
				double fuzzyTruth1 = calculateEdgeClass(featureVector);
				double fuzzyTruth2 = calculateBackgroundClass(featureVector);
				Color gray;
				if (fuzzyTruth1 > fuzzyTruth2) {
					gray = Color.BLACK;
				} else {
					gray = Color.WHITE;
				}
				processedImage.setRGB(x, y, gray.getRGB());
			}
		}
		return processedImage;           
	}
}
