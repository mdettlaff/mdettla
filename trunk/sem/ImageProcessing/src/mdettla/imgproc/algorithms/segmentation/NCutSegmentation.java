package mdettla.imgproc.algorithms.segmentation;

import java.util.Arrays;

import Jama.EigenvalueDecomposition;
import Jama.Matrix;

public class NCutSegmentation {

	static double[] getEigenvectorWithSecondSmallestEigenvalue(double[][] matrix) {
		Matrix M = new Matrix(matrix);
		EigenvalueDecomposition eigDec = new EigenvalueDecomposition(M);
		double[] eigenvalues = eigDec.getRealEigenvalues();
		int indexOfSecondSmallestEigenvalue = getIndexOfSecondSmallest(eigenvalues);
		Matrix eigenvectors = eigDec.getV();
		double[] secondSmallest =
			eigenvectors.getArray()[indexOfSecondSmallestEigenvalue];
		return secondSmallest;
	}

	static int getIndexOfSecondSmallest(double[] values) {
		if (values.length < 2) {
			throw new IllegalArgumentException(
					"the size of values parameter must be at least 2");
		}
		double[] sortedValues = Arrays.copyOf(values, values.length);
		Arrays.sort(sortedValues);
		double secondSmallest = sortedValues[1];
		for (int i = 0; i < values.length; i++) {
			if (secondSmallest == values[i]) {
				return i;
			}
		}
		throw new IllegalStateException("second smallest value does not exist");
	}

//	private static Double[] getWrapping(double[] array) {
//		Double[] result = new Double[array.length];
//		for (int i = 0; i < array.length; i++) {
//			result[i] = array[i];
//		}
//		return result;
//	}
}
