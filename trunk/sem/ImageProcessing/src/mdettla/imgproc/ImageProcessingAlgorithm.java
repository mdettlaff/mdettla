package mdettla.imgproc;

import java.awt.image.BufferedImage;

public interface ImageProcessingAlgorithm {

	/**
	 * @param  inputImage Obraz wejściowy. Nie powinien zostać zmodyfikowany
	 *                    przez algorytm.
	 * @return            Obraz wynikowy, powstały po zastosowaniu
	 *                    przekształcenia.
	 */
	public BufferedImage processImage(BufferedImage inputImage);
}
