package mdettla.imgproc;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;

import mdettla.imgproc.fuzzy.edge.EdgeDetectionWithFuzzyClassifier;

/**
 * Stosuje wybrany algorytm przetwarzania obrazu do danego pliku graficznego
 * i zapisuje wynik do innego podanego pliku.
 */
public class ProcessImage {

	public static void main(String[] args) throws IOException {
		if (args.length < 2) {
			System.out.println("Użycie: java " +
					ProcessImage.class.getSimpleName() +
					" PLIK_WEJŚCIOWY PLIK_WYJŚCIOWY");
			return;
		}
		File inputImage = new File(args[0]);
		File outputImage = new File(args[1]);
		BufferedImage image = ImageIO.read(inputImage);

		ImageProcessingAlgorithm algorithm = new EdgeDetectionWithFuzzyClassifier();

		image = algorithm.processImage(image);

		String outputFormatName = outputImage.getName().replaceAll(".*\\.", "");
		ImageIO.write(image, outputFormatName, outputImage);
		System.out.println("Zapisano do pliku: " + outputImage);
	}
}
