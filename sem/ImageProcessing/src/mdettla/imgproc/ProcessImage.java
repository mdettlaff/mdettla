package mdettla.imgproc;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;

/**
 * Stosuje wybrany algorytm przetwarzania obrazu do danego pliku graficznego
 * i zapisuje wynik do innego podanego pliku.
 */
public class ProcessImage {

	public static void main(String[] args) throws IOException {
		try {
			if (args.length < 3) {
				System.out.println("Użycie: java " +
						ProcessImage.class.getSimpleName() +
				"KLASA_ALGORYTMU PLIK_WEJŚCIOWY PLIK_WYJŚCIOWY");
				return;
			}
			File inputImage = new File(args[1]);
			File outputImage = new File(args[2]);
			BufferedImage image = ImageIO.read(inputImage);

			ImageProcessingAlgorithm algorithm =
				(ImageProcessingAlgorithm)Class.forName(args[0]).newInstance();

			image = algorithm.processImage(image);

			String outputFormatName = outputImage.getName().replaceAll(".*\\.", "");
			ImageIO.write(image, outputFormatName, outputImage);
			System.out.println("Zapisano do pliku: " + outputImage);
		} catch (InstantiationException e) {
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			e.printStackTrace();
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
		}
	}
}
