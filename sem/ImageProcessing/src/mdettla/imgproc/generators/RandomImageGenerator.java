package mdettla.imgproc.generators;

import java.awt.Color;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Random;

import javax.imageio.ImageIO;

/**
 * Tworzy obraz wypełniony losowo czarnymi i białymi pikselami.
 */
public class RandomImageGenerator {

	private static BufferedImage createRandomImage(int width, int height) {
		Random random = new Random();
		BufferedImage randomImage =
			new BufferedImage(width, height, BufferedImage.TYPE_BYTE_BINARY);

		for (int y = 0; y < height; y++) {
			for (int x = 0; x < width; x++) {
				randomImage.setRGB(x, y, random.nextInt(2) > 0 ?
						Color.WHITE.getRGB() : Color.BLACK.getRGB());
			}
		}
		return randomImage;
	}

	public static void main(String[] args) {
		try {
			if (args.length < 1) {
				System.out.println("Użycie: java " +
						RandomImageGenerator.class.getName() +
						" PLIK_WYJŚCIOWY");
				System.exit(2);
			}
			File outputFile = new File(args[0]);

			BufferedImage randomImage = createRandomImage(256, 256);

			String outputFormatName =
				outputFile.getName().replaceAll(".*\\.", "");
			ImageIO.write(randomImage, outputFormatName, outputFile);
			System.out.println("Zapisano do pliku: " + outputFile);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
