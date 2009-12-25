package mdettla.imgproc.stereogram;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;

import mdettla.imgproc.Util;

/**
 * Generator stereogramów.
 *
 * Generuje stereogram podanego obrazu dla podanego tła. Obraz traktowany
 * jest jako monochromatyczny, gdzie ciemniejsze obszary odpowiadają większej
 * wypukłości, a jaśniejesze mniejszej.
 */
public class StereogramGenerator {

	public static final int DEFAULT_DEPTH = 8;

	/**
	 * Tworzy stereogram.
	 *
	 * @param  mask       Obraz, który ma zostać ukryty w stereogramie.
	 *                    Jaśniejsze rejony oznaczają większą wypukłość.
	 * @param  background Tło stereogramu. Może być to np. szum.
	 * @param  maxDepth   Współczynnik głębokości. Im mniejsza wartość, tym
	 *                    bardziej płaski będzie stereogram. Nie może być
	 *                    mniejszy niż {@code 1}.
	 * @return            Obraz będący stereogramem.
	 */
	private static BufferedImage createStereogram(BufferedImage mask,
			BufferedImage background, int maxDepth) {
		if (maxDepth < 1) {
			throw new AssertionError(
					"Wartość maxDepth nie może być mniejsza niż 1.");
		}
		BufferedImage processedImage =
			new BufferedImage(mask.getWidth() * 2, mask.getHeight(),
				BufferedImage.TYPE_INT_RGB);

		// tworzymy lewą stronę, z przesunięciem na wypukłości
		for (int y = 0; y < mask.getHeight(); y++) {
			for (int x = 0; x < mask.getWidth(); x++) {
				// poziom jasności (składowa Y modelu YUV)
				int Y = (int)Util.componentY(mask.getRGB(x, y));
				// wypukłość jest odwrotnie proporcjonalna do jasności
				int convexity = (int)(maxDepth - ((double)Y) / 255 * maxDepth);
				// przesunięcie jest wprost proporcjonalne do wypukłości
				int shift = convexity;
				int maskX = (x - shift) % mask.getWidth();
				processedImage.setRGB(x, y, background.getRGB(maskX, y));
			}
		}

		// tworzymy prawą stronę, gdzie tło jest niezmienione
		for (int y = 0; y < mask.getHeight(); y++) {
			for (int x = 0; x < mask.getWidth(); x++) {
				processedImage.setRGB(x + mask.getWidth(), y,
						background.getRGB(x, y));
			}
		}
		return processedImage;
	}

	public static void main(String[] args) {
		try {
			if (args.length < 3) {
				System.out.println("Użycie: java " +
						StereogramGenerator.class.getSimpleName() +
				"PLIK_MASKI PLIK_TŁA PLIK_WYJŚCIOWY");
				return;
			}
			BufferedImage mask = ImageIO.read(new File(args[0]));
			BufferedImage background = ImageIO.read(new File(args[1]));
			File stereogramFile = new File(args[2]);

			BufferedImage stereogram = createStereogram(mask, background,
					DEFAULT_DEPTH);

			String outputFormatName =
				stereogramFile.getName().replaceAll(".*\\.", "");
			ImageIO.write(stereogram, outputFormatName, stereogramFile);
			System.out.println("Zapisano do pliku: " + stereogramFile);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
