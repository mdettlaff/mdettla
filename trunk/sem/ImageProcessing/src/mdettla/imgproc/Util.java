package mdettla.imgproc;

import java.awt.Color;
import java.awt.image.BufferedImage;

public class Util {

	/**
	 * Oblicza składową Y z modelu YUV dla danego koloru.
	 * Składowa Y reprezentuje poziom jasności (luma).
	 *
	 * @param  rgb Kolor w modelu RGB.
	 * @return     Składowa Y w modelu YUV.
	 */
	public static double componentY(int rgb) {
		Color color = new Color(rgb);
		return
		0.299 * color.getRed() +
		0.587 * color.getGreen() +
		0.114 * color.getBlue() +
		0.5;
	}

	public static int q(int x, int y, BufferedImage image) {
		Color color = new Color(image.getRGB(x, y));
		return (int)((color.getRed() + color.getGreen() + color.getBlue()) / 3.0);
	}
}
