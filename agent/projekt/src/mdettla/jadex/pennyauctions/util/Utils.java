package mdettla.jadex.pennyauctions.util;

public class Utils {

	public static String formatPrice(Integer grosze) {
		return String.format("%d,%02d", grosze / 100, grosze % 100);
	}
}
