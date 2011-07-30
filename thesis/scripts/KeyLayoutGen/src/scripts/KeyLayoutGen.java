package scripts;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

import javax.imageio.ImageIO;

public class KeyLayoutGen {

	private final int KEY_WIDTH = 37;
	private final int KEY_HEIGHT = KEY_WIDTH;
	private final int KEY_X_PADDING = 8;
	private final int KEY_Y_PADDING = 5;
	private final int KEY_Y_TOP_PADDING = 3;
	private final int KEY_Y_BOTTOM_PADDING = 18;
	private final Point MARGIN = new Point(1, 12);
	private final int[] ROWS_X_MARGINS = {
			MARGIN.x, MARGIN.x + 55,
			MARGIN.x + 64, MARGIN.x + 83
	};
	private final int[] ROWS_Y_MARGINS = {
			MARGIN.y, MARGIN.y + KEY_HEIGHT,
			MARGIN.y + KEY_HEIGHT * 2, MARGIN.y + KEY_HEIGHT * 3
	};

	private final List<String> layout;

	public KeyLayoutGen(List<String> rawInput) {
		layout = createLayout(rawInput);
		System.out.println(layout);
	}

	public static void main(String[] args) throws IOException {
		if (args.length < 1) {
			System.err.println("Użycie: java " +
					KeyLayoutGen.class.getName() + " PLIK_WYJŚCIOWY");
			System.exit(2);
		}
		String filename = args[0];
		KeyLayoutGen generator = new KeyLayoutGen(getRawInput());
		generator.createKeyboardImage(filename);
	}

	private static List<String> getRawInput() {
		List<String> rawInput = new ArrayList<String>();
		Scanner scanner = new Scanner(System.in);
		while (scanner.hasNext()) {
			rawInput.add(scanner.nextLine());
		}
		return rawInput;
	}

	private List<String> createLayout(List<String> rawInput) {
		validateInput(rawInput);
		List<String> layout = new ArrayList<String>();
		layout.add("~`!1@2#3$4%5^6&7*8(9)0_-+=");
		for (String line : rawInput) {
			String row = line.replaceAll("\\s", "");
			if (row.contains(":")) {
				row = row.replace(":", ":;");
			} else {
				row = row.replace(";", ":;");
			}
			row = row.replace(",", "<,");
			row = row.replace(".", ">.");
			row = row.replace("?", "?/");
			layout.add(row);
		}
		layout.set(1, layout.get(1) + "{[}]|\\");
		layout.set(2, layout.get(2) + "\"'");
		return layout;
	}

	private void validateInput(List<String> rawInput) {
		if (rawInput.size() != 3) {
			throw new IllegalArgumentException("invalid input");
		}
		for (String line : rawInput) {
			if (line.length() < 10) {
				throw new IllegalArgumentException("invalid input line:\n" + line);
			}
		}
	}

	private void createKeyboardImage(String filename) throws IOException {
		InputStream inputImage = getClass().getResourceAsStream("keyboard.png");
		BufferedImage keyboard = ImageIO.read(inputImage);
		addCharacters(keyboard);
		File outputImage = new File(filename);
		ImageIO.write(keyboard, "png", outputImage);
		System.out.println("Zapisano do pliku: " + outputImage);
	}

	private void addCharacters(BufferedImage keyboard) {
		Graphics2D graphics = keyboard.createGraphics();
		graphics.setColor(Color.BLACK);
		graphics.setFont(graphics.getFont().deriveFont(Font.BOLD, 12));
		for (int j = 0; j < layout.size(); j++) {
			int x = ROWS_X_MARGINS[j];
			for (int i = 0; i < layout.get(j).length(); i++) {
				char c = layout.get(j).charAt(i);
				int y = ROWS_Y_MARGINS[j];
				if (Character.isLetter(c)) {
					graphics.drawString("" + Character.toUpperCase(c),
							x + KEY_X_PADDING, y + KEY_Y_PADDING);
				} else {
					graphics.drawString("" + c, x + KEY_X_PADDING,
							y + KEY_Y_TOP_PADDING);
					i++;
					c = layout.get(j).charAt(i);
					graphics.drawString("" + c, x + KEY_X_PADDING,
							y + KEY_Y_BOTTOM_PADDING);
				}
				x += KEY_WIDTH;
			}
		}
	}
}
