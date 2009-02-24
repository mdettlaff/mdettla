import java.io.*;
import java.awt.*;
import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

public class ReadMap {
	public static void main(String args[]) throws IOException{
		BufferedReader in = new BufferedReader(
				new InputStreamReader(System.in));
		File file = new File("mapa_polski.png");
		BufferedImage image = ImageIO.read(file);

		System.out.println("Content-type: text/html\n\n");
		System.out.println("<HTML>\n");
		//System.out.println("<HEAD>\n");
		//System.out.println("<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-2\" />");
		//System.out.println("</HEAD>\n");
		System.out.println("<BODY>\n");

		String query = in.readLine();
		Integer x = new Integer(query.replaceAll("&obrazek.y.*","").
				replaceAll("obrazek.x=",""));
		Integer y = new Integer(query.replaceAll(".*obrazek.y=",""));

		int clr = image.getRGB(x, y); 
		int red = (clr & 0x00ff0000) >> 16;
		int green = (clr & 0x0000ff00) >> 8;
		int blue = clr & 0x000000ff;
		//System.out.println("RGB: "+red+", "+green+", "+blue+"<br>");

		if ((red == 255 && green == 255 && blue == 255)
		    || (red <= 75 && green <= 75 && blue <= 75)) {
		  System.out.println("Nie wybrales zadnego wojewodztwa.");
		} else {
			System.out.print("Wybrales wojewodztwo ");
			if (red == 64 && green == 164 && blue == 234) {
				System.out.print("pomorskie");
			} else if (red == 231 && green == 34 && blue == 34) {
				System.out.print("kujawsko-pomorskie");
			} else if (red == 80 && green == 200 && blue == 97) {
				System.out.print("mazowieckie");
			} else if (red == 27 && green == 215 && blue == 173) {
				System.out.print("zachodniopomorskie");
			} else if (red == 227 && green == 182 && blue == 210) {
				System.out.print("podlaskie");
			} else if (red == 48 && green == 111 && blue == 123) {
				System.out.print("lubuskie");
			} else if (red == 219 && green == 243 && blue == 19) {
				System.out.print("wielkopolskie");
			} else if (red == 246 && green == 162 && blue == 87) {
				System.out.print("lodzkie");
			} else if (red == 198 && green == 198 && blue == 198) {
				System.out.print("lubelskie");
			} else if (red == 123 && green == 123 && blue == 123) {
				System.out.print("dolnoslaskie");
			} else if (red == 247 && green == 255 && blue == 214) {
				System.out.print("opolskie");
			} else if (red == 166 && green == 115 && blue == 147) {
				System.out.print("slaskie");
			} else if (red == 140 && green == 131 && blue == 207) {
				System.out.print("malopolskie");
			} else if (red == 152 && green == 230 && blue == 244) {
				System.out.print("swietokrzyskie");
			} else if (red == 241 && green == 152 && blue == 152) {
				System.out.print("podkarpackie");
			} else if (red == 210 && green == 175 && blue == 99) {
				System.out.print("warminsko-mazurskie");
			}
			System.out.println(".");
		}
		System.out.println("<br><br>");
		System.out.println("<a href=\"formmap.html\">Powrot</a>");

		System.out.println("</BODY>\n");
		System.out.println("</HTML>\n");
	}
}
