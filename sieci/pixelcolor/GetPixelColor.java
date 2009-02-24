import java.io.*;
import java.awt.*;
import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;

public class GetPixelColor
{
	public static void main(String args[]) throws IOException{
		if (args.length < 1) {
			System.out.println("Uzycie: java GetPixelColor nazwapliku x y");
		} else {
			File file= new File(args[0]);
			BufferedImage image = ImageIO.read(file);
			int clr=  image.getRGB(new Integer(args[1]), new Integer(args[2])); 
			int  red = (clr & 0x00ff0000) >> 16;
			int  green = (clr & 0x0000ff00) >> 8;
			int  blue = clr & 0x000000ff;
			System.out.println("Red Color value = "+ red);
			System.out.println("Green Color value = "+ green);
			System.out.println("Blue Color value = "+ blue);
		}
	}
}
