package mdettla.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.UnsupportedEncodingException;

public class FileUtils {

	public static String readFileAsString(File file)
	throws IOException {
		BufferedReader reader = new BufferedReader(
				new FileReader(file));
		return readAsString(reader);
	}

	public static String readFileAsString(File file, String encoding)
	throws IOException {
		Reader reader = new BufferedReader(
				new InputStreamReader(new FileInputStream(file), encoding));
		return readAsString(reader);
	}

	public static String readAsString(Reader reader) throws IOException {
		StringBuffer sb = new StringBuffer();
		final int BUFFER_SIZE = 4 * 1024;
		char[] buffer = new char[BUFFER_SIZE];
		int readCount = 0;
		while ((readCount = reader.read(buffer)) != -1) {
			sb.append(buffer, 0, readCount);
		}
		reader.close();
		return sb.toString();
	}

	public static boolean writeStringToFile(String string, File file) {
		try {
			PrintWriter writer = new PrintWriter(new FileOutputStream(file));
			writer.write(string);
			writer.close();
			return true;
		} catch (FileNotFoundException e) {
			e.printStackTrace();
			return false;
		}
	}

	public static boolean writeStringToFile(String string, File file, String encoding)
	throws UnsupportedEncodingException {
		try {
			PrintWriter writer = new PrintWriter(new OutputStreamWriter(
					new FileOutputStream(file), encoding));
			writer.write(string);
			writer.close();
			return true;
		} catch (FileNotFoundException e) {
			e.printStackTrace();
			return false;
		}
	}
}
