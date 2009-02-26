import java.io.File;
import java.io.FilenameFilter;

/**
 * Filtr sprawdzający, czy podane pliki mają rozszerzenie txt.
 */
public class TextfileFilter implements FilenameFilter {
  public boolean accept(File dir, String name) {
    return name.toLowerCase().endsWith(".txt");
  }
}
