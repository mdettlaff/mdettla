package mdettla.directorytree;

import java.io.File;

public class ShortNameFile {

	private final File file;

	public ShortNameFile(File file) {
		this.file = file;
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof ShortNameFile)) {
			return false;
		}
		ShortNameFile other = (ShortNameFile)obj;
		return file.equals(other.file);
	}

	@Override
	public int hashCode() {
		return file.hashCode();
	}

	@Override
	public String toString() {
		return file.getName().toString();
	}
}
