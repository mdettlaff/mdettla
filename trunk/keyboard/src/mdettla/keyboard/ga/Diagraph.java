package mdettla.keyboard.ga;

public class Diagraph {

	private final Character firstLetter;
	private final Character secondLetter;

	public Diagraph(Character firstLetter, Character secondLetter) {
		this.firstLetter = firstLetter;
		this.secondLetter = secondLetter;
	}

	public Character getFirstLetter() {
		return firstLetter;
	}

	public Character getSecondLetter() {
		return secondLetter;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((firstLetter == null) ? 0 : firstLetter.hashCode());
		result = prime * result
				+ ((secondLetter == null) ? 0 : secondLetter.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Diagraph other = (Diagraph) obj;
		if (firstLetter == null) {
			if (other.firstLetter != null)
				return false;
		} else if (!firstLetter.equals(other.firstLetter))
			return false;
		if (secondLetter == null) {
			if (other.secondLetter != null)
				return false;
		} else if (!secondLetter.equals(other.secondLetter))
			return false;
		return true;
	}
}
