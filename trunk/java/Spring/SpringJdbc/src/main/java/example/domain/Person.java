package example.domain;

public class Person {

	private final String firstName;
	private final String lastName;

	public Person(String firstName, String lastName) {
		this.firstName = firstName;
		this.lastName = lastName;
	}

	public String getFirstName() {
		return firstName;
	}

	public String getLastName() {
		return lastName;
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof Person)) {
			return false;
		}
		Person other = (Person)obj;
		if (!getFirstName().equals(other.getFirstName())) {
			return false;
		}
		if (!getLastName().equals(other.getLastName())) {
			return false;
		}
		return true;
	}

	@Override
	public int hashCode() {
		return getFirstName().hashCode() + 17 * getLastName().hashCode();
	}

	@Override
	public String toString() {
		return getFirstName() + " " + getLastName();
	}
}
