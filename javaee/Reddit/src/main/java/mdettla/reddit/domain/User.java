package mdettla.reddit.domain;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Transient;

@Entity
public class User {

	private static final String ADMINISTRATOR_USERNAME = "administrator";

	@Id
	@GeneratedValue
	private Long id;
	private String name;
	private String password;

	public User() {
	}

	public User(String name, String password) {
		this.name = name;
		this.password = password;
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	@Transient
	public boolean isAdministrator() {
		return ADMINISTRATOR_USERNAME.equals(name);
	}

	@Override
	public String toString() {
		return "User[id=" + id + ", name=" + name + "]";
	}
}
