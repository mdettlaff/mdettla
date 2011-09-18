package mdettla.reddit.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;

@Entity
public class User {

	private static final String ADMINISTRATOR_USERNAME = "administrator";

	@Id
	@GeneratedValue
	private Long id;
	@NotNull
	@Column(unique = true)
	private String name;
	@NotNull
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
	public boolean equals(Object obj) {
		if (!(obj instanceof User)) {
			return false;
		}
		User other = (User)obj;
		EqualsBuilder equalsBuilder = new EqualsBuilder();
		equalsBuilder.append(name, other.name);
		equalsBuilder.append(password, other.password);
		return equalsBuilder.build();
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(name).append(password).build();
	}

	@Override
	public String toString() {
		ToStringBuilder toStringBuilder = new ToStringBuilder(this);
		return toStringBuilder.append(id).append(name).append(password).build();
	}
}
