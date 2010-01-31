package mdettla.javazt.entities;

import java.io.Serializable;
import javax.persistence.*;

/**
 * Entity implementation class for Entity: Group
 *
 */
@Entity
@Table(name="groups")

public class Group implements Serializable {

	
	private String username;
	private String groupName;
	@Id
    @GeneratedValue(strategy=GenerationType.IDENTITY)
	private int id;
	private static final long serialVersionUID = 1L;

	public Group() {
		super();
	}   
	public String getUsername() {
		return this.username;
	}

	public void setUsername(String username) {
		this.username = username;
	}   
	public String getGroupName() {
		return this.groupName;
	}

	public void setGroupName(String groupName) {
		this.groupName = groupName;
	}
	public int getId() {
		return id;
	}
	public void setId(int id) {
		this.id = id;
	}
	
	
}
