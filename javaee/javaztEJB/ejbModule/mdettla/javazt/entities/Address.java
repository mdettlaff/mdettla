package mdettla.javazt.entities;

import java.io.Serializable;
import javax.persistence.*;

/**
 * Entity implementation class for Entity: Address
 *
 */
@Entity
@Table(name="addresses")

public class Address implements Serializable {

	
	private String town;
	private String zipCode;
	private String street;   
	private int id;
	private static final long serialVersionUID = 1L;

	public Address() {
		super();
	}   
	public String getTown() {
		return this.town;
	}

	public void setTown(String town) {
		this.town = town;
	}   
	public String getZipCode() {
		return this.zipCode;
	}

	public void setZipCode(String zipCode) {
		this.zipCode = zipCode;
	}   
	public String getStreet() {
		return this.street;
	}

	public void setStreet(String street) {
		this.street = street;
	}
	
	@Id
    @GeneratedValue(strategy=GenerationType.IDENTITY)
	public int getId() {
		return this.id;
	}

	public void setId(int id) {
		this.id = id;
	}
}
