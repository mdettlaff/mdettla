package mdettla.javazt.entities;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * Entity implementation class for Entity: PhoneNumber
 *
 */
@Entity
@Table(name="phone_numbers")
public class PhoneNumber implements Serializable {
	
	private static final long serialVersionUID = 1L;
	int id;
	private String areaCode = "";
	private String prefix = "";
	private String number = "";
	
	public PhoneNumber() {
		super();
	}
	
	@Id
    @GeneratedValue(strategy=GenerationType.IDENTITY)
	public int getId() {
		return this.id;
	}

	public void setId(int id) {
		this.id = id;
	}
	public String getAreaCode() {
		return areaCode;
	}
	public void setAreaCode(String areaCode) {
		this.areaCode = areaCode;
	}
	public String getPrefix() {
		return prefix;
	}
	public void setPrefix(String prefix) {
		this.prefix = prefix;
	}
	public String getNumber() {
		return number;
	}
	public void setNumber(String number) {
		this.number = number;
	}
	
	public String toString() {
		String pn = "";
		if (getAreaCode() != "") {
			pn += "(" + getAreaCode() + ") ";
		}
		if (getPrefix() != "") {
			pn += getPrefix() + "-";
		}
		if (getNumber() != "") {
			pn += getNumber();
		}
		return pn;
	}
}
