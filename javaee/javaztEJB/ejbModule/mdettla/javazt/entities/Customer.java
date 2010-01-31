package mdettla.javazt.entities;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.*;

/**
 * Entity implementation class for Entity: Customer
 *
 */
@Entity

@Table(name = "customers")
public class Customer implements Serializable {

	private int id;
	private String name;
	private String surname;
	private int age;
	private String pesel;
	private PhoneNumber phone;
	private Address address;
	private List<Order> orders = new ArrayList<Order>();
	/**
	 * Użytkownik, z którym związane są dane o kliencie zawarte w tej klasie.
	 */
	private User user;
	private static final long serialVersionUID = 1L;

	public Customer() {
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
	
	public String getName() {
		return this.name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getSurname() {
		return surname;
	}

	public void setSurname(String surname) {
		this.surname = surname;
	}
	
	public int getAge() {
		return age;
	}

	public void setAge(int age) {
		this.age = age;
	}
	
	public String getPesel() {
		return pesel;
	}
	
	public void setPesel(String pesel) {
		this.pesel = pesel;
	}
	
	@OneToOne(cascade = { CascadeType.ALL })
	@JoinColumn(name = "PHONE_NUMBER_ID")
	public PhoneNumber getPhone() {
		return phone;
	}
	
	public void setPhone(PhoneNumber phone) {
		this.phone = phone;
	}
	
	@OneToOne(cascade = { CascadeType.ALL })
	@JoinColumn(name = "ADDRESS_ID")
	public Address getAddress() {
		return address;
	}
	
	public void setAddress(Address address) {
		this.address = address;
	}

	@OneToOne(cascade = { CascadeType.ALL })
	@JoinColumn(name = "USER_ID")
	public User getUser() {
		return user;
	}
	
	public void setUser(User user) {
		this.user = user;
	}
	
	@OneToMany(cascade={CascadeType.ALL}, fetch=FetchType.EAGER)
	@JoinTable(name="CUSTOMER_ORDER",
	joinColumns={@JoinColumn(name="CUSTOMER_ID")},
	inverseJoinColumns={@JoinColumn(name="ORDER_ID")})	
	public List<Order> getOrders() {
		return orders;
	}
	
	public void setOrders(List<Order> orders) {
		this.orders = orders;
	}
}
