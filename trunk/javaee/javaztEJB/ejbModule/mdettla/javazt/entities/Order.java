package mdettla.javazt.entities;

import java.io.Serializable;
import java.util.List;

import javax.persistence.*;

/**
 * Entity implementation class for Entity: Order
 */
@Entity

/**
 * Złożone zamówienie. Zawiera adres wysyłki oraz
 * listę produktów i ich ilości.
 */
@Table(name = "orders")
public class Order implements Serializable {

	private int id;
	private String sendAddress;
	private List<OrderedProduct> orderedProducts;
	private static final long serialVersionUID = 1L;

    @Id
    @GeneratedValue(strategy=GenerationType.IDENTITY)
	public int getId() {
		return this.id;
	}

	public void setId(int id) {
		this.id = id;
	}   

	public Order() {
		super();
	}   
	
	public String getSendAddress() {
		return this.sendAddress;
	}

	public void setSendAddress(String sendAddress) {
		this.sendAddress = sendAddress;
	}

	@OneToMany(cascade={CascadeType.ALL}, fetch=FetchType.EAGER)
	@JoinTable(name="ORDER_PRODUCT",
	joinColumns={@JoinColumn(name="ORDER_ID")},
	inverseJoinColumns={@JoinColumn(name="PRODUCT_ID")})	
	public List<OrderedProduct> getOrderedProducts() {
		return orderedProducts;
	}

	public void setOrderedProducts(List<OrderedProduct> orderedProducts) {
		this.orderedProducts = orderedProducts;
	}
}
