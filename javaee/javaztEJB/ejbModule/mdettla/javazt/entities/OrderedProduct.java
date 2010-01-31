package mdettla.javazt.entities;

import java.io.Serializable;
import javax.persistence.*;

/**
 * Entity implementation class for Entity: Product
 * Informacje o produkcie oraz jego zamówionej ilości.
 */
@Entity

@Table(name = "ordered_products")
public class OrderedProduct implements Serializable {

	private int id;
	
	private int priceZloty;
	private int priceGrosz;
	private String title;
	private String description;
	private String author;
	private String category;	
	private int quantity;
	private static final long serialVersionUID = 1L;

	
	public OrderedProduct() {
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

	public int getQuantity() {
		return quantity;
	}
	
	public void setQuantity(int quantity) {
		this.quantity = quantity;
	}

	public int getPriceZloty() {
		return priceZloty;
	}

	public void setPriceZloty(int priceZloty) {
		this.priceZloty = priceZloty;
	}

	public int getPriceGrosz() {
		return priceGrosz;
	}

	public void setPriceGrosz(int priceGrosz) {
		this.priceGrosz = priceGrosz;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getAuthor() {
		return author;
	}

	public void setAuthor(String author) {
		this.author = author;
	}

	public String getCategory() {
		return category;
	}

	public void setCategory(String category) {
		this.category = category;
	}
	
}
