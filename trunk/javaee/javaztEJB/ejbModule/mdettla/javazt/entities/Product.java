package mdettla.javazt.entities;

import java.io.Serializable;
import javax.persistence.*;

/**
 * Entity implementation class for Entity: Product
 */
@Entity

@Table(name = "products")
public class Product implements Serializable {

	private int id;
	
	@Id
    @GeneratedValue(strategy=GenerationType.IDENTITY)
	public int getId() {
		return this.id;
	}
	public void setId(int id) {
		this.id = id;
	}
	
	private int priceZloty;
	private int priceGrosz;
	private String title;
	private String description;
	private String author;
	private String category;
	private static final long serialVersionUID = 1L;

	public Product() {
		super();
	}   
	public int getPriceZloty() {
		return this.priceZloty;
	}

	public void setPriceZloty(int priceZloty) {
		this.priceZloty = priceZloty;
	}   
	public int getPriceGrosz() {
		return this.priceGrosz;
	}

	public void setPriceGrosz(int priceGrosz) {
		this.priceGrosz = priceGrosz;
	}   
	public String getTitle() {
		return this.title;
	}

	public void setTitle(String title) {
		this.title = title;
	}   
	public String getDescription() {
		return this.description;
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
