package mdettla.jadex.pennyauctions.seller;

import mdettla.jadex.pennyauctions.util.Utils;

public class Product {

	private Integer productId;
	private String name;
	private Integer retailPrice;

	public Product(Integer productId, String name, Integer retailPrice) {
		this.productId = productId;
		this.name = name;
		this.retailPrice = retailPrice;
	}

	public Integer getId() {
		return productId;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}

	public void setRetailPrice(Integer retailPrice) {
		this.retailPrice = retailPrice;
	}

	public Integer getRetailPrice() {
		return retailPrice;
	}

	public String toString() {
		return name + ", cena detaliczna: " +
		Utils.formatPrice(retailPrice) + " zł";
	}
}
