package mdettla.jadex.pennyauctions.seller;

public class Product {

	private String name;
	private Integer retailPrice;

	public Product(String name, Integer retailPrice) {
		this.name = name;
		this.retailPrice = retailPrice;
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
}
