package mdettla.jadex.pennyauctions.seller;

import java.util.ArrayList;
import java.util.List;

public class ProductsDatabase {

	private static List<Product> products = new ArrayList<Product>();
	static {
		products.add(new Product("Nokia 7070", 24000));
		products.add(new Product("Blueberry London zapach", 17000));
		products.add(new Product("Bon Sodexo", 20000));
	}

	public static List<Product> getProducts() {
		return products;
	}
}
