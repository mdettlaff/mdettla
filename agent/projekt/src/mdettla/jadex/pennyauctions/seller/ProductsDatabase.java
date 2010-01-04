package mdettla.jadex.pennyauctions.seller;

import java.util.ArrayList;
import java.util.List;

public class ProductsDatabase {

	private static int productIdGenerator = 0;

	private static List<Product> products = new ArrayList<Product>();
	static {
		products.add(new Product(generateProductId(), "Nokia 7070", 24000));
		products.add(new Product(generateProductId(), "Blueberry London zapach", 17000));
		products.add(new Product(generateProductId(), "Bon Sodexo", 20000));
	}

	public static Product getProduct(int productId) {
		for (Product product : products) {
			if (product.getId().equals(productId)) {
				return product;
			}
		}
		return null;
	}

	private static int generateProductId() {
		productIdGenerator++;
		return productIdGenerator;
	}
}
