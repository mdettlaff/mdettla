package mdettla.javazt.beans;
import java.util.List;

import javax.ejb.Local;

import mdettla.javazt.entities.Product;

@Local
public interface ProductManagerLocal {
    public List<Product> getProducts();

    public List<Product> getProducts(String category);

    public Product getProduct(int id);
}
