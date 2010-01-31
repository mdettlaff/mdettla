package mdettla.javazt.beans;

import java.util.List;

import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;

import mdettla.javazt.entities.Product;

/**
 * Session Bean implementation class CustomerManager
 */
@Stateless
public class ProductManager implements ProductManagerLocal {

    /**
     * Default constructor. 
     */
    public ProductManager() {
    }

    @PersistenceContext(unitName="javaztEJB")
    EntityManager em;

    @SuppressWarnings("unchecked")
	@Override
	public List<Product> getProducts() {
		Query query = em
		.createQuery("SELECT p FROM Product AS p");
		return query.getResultList();
	}

	@Override
	public Product getProduct(int id) {
		Query query = em
		.createQuery("SELECT p FROM Product AS p WHERE p.id=" + id);
		return (Product) query.getSingleResult();
	}

    @SuppressWarnings("unchecked")
	@Override
	public List<Product> getProducts(String category) {
		Query query = em
		.createQuery("SELECT p FROM Product AS p WHERE p.category='" + category + "'");
		return query.getResultList();
	}

}
