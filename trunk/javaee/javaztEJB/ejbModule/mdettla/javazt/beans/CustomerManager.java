package mdettla.javazt.beans;

import java.util.ArrayList;
import java.util.List;

import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;

import mdettla.javazt.entities.Address;
import mdettla.javazt.entities.Customer;
import mdettla.javazt.entities.Group;
import mdettla.javazt.entities.Order;
import mdettla.javazt.entities.OrderedProduct;
import mdettla.javazt.entities.PhoneNumber;
import mdettla.javazt.entities.Product;
import mdettla.javazt.entities.User;

/**
 * Session Bean implementation class CustomerManager
 */
@Stateless
public class CustomerManager implements CustomerManagerLocal {

    /**
     * Default constructor. 
     */
    public CustomerManager() {
    }

    @PersistenceContext(unitName="javaztEJB")
    EntityManager em;

    @Override
	public boolean addCustomer(String login, String password,
			String name, String surname, int age, String pesel,
			PhoneNumber phone, Address address) {
    	// sprawdzamy czy taki login już istnieje w bazie danych
    	Query query = em.
    	createQuery("SELECT user FROM User AS user WHERE user.username='" + login + "'");
		if (query.getResultList().size() > 0) {
			return false;
		}
    	User user = new User();
    	user.setUsername(login);
    	user.setPassword(password);
    	Group group = new Group();
    	group.setGroupName("registeredUsers");
    	group.setUsername(login);
    	em.persist(group);
    	
		Customer customer = new Customer();
		customer.setName(name);
		customer.setSurname(surname);
		customer.setAge(age);
		customer.setPesel(pesel);
		customer.setPhone(phone);
		customer.setAddress(address);
		customer.setUser(user);
		em.persist(customer);
		return true;
	}

    @Override
	public boolean updateCustomer(String login,
			String name, String surname, int age, String pesel,
			PhoneNumber phone, Address address) {
		// znajdujemy klienta
    	Query query = em.
    	createQuery("SELECT c FROM Customer AS c WHERE c.user.username='" + login + "'");
    	Customer customer = (Customer) query.getSingleResult();
    	
		customer.setName(name);
		customer.setSurname(surname);
		customer.setAge(age);
		customer.setPesel(pesel);
		customer.setPhone(phone);
		customer.setAddress(address);
		em.merge(customer);
		return true;
	}

	@Override
	public boolean addOrder(String login,
			String sendAddress, List<Product> products, List<Integer> quantities) {
		// tworzymy zamówienie, które dodamy do klienta
		Order order = new Order();
		List<OrderedProduct> orderedProducts = new ArrayList<OrderedProduct>();
		int i = 0;
		for (Product product : products) {
			OrderedProduct orderedProduct = new OrderedProduct();

			orderedProduct.setPriceZloty(product.getPriceZloty());
			orderedProduct.setPriceGrosz(product.getPriceGrosz());
			orderedProduct.setTitle(product.getTitle());
			orderedProduct.setDescription(product.getDescription());
			orderedProduct.setAuthor(product.getAuthor());
			orderedProduct.setCategory(product.getCategory());
			orderedProduct.setQuantity(quantities.get(i));

			orderedProducts.add(orderedProduct);
			i++;
		}
		order.setOrderedProducts(orderedProducts);
		// znajdujemy klienta
    	Query query = em.
    	createQuery("SELECT c FROM Customer AS c WHERE c.user.username='" + login + "'");
    	Customer customer = (Customer) query.getSingleResult();

    	// sprawdzamy czy podano inny adres niż przy rejestracji
    	if (!sendAddress.trim().equals("")) {
    		order.setSendAddress(sendAddress);
    	} else {
    		order.setSendAddress(customer.getAddress().getZipCode() + " "
    				+ customer.getAddress().getTown() + ", "
    				+ customer.getAddress().getStreet());
    	}
    	// dodajemy zamówienie do listy zamówień klienta
    	customer.getOrders().add(order);
		em.merge(customer);
		return true;
	}

	/**
	 * Zwraca listę zamówień dokonanych przez użytkownika o podanym loginie.
	 */
	@Override
	public List<Order> getOrders(String login) {
		// znajdujemy klienta
    	Query query = em.
    	createQuery("SELECT c FROM Customer AS c WHERE c.user.username='" + login + "'");
    	Customer customer = (Customer) query.getSingleResult();
    	// dodajemy zamówienie do listy zamówień klienta
    	return customer.getOrders();
	}
	
	/**
	 * Zwraca obiekt Customer zawierający informacje o kliencie.
	 */
	@Override
	public Customer getCustomerData(String login) {
		// znajdujemy klienta
    	Query query = em.
    	createQuery("SELECT c FROM Customer AS c WHERE c.user.username='" + login + "'");
    	Customer customer = (Customer) query.getSingleResult();
    	return customer;
	}

}
