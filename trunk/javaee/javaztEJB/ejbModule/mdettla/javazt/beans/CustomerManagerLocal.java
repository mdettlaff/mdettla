package mdettla.javazt.beans;

import java.util.List;

import javax.ejb.Local;

import mdettla.javazt.entities.Address;
import mdettla.javazt.entities.Customer;
import mdettla.javazt.entities.Order;
import mdettla.javazt.entities.PhoneNumber;
import mdettla.javazt.entities.Product;

@Local
public interface CustomerManagerLocal {
	public boolean addCustomer(String login, String password,
			String name, String surname, int age, String pesel,
			PhoneNumber phone, Address address);
	
	public boolean updateCustomer(String login,
			String name, String surname, int age, String pesel,
			PhoneNumber phone, Address address);
	
	public boolean addOrder(String login,
			String sendAddress, List<Product> products, List<Integer> quantities);
	
	public List<Order> getOrders(String login);
	
	public Customer getCustomerData(String login);
}
