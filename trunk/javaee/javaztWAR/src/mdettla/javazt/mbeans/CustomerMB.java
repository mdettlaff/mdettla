package mdettla.javazt.mbeans;

import java.util.ArrayList;
import java.util.List;

import javax.ejb.EJB;
import javax.faces.model.SelectItem;

import mdettla.javazt.beans.CustomerManagerLocal;
import mdettla.javazt.beans.ProductManagerLocal;
import mdettla.javazt.entities.Address;
import mdettla.javazt.entities.Customer;
import mdettla.javazt.entities.Order;
import mdettla.javazt.entities.PhoneNumber;
import mdettla.javazt.entities.Product;
import mdettla.javazt.utils.CartProduct;

public class CustomerMB {

	/*
	 * Informacje o kupującym.
	 */
	private String login;
	private String password;
	private String name;
	private String surname;
	private String pesel;
	private int age;
	private PhoneNumber phone;
	// dane adresowe
	private String town;
	private String zipCode;
	private String street;

	/**
	 * Zamówienia aktualnego klienta.
	 */
	private List<Order> orders = new ArrayList<Order>();

	/**
	 * Adres do wysyłki, dla konkretnego zamówienia.
	 */
	private String sendAddress;

	/**
	 * Koszyk, będący listą produktów wybranych do koszyka (razem z ilościami).
	 */
	private List<CartProduct> cart = new ArrayList<CartProduct>();
	/**
	 * Wybrane z formularza elementy z koszyka.
	 */
    private List<String> selectedToDeleteItems;
	/**
	 * Indeks aktualnie wybranego produktu.
	 */
	private int productId;
	/**
	 * Ilość egzemplarzy aktualnie wybranego produktu do kupienia.
	 */
	private int productQuantity;
	
	/**
	 * Aktualnie przeglądana kategoria produktów (filmy, muzyka itp.)
	 */
	private String category;
	
    @EJB
    ProductManagerLocal pml;
    @EJB
    CustomerManagerLocal cml;
    

    /**
     * @return Wartość pieniężna wszystkich przedmiotów znajdujących się w koszyku.
     */
    public String getCartSum() {
    	int sumZloty = 0;
    	int sumGrosz = 0;
    	for (CartProduct cp : cart) {
    		sumZloty += getProducts().get(cp.getId()-1).getPriceZloty() * cp.getQuantity();
    		sumGrosz += getProducts().get(cp.getId()-1).getPriceGrosz() * cp.getQuantity();
    	}
    	sumZloty += sumGrosz / 100;
    	sumGrosz = sumGrosz % 100;
    	return sumZloty + "." + sumGrosz;
    }
    
	public List<CartProduct> getCart() {
		return cart;
	}

	public void setCart(List<CartProduct> cart) {
		this.cart = cart;
	}

	public int getProductId() {
		return productId;
	}

	public void setProductId(int productId) {
		this.productId = productId;
	}

	public int getProductQuantity() {
		return productQuantity;
	}

	public void setProductQuantity(int productQuantity) {
		this.productQuantity = productQuantity;
	}

	
	public String getLogin() {
		return login;
	}

	public void setLogin(String login) {
		this.login = login;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
	
    public String getSurname() {
		return surname;
	}

	public void setSurname(String surname) {
		this.surname = surname;
	}

	public int getAge() {
		return age;
	}

	public void setAge(int age) {
		this.age = age;
	}

	public String getPesel() {
		return pesel;
	}

	public void setPesel(String pesel) {
		this.pesel = pesel;
	}

	public PhoneNumber getPhone() {
		return phone;
	}

	public void setPhone(PhoneNumber phone) {
		this.phone = phone;
	}


	public String getTown() {
		return town;
	}

	public void setTown(String town) {
		this.town = town;
	}

	public String getZipCode() {
		return zipCode;
	}

	public void setZipCode(String zipCode) {
		this.zipCode = zipCode;
	}

	public String getStreet() {
		return street;
	}

	public void setStreet(String street) {
		this.street = street;
	}

	public String getSendAddress() {
		return sendAddress;
	}
	public void setSendAddress(String sendAddress) {
		this.sendAddress= sendAddress;
	}
	
	public List<Order> getOrders() {
		return cml.getOrders(login);
	}

	public void setOrders(List<Order> orders) {
		this.orders = orders;
	}

	public String getCategory() {
		return category;
	}

	public void setCategory(String category) {
		this.category = category;
	}
	
	/**
	 * Ładuje dane z bazy danych do zmiennych w tym beanie (na temat użytkownika).
	 */
	public void loadData() {
		Customer customer = cml.getCustomerData(login);
		name = customer.getName();
		surname = customer.getSurname();
		age = customer.getAge();
		pesel = customer.getPesel();
		phone = customer.getPhone();
		zipCode = customer.getAddress().getZipCode();
		town = customer.getAddress().getTown();
		street = customer.getAddress().getStreet();
	}

	/**
	 * Rejestruje nowego klienta w bazie danych.
	 */
	public String add() {
		Address address = new Address();
		address.setZipCode(zipCode);
		address.setTown(town);
		address.setStreet(street);
    	if (cml.addCustomer(login, password, name, surname, age, pesel, phone, address)) {
    		return "success";
    	} else {
    		return "failure";
    	}
    }
    
	/**
	 * Aktualizuje dane klienta w bazie danych.
	 */
	public String update() {
		// sprawdzamy czy wprowadzono zmienione dane; jeśli tak, robimy merge
		Customer customer = cml.getCustomerData(login);
		Address address = new Address();
		address.setZipCode(zipCode);
		address.setTown(town);
		address.setStreet(street);
		boolean isDataChanged = false;
		if (!name.equals(customer.getName())
				|| !surname.equals(customer.getSurname())
				|| age != customer.getAge()
				|| !pesel.equals(customer.getPesel())
				|| !phone.getPrefix().equals(customer.getPhone().getPrefix())
				|| !phone.getAreaCode().equals(customer.getPhone().getAreaCode())
				|| !phone.getNumber().equals(customer.getPhone().getNumber())
				|| !zipCode.equals(customer.getAddress().getZipCode())
				|| !town.equals(customer.getAddress().getTown())
				|| !street.equals(customer.getAddress().getStreet())
			) {
			isDataChanged = true;
		}
		if (!isDataChanged) {
			return "unchanged";
		} else if (cml.updateCustomer(login, name, surname, age, pesel, phone, address)) {
    		return "success";
    	} else {
    		return "failure";
    	}
    }
    
	public List<Product> getProducts() {
    	return pml.getProducts();
    }
	
	public List<Product> getProductsFromCategory() {
    	return pml.getProducts(category);
    }
	
	public String makeOrder() {
		List<Product> products = new ArrayList<Product>();
		List<Integer> quantities = new ArrayList<Integer>();
		for (CartProduct cartProduct : cart) {
			products.add(pml.getProduct(cartProduct.getId()));
			quantities.add(cartProduct.getQuantity());
		}
		if (cml.addOrder(login, sendAddress, products, quantities)) {
			return "success";
		}
		return "failure";
	}

	
	public String addProductToCart() {
		int i;
		if (productQuantity < 1 || productQuantity > 100) {
			return "badQuantity";
		}
		for (i=0; i < cart.size() && cart.get(i).getId() != productId; i++) {}
		if (i < cart.size()) {
			cart.get(i).setQuantity(cart.get(i).getQuantity()+productQuantity);
		} else {
			cart.add(new CartProduct(productId, productQuantity));
		}
		return "success";
	}
	
	public void deleteFromCart() {
		for (String s : selectedToDeleteItems) {
			// s jest tutaj id produktu w bazie danych
			int product_id = Integer.parseInt(s);
			CartProduct cpToDelete = null;
			for (CartProduct cp : cart) {
				if (cp.getId() == product_id) {
					cpToDelete = cp;
				}
			}
			if (cpToDelete != null) {
				cart.remove(cpToDelete);
			}
		}
	}

	public List<String> getSelectedToDeleteItems() {
		return selectedToDeleteItems;
	}

	public void setSelectedToDeleteItems(List<String> selectedToDeleteItems) {
		this.selectedToDeleteItems = selectedToDeleteItems;
	}

	public List<SelectItem> getCartSelectItems() {
		List<SelectItem> cartProducts = new ArrayList<SelectItem>();
		for (CartProduct cp : cart) {
			cartProducts.add(new SelectItem("" + cp.getId(), " "));
		}
		return cartProducts;
	}
}
