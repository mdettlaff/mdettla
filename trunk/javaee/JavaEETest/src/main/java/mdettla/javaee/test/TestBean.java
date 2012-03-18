package mdettla.javaee.test;

import javax.faces.bean.ManagedBean;

@ManagedBean
public class TestBean {

	private int price;
	private int amount;

	public TestBean() {
		this.price = 5;
		this.amount = 10;
	}

	public void updatePrice() {
		this.setPrice(this.getAmount() / 2);
	}

	public void updateAmount() {
		this.setAmount(this.getPrice() * 2);
	}

	public void save() {
		System.out.println("Saving values to database:");
		System.out.println("price = " + this.getPrice());
		System.out.println("amount = " + this.getAmount());
	}

	public int getPrice() {
	    return this.price;
    }

	public void setPrice(int price) {
	    this.price = price;
    }

	public int getAmount() {
	    return this.amount;
    }

	public void setAmount(int amount) {
	    this.amount = amount;
    }
}
