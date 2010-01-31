package mdettla.javazt.utils;

/**
 * Klasa reprezentująca produkt w koszyku.
 */
public class CartProduct {

	/**
	 * ID produktu w bazie danych.
	 */
	private int id;
	/**
	 * Ilość egzemplarzy do zakupu.
	 */
	private int quantity;

	public CartProduct(int id, int quantity) {
		this.id = id;
		this.quantity = quantity;
	}
	public int getId() {
		return id;
	}
	public void setId(int id) {
		this.id = id;
	}
	public int getQuantity() {
		return quantity;
	}
	public void setQuantity(int quantity) {
		this.quantity = quantity;
	}
}
