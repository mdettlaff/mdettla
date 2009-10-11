package mdettla.jga.core;

public interface MutationOperator {

	/**
	 * Poddaje danego osobnika mutacji.
	 *
	 * @param specimen Osobnik, którego chcemy poddać mutacji.
	 */
	public void mutate(Specimen specimen);
}
