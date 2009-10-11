package mdettla.jga.core;

public interface MutationOperator {

	/**
	 * Poddaje danego osobnika mutacji.
	 *
	 * @param specimenToMutate Osobnik, którego chcemy poddać mutacji.
	 */
	public void mutate(Specimen specimenToMutate);
}
