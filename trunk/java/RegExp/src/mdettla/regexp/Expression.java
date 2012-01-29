package mdettla.regexp;

interface Expression {

	boolean match(CharIterator chars);
}
