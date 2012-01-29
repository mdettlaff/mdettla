package mdettla.regexp;

interface Expression {

	boolean match(CharReader chars);
}
