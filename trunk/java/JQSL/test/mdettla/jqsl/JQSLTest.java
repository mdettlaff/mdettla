package mdettla.jqsl;

import static mdettla.jqsl.JQSL.VALUE;
import static mdettla.jqsl.JQSL.cast;
import static mdettla.jqsl.JQSL.col;
import static mdettla.jqsl.JQSL.expr;
import static mdettla.jqsl.JQSL.select;
import static mdettla.jqsl.JQSL.sum;
import static mdettla.jqsl.JQSL.tbl;
import static org.junit.Assert.assertEquals;


import mdettla.jqsl.JQSLException;

import org.junit.Test;


public class JQSLTest {

	@Test
	public void testQueryHello() throws JQSLException {
		final String query =
			select(VALUE.as("HELLO"))
			.toString();
		assertEquals("SELECT ? AS HELLO", query);
	}

	@Test
	public void testQuerySmall() throws JQSLException {
		final String query =
			select(
					col("PRODUCTS.ID_PROD"),
					col("BARCODE"),
					col("PRODUCT_NAME"))
				.from(
					tbl("BILLS")
						.join(tbl("sklep.CONTRACTORS"))
							.using(col("ID_CONTR")))
				.where(
					col("PRODUCTS.ID_PROD").eq(col("WAREHOUSE.ID_PROD"))
					.and(col("WAREHOUSE.ID_RACH").eq(col("BILLS.ID_RACH"))))
			.toString();
		final String correctQuery =
			"SELECT " +
				"PRODUCTS.ID_PROD, BARCODE, PRODUCT_NAME " +
				"FROM BILLS JOIN sklep.CONTRACTORS " +
					"USING (ID_CONTR) " +
				"WHERE PRODUCTS.ID_PROD = WAREHOUSE.ID_PROD " +
					"AND WAREHOUSE.ID_RACH = BILLS.ID_RACH";
		assertEquals(correctQuery, query);
	}

	@Test
	public void testQueryMedium() throws JQSLException {
		final String query =
			select(
					col("PRODUCT_NAME"),
					col("BARCODE").as("BAR"),
					sum(col("PRODUCTS.ID_PROD")).as("MYSUM"),
					cast(
						VALUE.as("VARCHAR")
					).as("SCHEMA_NAME"))
				.from(
					select(
							col("ID_PROD"),
							expr("NET_BUY * 1000 / QUANTITY").as("WAREHOUSE_PRICE"),
							col("TYP_OPER").as("OPER_TYPE"))
						.from(tbl("sklep.ITEMS"))
					.alias("WAREHOUSE"),
					tbl("FOO"),
					tbl("BILLS")
						.leftJoin(tbl("sklep.CONTRACTORS"))
							.on(col("BILLS.ID_SUPPLIER")
								.eq(col("CONTRACTORS.ID_CONTR"))))
				.where(
					col("PRODUCTS.ID_PROD").eq(col("WAREHOUSE.ID_PROD"))
					.or(col("WAREHOUSE.ID_RACH").uneq(col("BILLS.ID_RACH"))))
				.groupBy(
					col("BAR"), col("PRODUCT_NAME"), col("SCHEMA_NAME"))
				.orderBy(
					col("SCHEMA_NAME"), col("PRODUCT_NAME").asc(), col("MYSUM").desc())
			.toString();
		final String correctQuery =
			"SELECT " +
					"PRODUCT_NAME, BARCODE AS BAR, " +
					"SUM(PRODUCTS.ID_PROD) AS MYSUM, " +
					"CAST(? AS VARCHAR) AS SCHEMA_NAME " +
				"FROM " +
					"(" +
						"SELECT " +
								"ID_PROD, " +
								"NET_BUY * 1000 / QUANTITY AS WAREHOUSE_PRICE, " +
								"TYP_OPER AS OPER_TYPE " +
							"FROM sklep.ITEMS" +
					") WAREHOUSE, " +
					"FOO, " +
					"BILLS LEFT JOIN sklep.CONTRACTORS " +
						"ON (BILLS.ID_SUPPLIER = CONTRACTORS.ID_CONTR) " +
				"WHERE " +
					"(PRODUCTS.ID_PROD = WAREHOUSE.ID_PROD " +
						"OR WAREHOUSE.ID_RACH <> BILLS.ID_RACH) " +
				"GROUP BY BAR, PRODUCT_NAME, SCHEMA_NAME " +
				"ORDER BY SCHEMA_NAME, PRODUCT_NAME ASC, MYSUM DESC";
		assertEquals(correctQuery, query);
	}

	@Test
	public void testUnion() throws JQSLException {
		final String query =
			select(col("FOO")).from(tbl("BAR"))
			.unionAll(
			select(col("BAZ")).from(tbl("QUUX")))
			.toString();
		final String correctQuery =
			"(SELECT FOO FROM BAR) " +
			"UNION ALL " +
			"(SELECT BAZ FROM QUUX)";
		assertEquals(correctQuery, query);
	}

	@Test
	public void testAutoGroupBy() throws JQSLException {
		final String query =
			select(
					col("PRODUCT_NAME"),
					sum(col("NET_BUY")).as("NET_BUY"),
					col("BARCODE"))
				.from(
					tbl("QUUX"))
			.toString();
		final String correctQuery =
			"SELECT " +
					"PRODUCT_NAME, " +
					"SUM(NET_BUY) AS NET_BUY, " +
					"BARCODE " +
				"FROM QUUX " +
				"GROUP BY PRODUCT_NAME, BARCODE";
		assertEquals(correctQuery, query);
	}

	@Test
	public void testHaving() throws JQSLException {
		final String query =
			select(
					sum(col("NET_BUY")))
				.from(
					tbl("BAR"))
				.having(
					sum(col("NET_BUY")).gt(VALUE))
			.toString();
		final String correctQuery =
			"SELECT SUM(NET_BUY) " +
				"FROM BAR " +
				"HAVING SUM(NET_BUY) > ?";
		assertEquals(correctQuery, query);
	}

	@Test
	public void testWhereHaving() throws JQSLException {
		final String query =
			select(
					col("PRODUCT_NAME"),
					sum(col("NET_BUY")))
				.from(
					tbl("BAR"))
				.where(
					col("PRODUCT_NAME").eq(VALUE))
				.having(
					sum(col("NET_BUY")).gt(VALUE))
			.toString();
		final String correctQuery =
			"SELECT " +
					"PRODUCT_NAME, " +
					"SUM(NET_BUY) " +
				"FROM BAR " +
				"WHERE PRODUCT_NAME = ? " +
				"GROUP BY PRODUCT_NAME " +
				"HAVING SUM(NET_BUY) > ?";
		assertEquals(correctQuery, query);
	}
}
