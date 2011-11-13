package mdettla.jqsl;

import static mdettla.jqsl.JQSL.VALUE;
import static mdettla.jqsl.JQSL.cast;
import static mdettla.jqsl.JQSL.col;
import static mdettla.jqsl.JQSL.expr;
import static mdettla.jqsl.JQSL.select;
import static mdettla.jqsl.JQSL.sum;
import static mdettla.jqsl.JQSL.tbl;

import java.sql.SQLException;

public class Main {

	public static void main(String[] args) throws SQLException {
		String query =
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
					col("PRODUCT_NAME"))
			.toString();
		System.out.println(query);
	}
}
