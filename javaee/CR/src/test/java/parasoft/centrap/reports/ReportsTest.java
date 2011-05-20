package parasoft.centrap.reports;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import javax.ejb.embeddable.EJBContainer;
import javax.naming.Context;
import javax.naming.NamingException;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class ReportsTest {

	private static EJBContainer ejbContainer;
	private static Context context;

	@BeforeClass
	public static void setUp() {
		ejbContainer = EJBContainer.createEJBContainer();
		context = ejbContainer.getContext();
	}

	@AfterClass
	public static void tearDown() {
		ejbContainer.close();
	}

	@Test
	public void testSalesReport() throws NamingException {
		Report report =
			(Report)context.lookup("java:global/classes/SalesReport");
		assertNotNull(report);
		ReportFilter filter = new ReportFilter();
		filter.setIndustryName("Ryby");
		filter.setProductName("Halibut");
		report.start(filter);
		assertEquals(
				"Report in plain text format.\n" +
				"Sales computed for industry Ryby\n" +
				"Sales computed for product Halibut",
				report.getPlainTextResults());
		assertEquals(
				"Report in CSV format.\n" +
				"Sales computed for industry Ryby\n" +
				"Sales computed for product Halibut",
				report.getCsvResults());
	}

	@Test
	public void testDiscountsReport() throws NamingException {
		Report report =
			(Report)context.lookup("java:global/classes/DiscountsReport");
		assertNotNull(report);
		ReportFilter filter = new ReportFilter();
		filter.setIndustryName("Ryby");
		filter.setProductName("Halibut");
		filter.setDiscount(5);
		report.start(filter);
		assertEquals(
				"Report in plain text format.\n" +
				"Discounts computed for industry Ryby\n" +
				"Discounts computed for product Halibut\n" +
				"Discounts computed for discount 5",
				report.getPlainTextResults());
		assertEquals(
				"Report in CSV format.\n" +
				"Discounts computed for industry Ryby\n" +
				"Discounts computed for product Halibut\n" +
				"Discounts computed for discount 5",
				report.getCsvResults());
	}
}
