package parasoft.centrap.reports;

import javax.ejb.Stateful;

@Stateful
public class SalesReport extends Report {

	@Override
	protected String computeResults(ReportFilter filter) {
		try {
			Thread.sleep(4000);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		return
			"Sales computed for industry " +
			filter.getIndustryName() + "\n" +
			"Sales computed for product " +
			filter.getProductName();
	}
}
