package parasoft.centrap.reports;

import javax.inject.Named;

@Named
public class SalesReport extends Report {

	@Override
	protected String computeResults(ReportFilter filter) {
		return
			"Sales computed for industry " +
			filter.getIndustryName() + "\n" +
			"Sales computed for product " +
			filter.getProductName();
	}
}
