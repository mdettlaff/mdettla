package parasoft.centrap.reports;

import javax.inject.Named;

@Named
public class DiscountsReport extends Report {

	@Override
	protected String computeResults(ReportFilter filter) {
		return
			"Discounts computed for industry " +
			filter.getIndustryName() + "\n" +
			"Discounts computed for product " +
			filter.getProductName();
	}
}
