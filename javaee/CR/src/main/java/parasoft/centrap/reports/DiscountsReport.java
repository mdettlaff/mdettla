package parasoft.centrap.reports;

import javax.ejb.Stateful;

@Stateful
public class DiscountsReport extends Report {

	@Override
	protected String computeResults(ReportFilter filter) {
		return
			"Discounts computed for industry " +
			filter.getIndustryName() + "\n" +
			"Discounts computed for product " +
			filter.getProductName() + "\n" +
			"Discounts computed for discount " +
			filter.getDiscount();
	}
}
