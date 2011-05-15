package parasoft.centrap.reports;

public class RaportZakonczonychPromocji extends Report {

	@Override
	protected String computeResults(ReportFilter filter) {
		return
			"Computing results for industry " +
			filter.getIndustryName() + "\n" +
			"Computing results for product: " +
			filter.getProductName();
	}
}
