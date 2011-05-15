package parasoft.centrap.reports;

public abstract class Report {

	private String results;
	private ReportFilter filter;

	protected abstract String computeResults(ReportFilter filter);

	public void setFilter(ReportFilter filter) {
		this.filter = filter;
	}

	public void start() {
		if (filter == null) {
			throw new IllegalStateException(
					"Filter cannot be null after report is started.");
		}
		results = computeResults(filter);
	}

	public String getHtmlResults() {
		checkResultsNotNull();
		return "Report in HTML format.<br />\n" +
			results.replace("\n", "<br />\n");
	}

	public String getPlainTextResults() {
		checkResultsNotNull();
		return "Report in plain text format.\n" + results;
	}

	public String getCsvResults() {
		checkResultsNotNull();
		return "Report in CSV format.\n" + results;
	}

	private void checkResultsNotNull() {
		if (results == null) {
			throw new IllegalStateException(
					"Results cannot be null before conversion.");
		}
	}
}
