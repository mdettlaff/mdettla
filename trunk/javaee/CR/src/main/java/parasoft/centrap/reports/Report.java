package parasoft.centrap.reports;

public abstract class Report {

	private static final String NULL_RESULTS = "Null results.";
	public static final Report EMPTY = new Report() {
		{
			computeResults(null);
		}
		@Override
		protected String computeResults(ReportFilter filter) {
			return NULL_RESULTS;
		}
	};

	private String results = NULL_RESULTS;

	protected abstract String computeResults(ReportFilter filter);

	public void start(ReportFilter filter) {
		results = computeResults(filter);
	}

	public String getHtmlResults() {
		return "Report in HTML format.<br />\n" +
			results.replace("\n", "<br />\n");
	}

	public String getPlainTextResults() {
		return "Report in plain text format.\n" + results;
	}

	public String getCsvResults() {
		return "Report in CSV format.\n" + results;
	}
}
