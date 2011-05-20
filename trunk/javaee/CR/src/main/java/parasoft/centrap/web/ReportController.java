package parasoft.centrap.web;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.Serializable;

import javax.enterprise.context.SessionScoped;
import javax.inject.Named;

import org.primefaces.model.DefaultStreamedContent;
import org.primefaces.model.StreamedContent;

import parasoft.centrap.reports.Report;
import parasoft.centrap.reports.ReportFilter;

@Named
@SessionScoped
public class ReportController implements Serializable {

	private Report report;
	private ReportFilter reportFilter;

	public ReportController() {
		report = Report.EMPTY;
		reportFilter = new ReportFilter();
	}

	public ReportFilter getReportFilter() {
		return reportFilter;
	}

	public String startReport(Report report) {
		this.report = report;
		report.start(reportFilter);
		return "report-success";
	}

	public String getHtmlReport() {
		return report.getHtmlResults();
	}

	public StreamedContent getPlainTextReport() {
		return getStringAsTextContent(
				report.getPlainTextResults(), "report.txt", "text/plain");
	}

	public StreamedContent getCsvReport() {
		return getStringAsTextContent(
				report.getCsvResults(), "report.csv", "text/csv");
	}

	private StreamedContent getStringAsTextContent(
			String content, String filename, String contentType) {
		InputStream stream = new ByteArrayInputStream(content.getBytes());
		return new DefaultStreamedContent(
				stream, contentType + "; charset=UTF-8", filename);
	}
}
