package parasoft.centrap.web;

import java.io.Serializable;

import javax.enterprise.context.SessionScoped;
import javax.inject.Inject;
import javax.inject.Named;

import parasoft.centrap.reports.Report;
import parasoft.centrap.reports.ReportFilter;
import parasoft.centrap.web.util.FileDownloader;

@Named
@SessionScoped
public class ReportController implements Serializable {

	private ReportFilter reportFilter;
	private FileDownloader fileDownloader;
	private Report report = Report.EMPTY;

	public ReportController() {
		this.reportFilter = new ReportFilter();
	}

	@Inject
	public ReportController(FileDownloader fileDownloader) {
		this();
		this.fileDownloader = fileDownloader;
	}

	public ReportFilter getReportFilter() {
		return reportFilter;
	}

	public String startReport(Report report) {
		this.report = report;
		report.setFilter(reportFilter);
		report.start();
		return "report-success";
	}

	public String getHtmlResults() {
		return report.getHtmlResults();
	}

	public void downloadPlainTextReport() {
		fileDownloader.downloadFile(
				report.getPlainTextResults(), "report.txt", "text/plain");
	}

	public void downloadCsvReport() {
		fileDownloader.downloadFile(
				report.getCsvResults(), "report.csv", "text/csv");
	}
}
