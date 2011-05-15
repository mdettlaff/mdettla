package parasoft.centrap.web;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.Serializable;

import javax.enterprise.context.SessionScoped;
import javax.faces.context.FacesContext;
import javax.inject.Named;
import javax.servlet.http.HttpServletResponse;

import parasoft.centrap.reports.RaportZakonczonychPromocji;
import parasoft.centrap.reports.Report;
import parasoft.centrap.reports.ReportFilter;

@Named
@SessionScoped
public class ReportController implements Serializable {

	private ReportFilter reportFilter;
	private Report report;

	public ReportController() {
		this.reportFilter = new ReportFilter();
		report = new RaportZakonczonychPromocji();
	}

	public ReportFilter getReportFilter() {
		return reportFilter;
	}

	public String startReport() {
		report.setFilter(reportFilter);
		report.start();
		return "success";
	}

	public String getHtmlResults() {
		return report.getHtmlResults();
	}

	public void downloadPlainTextReport() {
		downloadFile(report.getPlainTextResults(), "report.txt", "text/plain");
	}

	public void downloadCsvReport() {
		downloadFile(report.getCsvResults(), "report.csv", "text/csv");
	}

	private void downloadFile(
			String content, String filename, String mimeType) {
		try {
			FacesContext context = FacesContext.getCurrentInstance();
			HttpServletResponse response =
				getFileDownloadResponse(context, filename, mimeType);
			PrintWriter writer = response.getWriter();
			writer.println(content);
			context.responseComplete();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private HttpServletResponse getFileDownloadResponse(
			FacesContext context, String filename, String mimeType) {
		HttpServletResponse response =
			(HttpServletResponse)context.getExternalContext().getResponse();
		response.setHeader(
				"Content-disposition",
				"attachment; filename=" + filename);
		response.setContentType(mimeType + "; charset=UTF-8");
		return response;
	}
}
