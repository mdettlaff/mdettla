package parasoft.centrap.web.util;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.Serializable;

import javax.faces.context.FacesContext;
import javax.servlet.http.HttpServletResponse;

public class FileDownloader implements Serializable {

	public void downloadFile(
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
