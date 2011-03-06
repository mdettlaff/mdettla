package nibblr.domain;

import java.util.Date;

public class FeedItem {

	private String title;
	private String url;
	private String htmlContent;
	private Date date;

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getURL() {
		return url;
	}

	public void setUrl(String url) {
		this.url = url;
	}

	public String getHTMLContent() {
		return htmlContent;
	}

	public void setHTMLContent(String htmlContent) {
		this.htmlContent = htmlContent;
	}

	public void setDate(Date date) {
		this.date = date;
	}

	public Date getDate() {
		return date;
	}

	@Override
	public String toString() {
		return getClass().getSimpleName() + "[" +
		"title=" + title +
		", url=" + url +
		", htmlContent=" + htmlContent +
		", date=" + date +
		"]";
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((htmlContent == null) ? 0 : htmlContent.hashCode());
		result = prime * result + ((title == null) ? 0 : title.hashCode());
		result = prime * result + ((url == null) ? 0 : url.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		FeedItem other = (FeedItem) obj;
		if (htmlContent == null) {
			if (other.htmlContent != null)
				return false;
		} else if (!htmlContent.equals(other.htmlContent))
			return false;
		if (title == null) {
			if (other.title != null)
				return false;
		} else if (!title.equals(other.title))
			return false;
		if (url == null) {
			if (other.url != null)
				return false;
		} else if (!url.equals(other.url))
			return false;
		return true;
	}
}
