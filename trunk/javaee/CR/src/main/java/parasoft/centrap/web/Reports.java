package parasoft.centrap.web;

import javax.ejb.EJB;
import javax.inject.Named;

import parasoft.centrap.reports.Report;
import parasoft.centrap.reports.DiscountsReport;
import parasoft.centrap.reports.SalesReport;

@Named
public class Reports {

	@EJB
	private SalesReport salesReport;
	@EJB
	private DiscountsReport discountsReport;

	public Report getSalesReport() {
		return salesReport;
	}

	public Report getDiscountsReport() {
		return discountsReport;
	}
}
