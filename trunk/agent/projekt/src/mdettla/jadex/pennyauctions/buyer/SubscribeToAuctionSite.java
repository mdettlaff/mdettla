package mdettla.jadex.pennyauctions.buyer;

import jadex.adapter.fipa.AgentDescription;
import jadex.adapter.fipa.AgentIdentifier;
import jadex.adapter.fipa.SFipa;
import jadex.adapter.fipa.ServiceDescription;
import jadex.runtime.IGoal;
import jadex.runtime.IMessageEvent;
import jadex.runtime.Plan;

import java.util.Arrays;

public class SubscribeToAuctionSite extends Plan {
	private static final long serialVersionUID = 1L;

	@Override
	public void body() {
		// Search for a service with given type.
		getLogger().info("szukam serwisów aukcyjnych...");
		IGoal df_search = createGoal("df_search");
		AgentDescription desc = new AgentDescription();
		desc.addService(new ServiceDescription(null, "penny_auction", null));
		df_search.getParameter("description").setValue(desc);
		// for a remote DF
		// AgentIdentifier df = ...
		// modify.getParameter("df").setValue(df);
		dispatchSubgoalAndWait(df_search);
		AgentDescription[] result = (AgentDescription[])df_search
		.getParameterSet("result").getValues();
		if (result.length != 0) {
			AgentIdentifier[] serviceproviders = new AgentIdentifier[result.length];
			for (int i = 0; i < result.length; i++) {
				serviceproviders[i] = result[i].getName();
			}
			getLogger().info("znalezione serwisy: " +
					Arrays.toString(serviceproviders));
			IMessageEvent me = createMessageEvent("register");
			me.setContent(getAgentName());
			for (AgentIdentifier auctionSite : serviceproviders) {
				me.getParameterSet(SFipa.RECEIVERS).addValue(auctionSite);
			}
			sendMessage(me);
		}
	}
}
