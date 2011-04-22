package mdettla.classycletree;

import java.util.ArrayList;
import java.util.List;

public class GetOpt {

	private static final String LONG_OPT = "--";

	private List<String> arguments;
	private List<String> options;

	public GetOpt(String[] args) {
		arguments = new ArrayList<String>();
		options = new ArrayList<String>();
		for (String arg : args) {
			if (arg.startsWith(LONG_OPT)) {
				options.add(arg.substring(LONG_OPT.length(), arg.length()));
			} else {
				arguments.add(arg);
			}
		}
	}

	public boolean isOptionSet(String option) {
		return options.contains(option);
	}

	public List<String> getArguments() {
		return arguments;
	}
}
