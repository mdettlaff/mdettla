package mdettla.keyboard.ga;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class GetOpt {

	private final Map<String, String> options;
	private final List<String> arguments;

	public GetOpt(String[] args) {
		options = new HashMap<String, String>();
		arguments = new ArrayList<String>();
		for (String arg : args) {
			if (arg.matches(".+=.+")) {
				String[] keyValue = arg.split("=");
				options.put(keyValue[0], keyValue[1]);
			} else {
				arguments.add(arg);
			}
		}
	}

	public boolean isOptionSet(String optionName) {
		return options.containsKey(optionName);
	}

	public String getValue(String optionName) {
		return options.get(optionName);
	}

	public int getIntValue(String optionName) {
		return Integer.valueOf(getValue(optionName));
	}

	public boolean getBooleanValue(String optionName) {
		return Boolean.valueOf(getValue(optionName));
	}

	public double getDoubleValue(String optionName) {
		return Double.valueOf(getValue(optionName));
	}

	public List<String> getArguments() {
		return arguments;
	}
}
