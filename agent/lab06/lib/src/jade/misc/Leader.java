package jade.misc;

import jade.content.Predicate;
import jade.core.AID;

public class Leader implements Predicate {
	private AID name;
	private long age;
	
	public Leader() {
	}
	
	public Leader(AID name, long age) {
		this.name = name;
		this.age = age;
	}
	
	public long getAge() {
		return age;
	}
	public void setAge(long age) {
		this.age = age;
	}
	public AID getName() {
		return name;
	}
	public void setName(AID name) {
		this.name = name;
	}
}
