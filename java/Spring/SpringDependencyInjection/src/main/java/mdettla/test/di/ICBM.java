package mdettla.test.di;

public class ICBM implements Missile {

	private boolean isLaunched;

	public void launch() {
		System.out.println("ICBM missile launched.");
		isLaunched = true;
	}

	public boolean isLaunched() {
		return isLaunched;
	}
}
