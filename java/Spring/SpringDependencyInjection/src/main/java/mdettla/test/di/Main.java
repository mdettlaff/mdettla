package mdettla.test.di;

import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

public class Main {

	public static void main(String[] args) {
		ApplicationContext context =
			new ClassPathXmlApplicationContext("mdettla/test/di/appContext.xml");
		MissileLauncher launcher = context.getBean("launcher", MissileLauncher.class);

		String password = args.length > 0 ? args[0] : "";
		boolean isLaunched = launcher.launchMissile(password);
		if (!isLaunched) {
			System.err.println("Problem during launch.");
		}
	}
}
