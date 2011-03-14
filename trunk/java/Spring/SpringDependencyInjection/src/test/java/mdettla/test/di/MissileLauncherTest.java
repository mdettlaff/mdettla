package mdettla.test.di;

import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.AbstractJUnit4SpringContextTests;

@ContextConfiguration
public class MissileLauncherTest extends AbstractJUnit4SpringContextTests {

	@Autowired
	private MissileLauncher launcher;

	@Test(expected = SecurityException.class)
	public void testUnsucccessfulLaunch() {
	    launcher.launchMissile("555");
	}

	@Test
	public void testSucccessfulLaunch() {
	    boolean isLaunched = launcher.launchMissile("11111");
	    assertTrue(isLaunched);
	}
}
