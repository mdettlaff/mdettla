package mdettla.test.di;

import static org.junit.Assert.assertTrue;

import javax.annotation.Resource;

import mdettla.test.di.MissileLauncher;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("testContext.xml")
public class MissileLauncherTest {

	private MissileLauncher launcher;

	@Resource
	public void setLauncher(MissileLauncher launcher) {
		this.launcher = launcher;
	}

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
