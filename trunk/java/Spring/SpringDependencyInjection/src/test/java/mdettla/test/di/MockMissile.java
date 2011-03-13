package mdettla.test.di;

import mdettla.test.di.Missile;

class MockMissile implements Missile {

    public void launch() {
		System.out.println("Pretending to launch a missile.");
    }

    public boolean isLaunched() {
        return true;
    }
}
