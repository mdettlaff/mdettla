package mdettla.test.di;

public class MissileLauncher {

    private final Missile missile;

    public MissileLauncher(Missile missile) {
        this.missile = missile;
    }

    public boolean launchMissile(String password) {
        if ("11111".equals(password)) {
            missile.launch();
            return missile.isLaunched();
        } else {
            throw new SecurityException("Wrong password.");
        }
    }
}
