package mdettla.reddit.test.mock;

import javax.persistence.EntityTransaction;

public class MockEntityTransaction implements EntityTransaction {

	@Override
	public void begin() {
	}

	@Override
	public void commit() {
	}

	@Override
	public boolean getRollbackOnly() {
		return false;
	}

	@Override
	public boolean isActive() {
		return false;
	}

	@Override
	public void rollback() {
	}

	@Override
	public void setRollbackOnly() {
	}
}
