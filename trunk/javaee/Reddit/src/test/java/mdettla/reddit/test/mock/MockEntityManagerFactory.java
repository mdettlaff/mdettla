package mdettla.reddit.test.mock;

import java.util.Map;

import javax.persistence.Cache;
import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.PersistenceUnitUtil;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.metamodel.Metamodel;

public class MockEntityManagerFactory implements EntityManagerFactory {

	@Override
	public void close() {
	}

	@Override
	public EntityManager createEntityManager() {
		return new MockEntityManager();
	}

	@Override
	public EntityManager createEntityManager(@SuppressWarnings("rawtypes") Map arg0) {
		return new MockEntityManager();
	}

	@Override
	public Cache getCache() {
		return null;
	}

	@Override
	public CriteriaBuilder getCriteriaBuilder() {
		return null;
	}

	@Override
	public Metamodel getMetamodel() {
		return null;
	}

	@Override
	public PersistenceUnitUtil getPersistenceUnitUtil() {
		return null;
	}

	@Override
	public Map<String, Object> getProperties() {
		return null;
	}

	@Override
	public boolean isOpen() {
		return false;
	}
}
