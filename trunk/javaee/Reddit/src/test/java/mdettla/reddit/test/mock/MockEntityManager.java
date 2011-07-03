package mdettla.reddit.test.mock;

import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.EntityTransaction;
import javax.persistence.FlushModeType;
import javax.persistence.LockModeType;
import javax.persistence.Parameter;
import javax.persistence.Query;
import javax.persistence.TemporalType;
import javax.persistence.TypedQuery;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.metamodel.Metamodel;

public class MockEntityManager implements EntityManager {

	@Override
	public void clear() {
	}

	@Override
	public void close() {
	}

	@Override
	public boolean contains(Object arg0) {
		return false;
	}

	@Override
	public Query createNamedQuery(String arg0) {
		return null;
	}

	@Override
	public <T> TypedQuery<T> createNamedQuery(String arg0, Class<T> arg1) {
		return null;
	}

	@Override
	public Query createNativeQuery(String arg0) {
		return null;
	}

	@Override
	public Query createNativeQuery(String arg0, @SuppressWarnings("rawtypes") Class arg1) {
		return null;
	}

	@Override
	public Query createNativeQuery(String arg0, String arg1) {
		return null;
	}

	@Override
	public Query createQuery(String arg0) {
		return new Query() {

			@Override
			public <T> T unwrap(Class<T> arg0) {
				return null;
			}

			@Override
			public Query setParameter(int arg0, Date arg1, TemporalType arg2) {
				return null;
			}

			@Override
			public Query setParameter(int arg0, Calendar arg1, TemporalType arg2) {
				return null;
			}

			@Override
			public Query setParameter(String arg0, Date arg1, TemporalType arg2) {
				return null;
			}

			@Override
			public Query setParameter(String arg0, Calendar arg1, TemporalType arg2) {
				return null;
			}

			@Override
			public Query setParameter(Parameter<Date> arg0, Date arg1, TemporalType arg2) {
				return null;
			}

			@Override
			public Query setParameter(Parameter<Calendar> arg0, Calendar arg1,
					TemporalType arg2) {
				return null;
			}

			@Override
			public Query setParameter(int arg0, Object arg1) {
				return null;
			}

			@Override
			public Query setParameter(String arg0, Object arg1) {
				return null;
			}

			@Override
			public <T> Query setParameter(Parameter<T> arg0, T arg1) {
				return null;
			}

			@Override
			public Query setMaxResults(int arg0) {
				return null;
			}

			@Override
			public Query setLockMode(LockModeType arg0) {
				return null;
			}

			@Override
			public Query setHint(String arg0, Object arg1) {
				return null;
			}

			@Override
			public Query setFlushMode(FlushModeType arg0) {
				return null;
			}

			@Override
			public Query setFirstResult(int arg0) {
				return null;
			}

			@Override
			public boolean isBound(Parameter<?> arg0) {
				return false;
			}

			@Override
			public Object getSingleResult() {
				return null;
			}

			@Override
			@SuppressWarnings("rawtypes")
			public List getResultList() {
				return null;
			}

			@Override
			public Set<Parameter<?>> getParameters() {
				return null;
			}

			@Override
			public Object getParameterValue(int arg0) {
				return null;
			}

			@Override
			public Object getParameterValue(String arg0) {
				return null;
			}

			@Override
			public <T> T getParameterValue(Parameter<T> arg0) {
				return null;
			}

			@Override
			public <T> Parameter<T> getParameter(int arg0, Class<T> arg1) {
				return null;
			}

			@Override
			public <T> Parameter<T> getParameter(String arg0, Class<T> arg1) {
				return null;
			}

			@Override
			public Parameter<?> getParameter(int arg0) {
				return null;
			}

			@Override
			public Parameter<?> getParameter(String arg0) {
				return null;
			}

			@Override
			public int getMaxResults() {
				return 0;
			}

			@Override
			public LockModeType getLockMode() {
				return null;
			}

			@Override
			public Map<String, Object> getHints() {
				return null;
			}

			@Override
			public FlushModeType getFlushMode() {
				return null;
			}

			@Override
			public int getFirstResult() {
				return 0;
			}

			@Override
			public int executeUpdate() {
				return 0;
			}
		};
	}

	@Override
	public <T> TypedQuery<T> createQuery(CriteriaQuery<T> arg0) {
		return null;
	}

	@Override
	public <T> TypedQuery<T> createQuery(String arg0, Class<T> arg1) {
		return null;
	}

	@Override
	public void detach(Object arg0) {
	}

	@Override
	public <T> T find(Class<T> arg0, Object arg1) {
		return null;
	}

	@Override
	public <T> T find(Class<T> arg0, Object arg1, Map<String, Object> arg2) {
		return null;
	}

	@Override
	public <T> T find(Class<T> arg0, Object arg1, LockModeType arg2) {
		return null;
	}

	@Override
	public <T> T find(Class<T> arg0, Object arg1, LockModeType arg2,
			Map<String, Object> arg3) {
		return null;
	}

	@Override
	public void flush() {
	}

	@Override
	public CriteriaBuilder getCriteriaBuilder() {
		return null;
	}

	@Override
	public Object getDelegate() {
		return null;
	}

	@Override
	public EntityManagerFactory getEntityManagerFactory() {
		return null;
	}

	@Override
	public FlushModeType getFlushMode() {
		return null;
	}

	@Override
	public LockModeType getLockMode(Object arg0) {
		return null;
	}

	@Override
	public Metamodel getMetamodel() {
		return null;
	}

	@Override
	public Map<String, Object> getProperties() {
		return null;
	}

	@Override
	public <T> T getReference(Class<T> arg0, Object arg1) {
		return null;
	}

	@Override
	public EntityTransaction getTransaction() {
		return new MockEntityTransaction();
	}

	@Override
	public boolean isOpen() {
		return false;
	}

	@Override
	public void joinTransaction() {
	}

	@Override
	public void lock(Object arg0, LockModeType arg1) {
	}

	@Override
	public void lock(Object arg0, LockModeType arg1, Map<String, Object> arg2) {
	}

	@Override
	public <T> T merge(T arg0) {
		return null;
	}

	@Override
	public void persist(Object arg0) {
	}

	@Override
	public void refresh(Object arg0) {
	}

	@Override
	public void refresh(Object arg0, Map<String, Object> arg1) {
	}

	@Override
	public void refresh(Object arg0, LockModeType arg1) {
	}

	@Override
	public void refresh(Object arg0, LockModeType arg1, Map<String, Object> arg2) {
	}

	@Override
	public void remove(Object arg0) {
	}

	@Override
	public void setFlushMode(FlushModeType arg0) {
	}

	@Override
	public void setProperty(String arg0, Object arg1) {
	}

	@Override
	public <T> T unwrap(Class<T> arg0) {
		return null;
	}
}
