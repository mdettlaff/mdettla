package example.dao;

import org.junit.After;
import org.junit.Before;

import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseBuilder;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabase;

public abstract class AbstractJdbcDaoTest {

	protected EmbeddedDatabase database;

	@Before
	public void setUpDatabase() {
		database = new EmbeddedDatabaseBuilder().addDefaultScripts().build();
	}

	@After
	public void tearDownDatabase() {
		database.shutdown();
	}
}
