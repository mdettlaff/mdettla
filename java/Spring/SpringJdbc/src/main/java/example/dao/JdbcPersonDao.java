package example.dao;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

import javax.sql.DataSource;

import org.springframework.jdbc.core.simple.SimpleJdbcOperations;
import org.springframework.jdbc.core.simple.SimpleJdbcTemplate;
import org.springframework.jdbc.core.RowMapper;

import example.domain.Person;

public class JdbcPersonDao implements PersonDao {

	private final SimpleJdbcOperations jdbc;

	public JdbcPersonDao(DataSource dataSource) {
		jdbc = new SimpleJdbcTemplate(dataSource);
	}

	public void create(String firstName, String lastName) {
		jdbc.update("INSERT INTO person (firstname, lastname) VALUES (?, ?)",
				firstName, lastName);
	}

	public List<Person> findByFirstName(String firstName) {
		return jdbc.query(
				"SELECT firstname, lastname FROM person WHERE firstname = ?",
				new PersonRowMapper(), firstName);
	}

	public List<Person> findAll() {
		return jdbc.query("SELECT firstname, lastname FROM person",
				new PersonRowMapper());
	}

	public void remove(String firstName, String lastName) {
		jdbc.update("DELETE FROM person WHERE firstname = ? AND lastname = ?",
				firstName, lastName);
	}

	public void removeAll() {
		jdbc.update("DELETE FROM person");
	}


	private static class PersonRowMapper implements RowMapper<Person> {

		@Override
		public Person mapRow(ResultSet rs, int line) throws SQLException {
			String firstName = rs.getString("firstname");
			String lastName = rs.getString("lastname");
			return new Person(firstName, lastName);
		}
	}
}
