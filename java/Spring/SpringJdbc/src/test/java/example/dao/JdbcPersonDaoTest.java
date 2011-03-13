package example.dao;

import static org.junit.Assert.assertEquals;

import example.domain.Person;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

public class JdbcPersonDaoTest extends AbstractJdbcDaoTest {

	private PersonDao dao;

	@Before
	public void setUp() {
		dao = new JdbcPersonDao(database);
	}

	@Test
	public void testFindAll() {
		List<Person> actual = dao.findAll();
		List<Person> expected = Arrays.asList(
				new Person("Foo", "Bar"),
				new Person("Baz", "Qux"),
				new Person("Foo", "Quux"));
		assertEquals(expected, actual);
	}

	@Test
	public void testFindByFirstName() {
		List<Person> actual = dao.findByFirstName("Foo");
		List<Person> expected = Arrays.asList(
				new Person("Foo", "Bar"),
				new Person("Foo", "Quux"));
		assertEquals(expected, actual);
	}

	@Test
	public void testCreate() {
		dao.create("John", "Doe");
		List<Person> actual = dao.findByFirstName("John");
		List<Person> expected = Arrays.asList(new Person("John", "Doe"));
		assertEquals(expected, actual);
	}

	@Test
	public void testRemoveAll() {
		dao.removeAll();
		assertEquals(Collections.emptyList(), dao.findAll());
	}

	@Test
	public void testRemove() {
		dao.remove("Foo", "Bar");
		dao.remove("Baz", "Qux");
		List<Person> actual = dao.findAll();
		List<Person> expected = Arrays.asList(new Person("Foo", "Quux"));
		assertEquals(expected, actual);
	}
}
