package example.dao;

import java.util.List;

import example.domain.Person;

public interface PersonDao {

	void create(String firstName, String lastName);

	List<Person> findByFirstName(String firstName);

	List<Person> findAll();

	void remove(String firstName, String lastName);

	void removeAll();
}
