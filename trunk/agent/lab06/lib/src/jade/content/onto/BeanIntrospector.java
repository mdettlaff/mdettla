/*****************************************************************
JADE - Java Agent DEvelopment Framework is a framework to develop 
multi-agent systems in compliance with the FIPA specifications.
Copyright (C) 2000 CSELT S.p.A. 

GNU Lesser General Public License

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation, 
version 2.1 of the License. 

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the
Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA  02111-1307, USA.
*****************************************************************/

package jade.content.onto;

//#J2ME_EXCLUDE_FILE
//#APIDOC_EXCLUDE_FILE

import jade.content.Concept;
import jade.content.abs.AbsAggregate;
import jade.content.abs.AbsHelper;
import jade.content.abs.AbsObject;
import jade.content.abs.AbsTerm;
import jade.content.schema.ObjectSchema;

import java.lang.reflect.Array;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

class BeanIntrospector implements Introspector {
	private static final long serialVersionUID = 1L;

	private static final int ACC_ABSTRACT = 0x0400;
	private static final int ACC_INTERFACE = 0x0200;
	
	private Map<SlotKey, SlotAccessData> accessors;

	BeanIntrospector() {
		accessors = new HashMap<SlotKey, SlotAccessData>();
	}

	void addAccessors(Map<SlotKey, SlotAccessData> accessors) {
		this.accessors.putAll(accessors);
	}

	private Object invokeGetterMethod(Method method, Object obj) throws OntologyException {
		Object result = null;
		try {
			result = method.invoke(obj, (Object[]) null);
			if (result != null && java.util.Calendar.class.isAssignableFrom(result.getClass())) {
				// ontologically, Calendar is translated into a Date => convert the Calendar into a Date
				result = ((java.util.Calendar)result).getTime();
			}
		} catch (IllegalArgumentException iae) {
			result = new Object();
		} catch (Exception e) {
			throw new OntologyException("Error invoking getter method "+method.getName()+" on object "+obj, e);
		}
		return result;
	} 

	private void invokeSetterMethod(Method method, Object obj, Object value, Class clazz) throws OntologyException {
		try {
			if (java.util.Calendar.class.isAssignableFrom(clazz)) {
				// ontologically, Calendar is translated into a Date => convert the date back into a Calendar
				java.util.Calendar calendar = new java.util.GregorianCalendar();
                calendar.setTime((java.util.Date)value);
                value = calendar;
			} 
			else {
				value = BasicOntology.adjustPrimitiveValue(value, clazz);
			}
			Object[] params = new Object[] {value};
			method.invoke(obj, params);
		} catch (Exception e) {
			throw new OntologyException("Error invoking setter method "+method.getName()+" on object "+obj+" with parameter "+value, e);
		}
	}

	private static void disallowRecursion(String slotName, Object container, Object contained) throws OntologyException {
		if (container.equals(contained)) {
			throw new OntologyException("cannot recursively externalise slot "+slotName);
		}
	}

	private void externaliseAndSetAggregateSlot(Object obj, AbsObject abs, ObjectSchema schema, String slotName, Object slotValue, ObjectSchema slotSchema, Ontology referenceOnto) throws OntologyException {
		AbsAggregate absSlotValue;
		Class slotClass = slotValue.getClass();
		if (slotClass.isArray()) {
			absSlotValue = new AbsAggregate(slotSchema.getTypeName());
			for (int i = 0; i < Array.getLength(slotValue); i++) {
				Object object = Array.get(slotValue, i);
				disallowRecursion(slotName, obj, object);
				absSlotValue.add((AbsTerm)referenceOnto.fromObject(object));
			}
			AbsHelper.setAttribute(abs, slotName, absSlotValue);
		} else {
			Iterator iter;
			if (slotValue instanceof java.util.Collection) {
				iter = ((java.util.Collection)slotValue).iterator();
			} else if (slotValue instanceof jade.util.leap.Collection) {
				iter = ((jade.util.leap.Collection)slotValue).iterator();
			} else {
				throw new OntologyException("Cannot externalise value of class "+slotClass+" for aggregate slot "+slotName);
			}
			if (iter.hasNext() || schema.isMandatory(slotName)) {
				absSlotValue = new AbsAggregate(slotSchema.getTypeName());
				try {
					while (iter.hasNext()) {
						Object object = iter.next();
						disallowRecursion(slotName, obj, object);
						absSlotValue.add((AbsTerm)referenceOnto.fromObject(object));
					}
				} catch (ClassCastException cce) {
					throw new OntologyException("Non term object in aggregate");
				}

				AbsHelper.setAttribute(abs, slotName, absSlotValue);
			}
		}
	}

	private java.util.Collection createConcreteJavaCollection(Class clazz) throws InstantiationException, IllegalAccessException {
		int modifiers = clazz.getModifiers();
		java.util.Collection result = null;
		if ((modifiers & ACC_ABSTRACT) == 0 && (modifiers & ACC_INTERFACE) == 0) {
			// class is concrete, we can instantiate it directly
			result = (java.util.Collection) clazz.newInstance();
		} else {
			// class is either abstract or an interface, we have to somehow choose a concrete collection :-(
			if (java.util.List.class.isAssignableFrom(clazz)) {
				result = new java.util.ArrayList(); 
			} else if (java.util.Set.class.isAssignableFrom(clazz)) {
				result = new java.util.HashSet();
			}
		}
		return result;
	}

	private jade.util.leap.Collection createConcreteJadeCollection(Class clazz) throws InstantiationException, IllegalAccessException {
		int modifiers = clazz.getModifiers();
		jade.util.leap.Collection result = null;
		if ((modifiers & ACC_ABSTRACT) == 0 && (modifiers & ACC_INTERFACE) == 0) {
			// class is concrete, we can instantiate it directly
			result = (jade.util.leap.Collection) clazz.newInstance();
		} else {
			// class is either abstract or an interface, we have to somehow choose a concrete collection :-(
			if (jade.util.leap.List.class.isAssignableFrom(clazz)) {
				result = new jade.util.leap.ArrayList(); 
			} else if (jade.util.leap.Set.class.isAssignableFrom(clazz)) {
				result = new jade.util.leap.HashSet();
			}
		}
		return result;
	}

	private Object internaliseAggregateSlot(AbsAggregate absAggregate, ObjectSchema schema, Class clazz, Class elementClazz, Ontology referenceOnto) throws OntologyException {
		Object result;
		jade.util.leap.Iterator iterator = absAggregate.iterator();
		try {
			if (clazz.isArray()) {
				int index = 0;
				result = Array.newInstance(elementClazz, absAggregate.size());
				while (iterator.hasNext()) {
					Array.set(result, index, referenceOnto.toObject((AbsTerm)iterator.next()));
					index++;
				}
			} else if (java.util.Collection.class.isAssignableFrom(clazz)) {
				java.util.Collection javaCollection = createConcreteJavaCollection(clazz);
				if (javaCollection == null) {
					throw new OntologyException("cannot create a concrete collection for class "+clazz.getName());
				}
				result = javaCollection;
				while (iterator.hasNext()) {
					javaCollection.add(referenceOnto.toObject((AbsTerm)iterator.next()));
				}
			} else if (jade.util.leap.Collection.class.isAssignableFrom(clazz)) {
				jade.util.leap.Collection jadeCollection = createConcreteJadeCollection(clazz);
				result = jadeCollection;
				while (iterator.hasNext()) {
					jadeCollection.add(referenceOnto.toObject((AbsTerm)iterator.next()));
				}
			} else {
				throw new OntologyException("don't know how to handle aggregate slot of class "+clazz.getName());
			}
		} catch (InstantiationException ie) {
			throw new OntologyException("cannot instantiate aggregate slot of non-concrete class "+clazz.getName(), ie);
		} catch (IllegalAccessException iae) {
			throw new OntologyException("cannot instantiate aggregate slot through unaccessible default constructor of class "+clazz.getName(), iae);
		}
		return result;
	}

	public AbsObject externalise(Object obj, ObjectSchema schema, Class javaClass, Ontology referenceOnto) throws OntologyException {
		try {
			AbsObject abs = schema.newInstance();            
			String[] names = schema.getNames();

			// loop over slots
			for (int i = 0; i < names.length; ++i) {
				String slotName = names[i];

				Method getter;
				Class clazz = javaClass;
				SlotAccessData slotAccessData;

				// search for the getter of this slot through clazz hierarchy
				while(true) {
					slotAccessData = accessors.get(new SlotKey(clazz, slotName));
					if (slotAccessData != null) {
						getter = slotAccessData.getter;
						break;
					}

					// clazz does not have this getter, let's search it on its superclass
					clazz = clazz.getSuperclass();

					if (Object.class.equals(clazz)) {
						throw new OntologyException("cannot retrieve a getter for slot "+slotName+", class "+javaClass);
					}
				}

				Object slotValue = invokeGetterMethod(getter, obj);

				if (slotValue != null) {
					// basic sanity check: do not try to set a slot that is a reference to self
					disallowRecursion(slotName, obj, slotValue);
					// Agregate slots require a special handling 
					if (slotAccessData.aggregate) {
						ObjectSchema slotSchema = schema.getSchema(slotName);
						externaliseAndSetAggregateSlot(obj, abs, schema, slotName, slotValue, slotSchema, referenceOnto);
					}
					else {
						AbsObject absSlotValue = referenceOnto.fromObject(slotValue);
						AbsHelper.setAttribute(abs, slotName, absSlotValue);
					}
				}
			} 

			return abs;
		} catch (OntologyException oe) {
			throw oe;
		} catch (Exception e) {
			throw new OntologyException("Error externalising object "+obj+" with schema "+schema, e);
		} 
	}

	public Object internalise(AbsObject abs, ObjectSchema schema, Class javaClass, Ontology referenceOnto) throws UngroundedException, OntologyException {
		try {
			Object       obj = javaClass.newInstance();            
			String[]     names = schema.getNames();

			// loop over slots 
			for (int i = 0; i < names.length; ++i) {
				String slotName = names[i];
				AbsObject absObj = abs.getAbsObject(slotName);
				if (absObj != null) {
					Method setter;
					Class clazz = javaClass;
					SlotAccessData slotAccessData;

					// search for the setter of this slot trough clazz hierarchy
					while(true) {
						slotAccessData = accessors.get(new SlotKey(clazz, slotName));
						if (slotAccessData != null) {
							setter = slotAccessData.setter;
							break;
						}

						// clazz does not have this setter, let's search it on its superclass
						clazz = clazz.getSuperclass();
						if (Object.class.equals(clazz)) {
							throw new OntologyException("cannot retrieve a setter for slot "+slotName+", class "+javaClass);
						}
					}

					Object slotValue = null;
					// Agregate slots require a special handling 
					if (absObj.getAbsType() == AbsObject.ABS_AGGREGATE) {
						slotValue = internaliseAggregateSlot((AbsAggregate) absObj, schema, slotAccessData.type, slotAccessData.aggregateClass, referenceOnto);
					}
					else {
						slotValue = referenceOnto.toObject(absObj);
					}

					invokeSetterMethod(setter, obj, slotValue, slotAccessData.type);
				}             	
			}

			return obj;
		} 
		catch (OntologyException oe) {
			throw oe;
		} 
		catch (InstantiationException ie) {
			throw new OntologyException("Class "+javaClass.getName()+" can't be instantiated", ie);
		} 
		catch (IllegalAccessException iae) {
			throw new OntologyException("Class "+javaClass.getName()+" does not have an accessible constructor", iae);
		} 
		catch (Exception e) {
			throw new OntologyException("Schema and Java class do not match", e);
		} 
	}

	public void checkClass(ObjectSchema schema, Class javaClass, Ontology onto) throws OntologyException {
		// FIXME not implemented yet
	}

	public Object getSlotValue(String slotName, Object obj, ObjectSchema schema) throws OntologyException {
		Method getter;
		Class javaClass = obj.getClass();
		Class clazz = javaClass;
		SlotAccessData slotAccessData;

		// search for the getter of this slot through clazz hierarchy
		while(true) {
			slotAccessData = accessors.get(new SlotKey(clazz, slotName));
			if (slotAccessData != null) {
				getter = slotAccessData.getter;
				break;
			}

			// clazz does not have this getter, let's search it on its superclass
			clazz = clazz.getSuperclass();
			if (!Concept.class.isAssignableFrom(clazz)) {
				throw new OntologyException("cannot retrieve a getter for slot "+slotName+", class "+javaClass);
			}
		}

		return invokeGetterMethod(getter, obj);
	}

	public void setSlotValue(String slotName, Object slotValue, Object obj, ObjectSchema schema) throws OntologyException {
		Method setter;
		Class javaClass = obj.getClass();
		Class clazz = javaClass;
		SlotAccessData slotAccessData;

		// search for the setter of this slot trough clazz hierarchy
		while(true) {
			slotAccessData = accessors.get(new SlotKey(clazz, slotName));
			if (slotAccessData != null) {
				setter = slotAccessData.setter;
				break;
			}

			// clazz does not have this setter, let's search it on its superclass
			clazz = clazz.getSuperclass();
			if (!Concept.class.isAssignableFrom(clazz)) {
				throw new OntologyException("cannot retrieve a setter for slot "+slotName+", class "+javaClass);
			}
		}

		invokeSetterMethod(setter, obj, slotValue, slotAccessData.type);
	}
}
