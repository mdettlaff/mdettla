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

import jade.content.AgentAction;
import jade.content.Concept;
import jade.content.Predicate;
import jade.content.onto.annotations.AggregateResult;
import jade.content.onto.annotations.AggregateSlot;
import jade.content.onto.annotations.Element;
import jade.content.onto.annotations.Result;
import jade.content.onto.annotations.Slot;
import jade.content.onto.annotations.SuppressSlot;
import jade.content.schema.AgentActionSchema;
import jade.content.schema.AggregateSchema;
import jade.content.schema.ConceptSchema;
import jade.content.schema.ObjectSchema;
import jade.content.schema.PredicateSchema;
import jade.content.schema.TermSchema;
import jade.util.Logger;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

class BeanOntologyBuilder {

	private final static Logger logger = Logger.getMyLogger(BeanOntologyBuilder.class.getName());

	private static final String GETTER_PREFIX = "get";
	private static final String SETTER_PREFIX = "set";
	private static final Object GET_CLASS_METHOD = "getClass";

	private Ontology ontology;
	private BeanIntrospector introspector;

	BeanOntologyBuilder(Ontology ontology) {
		this.ontology = ontology;
		Introspector ontoIntrospector = ontology.getIntrospector();
		introspector = (BeanIntrospector)ontoIntrospector;
	}

	private static boolean isGetter(Method method) {
		/*
		 * a getter method
		 *   - takes no parameters
		 *   - has a return value
		 *   - its name starts with "get"
		 *   - its 4th char is uppercase or is "_"
		 *   - its name is not "getClass" :-)
		 */
		String methodName = method.getName();
		if (methodName.length() < 4) {
			// it is surely too short
			return false;
		}
		if (!methodName.startsWith(GETTER_PREFIX)) {
			// it does not start with "get"
			return false;
		}
		if (!Character.isUpperCase(methodName.charAt(3)) && '_' != methodName.charAt(3)) {
			// its 4th char is not uppercase
			return false;
		}
		if (void.class.equals(method.getReturnType())) {
			// it does not have a return value
			return false;
		}
		if (method.getParameterTypes().length > 0) {
			// it takes some parameters
			return false;
		}
		if (methodName.equals(GET_CLASS_METHOD)) {
			// it is "getClass", to be discarded
			return false;
		}
		return true;
	}

	private static boolean isSetter(Method method) {
		/*
		 * a setter method takes one parameter, does not have a return value and its name starts with "set" and its 4th char is uppercase or is "_"
		 */
		String methodName = method.getName(); 
		if (methodName.length() < 4) {
			// it is surely too short
			return false;
		}
		if (!methodName.startsWith(SETTER_PREFIX)) {
			// it does not start with "set"
			return false;
		}
		if (!Character.isUpperCase(methodName.charAt(3)) && '_' != methodName.charAt(3)) {
			// its 4th char is not uppercase
			return false;
		}
		if (!void.class.equals(method.getReturnType())) {
			// it has a return value
			return false;
		}
		if (method.getParameterTypes().length != 1) {
			// it does not take exactly 1 parameter
			return false;
		}
		return true;
	}

	private static String buildPropertyNameFromGetter(Method getter) {
		/*
		 * 1) rip of the "get" prefix from method's name
		 * 2) make lower case the 1st char of the result
		 * 
		 * method name       slot name
		 * --------------    ---------
		 * getThatThing() -> thatThing
		 */
		String getterName = getter.getName();
		StringBuilder sb = new StringBuilder();
		sb.append(Character.toLowerCase(getterName.charAt(3)));
		sb.append(getterName.substring(4));
		return sb.toString();
	}

	private static String buildSetterNameFromBeanPropertyName(String beanPropertyName) {
		StringBuilder sb = new StringBuilder(SETTER_PREFIX);
		sb.append(Character.toUpperCase(beanPropertyName.charAt(0)));
		sb.append(beanPropertyName.substring(1));
		return sb.toString();
	}

	private static boolean accessorsAreConsistent(Method getter, Method setter) {
		/*
		 *  we have for sure a getter and a setter, so we don't need
		 *  to check the number of parameters and the existence of the return value
		 *  but only their type consistency
		 */
		return getter.getReturnType().equals(setter.getParameterTypes()[0]);
	}

	private static String getSchemaNameFromClass(Class clazz) {
		String result = clazz.getSimpleName();
		Element annotationElement = (Element)clazz.getAnnotation(Element.class);
		if (annotationElement != null) {
			if (!Element.USE_CLASS_SIMPLE_NAME.equals(annotationElement.name())) {
				result = annotationElement.name();
			}
		}
		return result;
	}

	private static Map<SlotKey, SlotAccessData> buildAccessorsMap(Class clazz, Method[] methodsArray) {
		Map<SlotKey, SlotAccessData> result = new HashMap<SlotKey, SlotAccessData>();
		List<Method> getters = new ArrayList<Method>();
		Map<String, Method> setters = new HashMap<String, Method>();
		for (Method method: methodsArray) {
			if (method.getAnnotation(SuppressSlot.class) == null) {
				if (isGetter(method)) {
					getters.add(method);
				} else if (isSetter(method)) {
					setters.put(method.getName(), method);
				}
			}
		}

		/*
		 * now that we have a list of getters and a map of setters, we iterate through getters
		 * searching for the matching setter; when we find it, we store the couple in a SlotAccessData
		 */
		Iterator<Method> gettersIter = getters.iterator();
		Method getter, setter;
		String setterName;
		Class<?> slotClazz;
		SlotAccessData sad;
		String propertyName;
		Slot slotAnnotation;
		Class aggregateType;
		AggregateSlot aggregateSlotAnnotation;
		boolean mandatory;
		int cardMin;
		int cardMax;

		while (gettersIter.hasNext()) {
			getter = gettersIter.next();
			slotClazz = getter.getReturnType();
			mandatory = false;
			cardMin = 0;
			cardMax = ObjectSchema.UNLIMITED;
			aggregateType = null;
			slotAnnotation = getter.getAnnotation(Slot.class);
			aggregateSlotAnnotation = getter.getAnnotation(AggregateSlot.class);

			// build the name of the bean property starting from the getter
			propertyName = buildPropertyNameFromGetter(getter);
			// build the name of the setter name coherent with bean rules
			setterName = buildSetterNameFromBeanPropertyName(propertyName);
			setter = setters.get(setterName);
			if (setter != null) {
				// ok, we have getter and setter, we need to check parameters consistency and we are done
				if (accessorsAreConsistent(getter, setter)) {
					/*
					 * if getter @Slot annotation provides a name, use it; otherwise
					 * use the bean property name
					 */
					String slotName = propertyName;
					if (slotAnnotation != null) {
						/*
						 * if there's a @Slot annotation which specifies the name of the slot, use it
						 */
						if (!Slot.USE_METHOD_NAME.equals(slotAnnotation.name())) {
							slotName = slotAnnotation.name();
						}
						mandatory = slotAnnotation.mandatory();
					}
					// if present, use getter @AggregateSlot annotation data
					if (SlotAccessData.isAggregate(slotClazz)) {
						if (slotClazz.isArray()) {
							// extract the type of array elements
							aggregateType = slotClazz.getComponentType();
						}
						Type slotType = getter.getGenericReturnType(); 
						if (slotType instanceof ParameterizedType) {
							ParameterizedType slotParameterizedType = (ParameterizedType)slotType;
							Type[] actuals = slotParameterizedType.getActualTypeArguments();
							// slotType must be an array or a Collection => we expect only 1 item in actuals
							// get first element
							if (actuals.length > 0) {
								aggregateType = (Class)actuals[0];
							}
						}
//						if (slotType has generics) {
//							aggregateType = type from generics;
//						}
						if (aggregateSlotAnnotation != null) {
							cardMin = aggregateSlotAnnotation.cardMin();
							cardMax = aggregateSlotAnnotation.cardMax();
							if (!Object.class.equals(aggregateSlotAnnotation.type())) {
								aggregateType = aggregateSlotAnnotation.type();
							}
						}
					}
					sad = new SlotAccessData(slotClazz, getter, setter, mandatory, aggregateType, cardMin, cardMax);
					result.put(new SlotKey(clazz, slotName), sad);
				} else {
					// TODO it's not a bean property, maybe we could generate a warning...
				}
			}
		}
		return result;
	}

	private static String getAggregateSchemaName(Class clazz) {
		String result = null;
		if (SlotAccessData.isSequence(clazz)) {
			result = BasicOntology.SEQUENCE;
		} else if (SlotAccessData.isSet(clazz)) {
			result = BasicOntology.SET;
		}
		return result;
	}

	private static AggregateSchema tryToGetAggregateSchema(Class clazz) {
		AggregateSchema result = null;

		try {
			String schemaName = getAggregateSchemaName(clazz);
			if (schemaName != null) {
				result = (AggregateSchema)BasicOntology.getInstance().getSchema(schemaName);
			}
		} catch (OntologyException oe) {
			// we should never arrive here
			oe.printStackTrace();
		}
		return result;
	}

	private TermSchema supplySchemaForClassFlat(Class clazz, boolean skipClassChecking) throws OntologyException, BeanOntologyException {
		TermSchema ts;
		ObjectSchema os;
		if (java.util.Calendar.class.isAssignableFrom(clazz)) {
			// ontologically, Calendar is translated into a Date
			os = ontology.getSchema(java.util.Date.class);
		} else {
			os = ontology.getSchema(clazz);
		}
		if (os == null) {
			if (!skipClassChecking && !Concept.class.isAssignableFrom(clazz)) {
				throw new BeanOntologyException("cannot add a slot of class "+clazz.getName()+" since it does not implement Concept");
			}
			os = doAddFlatSchema(clazz, skipClassChecking);
		}
		if (os != null && os instanceof TermSchema) {
			ts = (TermSchema)os;
		} else {
			throw new BeanOntologyException("cannot add a slot of class "+clazz.getName()+" since it does not extend TermSchema");
		}
		return ts;
	}

	private TermSchema supplySchemaForClassRecursive(Class clazz, boolean skipClassChecking) throws OntologyException, BeanOntologyException {
		TermSchema ts;
		ObjectSchema os;
		if (java.util.Calendar.class.isAssignableFrom(clazz)) {
			// ontologically, Calendar is translated into a Date
			os = ontology.getSchema(java.util.Date.class);
		} else {
			os = ontology.getSchema(clazz);
		}
		if (os == null) {
			if (!skipClassChecking && !Concept.class.isAssignableFrom(clazz)) {
				throw new BeanOntologyException("cannot add a slot of class "+clazz.getName()+" since it does not implement Concept");
			}
			ts = (ConceptSchema)doAddHierarchicalSchema(clazz, skipClassChecking);
		}
		if (os != null && os instanceof TermSchema) {
			ts = (TermSchema)os;
		} else {
			throw new BeanOntologyException("cannot add a slot of class "+clazz.getName()+" since it does not extend TermSchema");
		}
		return ts;
	}

	private void addTermSlotToConcept(ConceptSchema schema, String slotName, String schemaName, SlotAccessData sad, boolean skipClassChecking) throws OntologyException {
		if (logger.isLoggable(Logger.FINE)) {
			logger.log(Logger.FINE, "concept "+schemaName+": adding slot "+slotName);
		}
		try {
			if (!sad.aggregate) {
				TermSchema ts = supplySchemaForClassFlat(sad.type, skipClassChecking);
				schema.add(slotName, ts, sad.mandatory ? ObjectSchema.MANDATORY : ObjectSchema.OPTIONAL);
			} else {
				TermSchema ats = null;
				if (sad.aggregateClass != null) {
					// try to get a schema for the contained type
					ats = supplySchemaForClassFlat(sad.aggregateClass, skipClassChecking);
				}
				if (ats == null) {
					schema.add(slotName, tryToGetAggregateSchema(sad.type), sad.cardMin > 0 ? ObjectSchema.MANDATORY : ObjectSchema.OPTIONAL);
				} else {
					schema.add(slotName, ats, sad.cardMin, sad.cardMax, getAggregateSchemaName(sad.type));
				}
			}
		} catch (BeanOntologyException bobe) {
			throw new BeanOntologyException("error adding slot "+slotName+" to schema "+schemaName, bobe);
		}
	}

	private void addTermSlotToPredicate(PredicateSchema schema, String slotName, String schemaName, SlotAccessData sad, boolean skipClassChecking) throws OntologyException, BeanOntologyException {
		if (logger.isLoggable(Logger.FINE)) {
			logger.log(Logger.FINE, "predicate "+schemaName+": adding slot "+slotName);
		}
		try {
			if (!sad.aggregate) {
				TermSchema ts = supplySchemaForClassFlat(sad.type, skipClassChecking);
				schema.add(slotName, ts, sad.mandatory ? ObjectSchema.MANDATORY : ObjectSchema.OPTIONAL);
			} else {
				TermSchema ats = null;
				if (sad.aggregateClass != null) {
					// try to get a schema for the contained type
					ats = supplySchemaForClassFlat(sad.aggregateClass, skipClassChecking);
				}
				if (ats == null) {
					schema.add(slotName, tryToGetAggregateSchema(sad.type), sad.cardMin > 0 ? ObjectSchema.MANDATORY : ObjectSchema.OPTIONAL);
				} else {
					schema.add(slotName, ats, sad.cardMin, sad.cardMax, getAggregateSchemaName(sad.type));
				}
			}
		} catch (BeanOntologyException bobe) {
			throw new BeanOntologyException("error adding slot "+slotName+" to predicate "+schemaName, bobe);
		}
	}

	private ObjectSchema doAddFlatSchema(Class clazz, boolean skipClassChecking) throws BeanOntologyException {
		Map<SlotKey, SlotAccessData> slotAccessorsMap = buildAccessorsMap(clazz, clazz.getMethods());
		String slotName = null;
		SlotAccessData sad;

		String schemaName = getSchemaNameFromClass(clazz);
		if (logger.isLoggable(Logger.FINE)) {
			logger.log(Logger.FINE, "building concept "+schemaName);
		}
		ObjectSchema schema;
		boolean isAction = AgentAction.class.isAssignableFrom(clazz); 
		if (isAction) {
			schema = new AgentActionSchema(schemaName);
		} else {
			if (Predicate.class.isAssignableFrom(clazz)) {
				schema = new PredicateSchema(schemaName);
			} else {
				schema = new ConceptSchema(schemaName);
			}
		}

		try {
			ontology.add(schema, clazz);
		} catch (OntologyException oe) {
			throw new BeanOntologyException("error adding empty schema for class "+clazz);
		}

		try {
			for (Entry<SlotKey, SlotAccessData> entry: slotAccessorsMap.entrySet()) {
				slotName = entry.getKey().slotName;
				sad = entry.getValue();
				if (schema instanceof ConceptSchema) {
					addTermSlotToConcept((ConceptSchema)schema, slotName, schemaName, sad, skipClassChecking);
				} else {
					addTermSlotToPredicate((PredicateSchema)schema, slotName, schemaName, sad, skipClassChecking);
				}
			}
			introspector.addAccessors(slotAccessorsMap);
			if (isAction) {
				Annotation annotation;
				if ((annotation = clazz.getAnnotation(Result.class)) != null) {
					TermSchema ts = supplySchemaForClassFlat(((Result)annotation).type(), skipClassChecking);
					((AgentActionSchema)schema).setResult(ts);
				} else if ((annotation = clazz.getAnnotation(AggregateResult.class)) != null) {
					AggregateResult ar = (AggregateResult)annotation;
					TermSchema ts = supplySchemaForClassFlat(ar.type(), skipClassChecking);
					((AgentActionSchema)schema).setResult(ts, ar.cardMin(), ar.cardMax());
				}
			}
		} catch (OntologyException e) {
			throw new BeanOntologyException("error getting schema for slot "+slotName);
		}
		return schema;
	}

	private ObjectSchema doAddHierarchicalSchema(Class clazz, boolean skipClassChecking) throws BeanOntologyException {
		Class superClazz = clazz.getSuperclass();
		ObjectSchema superSchema = null;
		if (superClazz != null) {
			if ((skipClassChecking && !Object.class.equals(superClazz)) || Concept.class.isAssignableFrom(superClazz)) {
				int scms = superClazz.getModifiers();
				if (!Modifier.isAbstract(scms) && !Modifier.isInterface(scms) && !Modifier.isPrivate(scms)) {
					doAddHierarchicalSchema(superClazz, skipClassChecking);
					try {
						superSchema = ontology.getSchema(superClazz);
					} catch (OntologyException oe) {
						throw new BeanOntologyException("error getting schema for superclass "+superClazz);
					}
				}
			}
		}

		Method[] declaredMethods = clazz.getDeclaredMethods();
		List<Method> publicDeclaredMethodsList = new ArrayList<Method>();
		int modifiers;
		for (Method m: declaredMethods) {
			modifiers = m.getModifiers();
			if (Modifier.isPublic(modifiers) && !Modifier.isStatic(modifiers) && !Modifier.isAbstract(modifiers)) {
				publicDeclaredMethodsList.add(m);
			}
		}

		Method[] publicDeclaredMethods = new Method[publicDeclaredMethodsList.size()];
		for (int i = 0; i < publicDeclaredMethods.length; i++) {
			publicDeclaredMethods[i] = publicDeclaredMethodsList.get(i);
		}
		Map<SlotKey, SlotAccessData> slotAccessorsMap = buildAccessorsMap(clazz, publicDeclaredMethods);
		String slotName = null;
		SlotAccessData sad;

		String schemaName = getSchemaNameFromClass(clazz);
		if (logger.isLoggable(Logger.FINE)) {
			logger.log(Logger.FINE, "building concept "+schemaName);
		}
		ObjectSchema schema;
		boolean isAction = AgentAction.class.isAssignableFrom(clazz); 
		if (isAction) {
			schema = new AgentActionSchema(schemaName);
		} else {
			if (Predicate.class.isAssignableFrom(clazz)) {
				schema = new PredicateSchema(schemaName);
			} else {
				schema = new ConceptSchema(schemaName);
			}
		}
		if (superSchema != null) {
			if (logger.isLoggable(Logger.FINE)) {
				logger.log(Logger.FINE, "adding superschema from class "+superClazz);
			}
			if (schema instanceof ConceptSchema) {
				((ConceptSchema)schema).addSuperSchema((ConceptSchema)superSchema);
			} else {
				((PredicateSchema)schema).addSuperSchema((PredicateSchema)superSchema);
			}
		}

		try {
			ontology.add(schema, clazz);
		} catch (OntologyException oe) {
			throw new BeanOntologyException("error adding empty schema for class "+clazz);
		}

		try {
			for (Entry<SlotKey, SlotAccessData> entry: slotAccessorsMap.entrySet()) {
				slotName = entry.getKey().slotName;
				sad = entry.getValue();
				if (schema instanceof ConceptSchema) {
					addTermSlotToConcept((ConceptSchema)schema, slotName, schemaName, sad, skipClassChecking);
				} else {
					addTermSlotToPredicate((PredicateSchema)schema, slotName, schemaName, sad, skipClassChecking);
				}
			}
			introspector.addAccessors(slotAccessorsMap);
			if (isAction) {
				Annotation annotation;
				if ((annotation = clazz.getAnnotation(Result.class)) != null) {
					TermSchema ts = supplySchemaForClassRecursive(((Result)annotation).type(), skipClassChecking);
					((AgentActionSchema)schema).setResult(ts);
				} else if ((annotation = clazz.getAnnotation(AggregateResult.class)) != null) {
					AggregateResult ar = (AggregateResult)annotation;
					TermSchema ts = supplySchemaForClassRecursive(ar.type(), skipClassChecking);
					((AgentActionSchema)schema).setResult(ts, ar.cardMin(), ar.cardMax());
				}
			}
		} catch (OntologyException e) {
			throw new BeanOntologyException("error getting schema for slot "+slotName, e);
		}
		return schema;
	}

	private void doAddSchema(Class clazz, boolean buildHierarchy, boolean skipClassChecking) throws BeanOntologyException {
		boolean classIsValid = skipClassChecking || (Concept.class.isAssignableFrom(clazz) || Predicate.class.isAssignableFrom(clazz));

		if (classIsValid) {
			ObjectSchema schema = null;
			try {
				schema = BasicOntology.getInstance().getSchema(clazz);
			} catch (OntologyException oe) {
				// this should never happen
				oe.printStackTrace();
			}
			if (schema != null) {
				throw new BeanOntologyException("cannot add schema for class "+clazz+" since it is included in BasicOntology");
			}
			if (buildHierarchy) {
				doAddHierarchicalSchema(clazz, skipClassChecking);
			} else {
				doAddFlatSchema(clazz, skipClassChecking);
			}
		}
	}

	void addSchema(Class clazz, boolean buildHierarchy) throws BeanOntologyException {
		doAddSchema(clazz, buildHierarchy, true);
	}

	void addSchemas(String pkgname, boolean buildHierarchy) throws BeanOntologyException {
		try {
			List<Class> classesForPackage = ClassDiscover.getClassesForPackage(pkgname);
			if (classesForPackage.size() < 1) {
				throw new BeanOntologyException("no suitable classes found");
			}
			for (Class clazz: classesForPackage) {
				doAddSchema(clazz, buildHierarchy, false);
			}
		} catch (ClassNotFoundException cnfe) {
			throw new BeanOntologyException("Class not found", cnfe);
		}
	}
}
