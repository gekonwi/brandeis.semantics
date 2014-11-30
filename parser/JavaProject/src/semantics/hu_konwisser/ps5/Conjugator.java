package semantics.hu_konwisser.ps5;

import java.util.ArrayList;
import java.util.List;

import simplenlg.features.Feature;
import simplenlg.features.NumberAgreement;
import simplenlg.features.Person;
import simplenlg.features.Tense;
import simplenlg.framework.NLGFactory;
import simplenlg.lexicon.Lexicon;
import simplenlg.phrasespec.SPhraseSpec;
import simplenlg.realiser.english.Realiser;

public class Conjugator {

	/**
	 * Allows better readable function calls.
	 * 
	 * @author Shlomo Georg Konwisser, gekonwi@brandeis.edu
	 * 
	 */
	public enum Perfect {
		TRUE(true), FALSE(false);

		private final boolean value;

		private Perfect(boolean value) {
			this.value = value;
		}

		public boolean getValue() {
			return value;
		}
	}

	private final NLGFactory nlgFactory;
	private final Realiser realiser;

	public Conjugator() {
		Lexicon lexicon = Lexicon.getDefaultLexicon();
		nlgFactory = new NLGFactory(lexicon);
		realiser = new Realiser(lexicon);
	}

	public static void main(String[] args) {
		String verb = "say";

		Conjugator conj = new Conjugator();

		conj.conjugate(verb, Tense.PRESENT, Perfect.FALSE, Person.FIRST);
		conj.conjugate(verb, Tense.PRESENT, Perfect.FALSE, Person.THIRD);

		conj.conjugate(verb, Tense.PAST, Perfect.FALSE, Person.FIRST);

		conj.conjugate(verb, Tense.PRESENT, Perfect.TRUE, Person.FIRST);
		conj.conjugate(verb, Tense.PAST, Perfect.TRUE, Person.FIRST);

		conj.conjugate(verb, Tense.FUTURE, Perfect.FALSE, Person.FIRST);
		conj.conjugate(verb, Tense.FUTURE, Perfect.TRUE, Person.FIRST);
	}

	/**
	 * Assumes plurality = Singular.
	 * 
	 * @param verb
	 * @param tense
	 * @param perfect
	 * @param person
	 * @return
	 */
	public String conjugate(String verb, Tense tense, Perfect perfect,
			Person person) {

		SPhraseSpec p = nlgFactory.createClause();
		p.setVerb(verb);
		p.setFeature(Feature.TENSE, tense);
		p.setFeature(Feature.PERFECT, perfect.getValue());
		p.setFeature(Feature.PERSON, person);
		p.setFeature(Feature.NUMBER, NumberAgreement.SINGULAR);

		String output = realiser.realiseSentence(p);

		output = output.toLowerCase();

		// remove the dot at the end
		output = output.substring(0, output.length() - 1);

		// System.out.println(output);
		return output;
	}

	/**
	 * 
	 * @param verb
	 *            verb root (infinitive without the "to")
	 * @return simple past, present perfect non-third person, present perfect
	 *         third person, present non-third person, present third person,
	 *         future
	 */
	public List<String> getConjugations(String verb) {
		List<String> conjugations = new ArrayList<>();

		conjugations.add(conjugate(verb, Tense.PAST, Perfect.FALSE,
				Person.FIRST));

		conjugations.add(conjugate(verb, Tense.PRESENT, Perfect.TRUE,
				Person.FIRST));

		conjugations.add(conjugate(verb, Tense.PRESENT, Perfect.TRUE,
				Person.THIRD));

		conjugations.add(verb);

		conjugations.add(conjugate(verb, Tense.PRESENT, Perfect.FALSE,
				Person.THIRD));

		conjugations.add(conjugate(verb, Tense.FUTURE, Perfect.FALSE,
				Person.FIRST));

		return conjugations;
	}
}
