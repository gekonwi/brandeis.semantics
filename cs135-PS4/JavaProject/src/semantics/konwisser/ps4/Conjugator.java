package semantics.konwisser.ps4;

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

}
