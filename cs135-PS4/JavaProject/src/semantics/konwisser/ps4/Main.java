package semantics.konwisser.ps4;

import simplenlg.features.Feature;
import simplenlg.features.NumberAgreement;
import simplenlg.features.Person;
import simplenlg.features.Tense;
import simplenlg.framework.NLGFactory;
import simplenlg.lexicon.Lexicon;
import simplenlg.phrasespec.SPhraseSpec;
import simplenlg.realiser.english.Realiser;

public class Main {

	/**
	 * Allows better readable function calls.
	 * 
	 * @author Shlomo Georg Konwisser, gekonwi@brandeis.edu
	 * 
	 */
	private enum Perfect {
		TRUE(true), FALSE(false);

		private final boolean value;

		private Perfect(boolean value) {
			this.value = value;
		}

		public boolean getValue() {
			return value;
		}
	}

	private static NLGFactory nlgFactory;
	private static Realiser realiser;

	public static void main(String[] args) {
		init();

		String verb = "say";

		conjugate(verb, Tense.PRESENT, Perfect.FALSE, Person.THIRD);
		conjugate(verb, Tense.PRESENT, Perfect.FALSE, Person.FIRST);

		conjugate(verb, Tense.PAST, Perfect.FALSE, Person.FIRST);

		conjugate(verb, Tense.PRESENT, Perfect.TRUE, Person.FIRST);
		conjugate(verb, Tense.PAST, Perfect.TRUE, Person.FIRST);

		conjugate(verb, Tense.FUTURE, Perfect.FALSE, Person.FIRST);
		conjugate(verb, Tense.FUTURE, Perfect.TRUE, Person.FIRST);
	}

	private static void init() {
		Lexicon lexicon = Lexicon.getDefaultLexicon();
		nlgFactory = new NLGFactory(lexicon);
		realiser = new Realiser(lexicon);
	}

	private static void conjugate(String verb, Tense tense, Perfect perfect,
			Person person) {
		// NLGElement verbElement = nlgFactory.createWord(verb,
		// LexicalCategory.VERB);
		// verbElement.setFeature(Feature.TENSE, tense);
		// verbElement.setFeature(Feature.PERFECT, perfectForm);
		// verbElement.setFeature(Feature.PERSON, person);

		SPhraseSpec p = nlgFactory.createClause();
		// p.setSubject("I");
		p.setVerb(verb);
		p.setFeature(Feature.TENSE, tense);
		p.setFeature(Feature.PERFECT, perfect.getValue());
		p.setFeature(Feature.PERSON, person);
		p.setFeature(Feature.NUMBER, NumberAgreement.SINGULAR);

		String output = realiser.realiseSentence(p);

		// NLGElement realized = realiser.realise(verbElement);
		// String output = realized.getRealisation();

		output = output.toLowerCase();

		// remove the dot at the end
		output = output.substring(0, output.length() - 1);

		System.out.println(output);
	}

}
