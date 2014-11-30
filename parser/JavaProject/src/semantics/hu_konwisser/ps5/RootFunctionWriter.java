package semantics.hu_konwisser.ps5;

import static java.nio.file.StandardCopyOption.REPLACE_EXISTING;
import static java.nio.file.StandardOpenOption.APPEND;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import semantics.hu_konwisser.ps5.bvl.BVLRules;
import semantics.hu_konwisser.ps5.bvl.BVLRules.NoTransformationRuleFoundException;
import semantics.hu_konwisser.ps5.bvl.BVLVerb;
import semantics.konwisser.ps4.Conjugator;
import semantics.konwisser.ps4.Conjugator.Perfect;
import simplenlg.features.Person;
import simplenlg.features.Tense;

/**
 * This class generates the Haskell Prop module containing proposition helper
 * functions for PS5.
 * 
 * @author Shlomo Georg Konwisser, gekonwi@brandeis.edu
 * 
 */
public class RootFunctionWriter {

	private static final Path ROOT_HEAD_PATH = Paths.get("static",
			"root_head.hs");

	private final Conjugator conj = new Conjugator();

	public void write(Path ouput, List<BVLVerb> verbs) throws IOException {
		System.out.println("RootFunctionWriter: Strarting writing " + ouput);

		Files.copy(ROOT_HEAD_PATH, ouput, REPLACE_EXISTING);

		BufferedWriter bw = Files.newBufferedWriter(ouput,
				Charset.forName("UTF-8"), APPEND);

		for (BVLVerb verb : verbs) {
			if (!isRelevant(verb))
				continue;

			bw.write("\n");
			bw.write(getRootFunctionEntries(verb.getVerb()));
		}

		bw.flush();
		bw.close();

		System.out.println("RootFunctionWriter: done");
	}

	private boolean isRelevant(BVLVerb verb) {
		// we ignore those
		if (verb.getVerb().contains("-"))
			return false;

		try {
			BVLRules.getApplicableRules(verb.getCodes());
		} catch (NoTransformationRuleFoundException e) {
			/*
			 * this means there are no entries for it in Lexicon.hs, so we don't
			 * have to care about it here as well
			 */
			return false;
		}

		return true;
	}

	private String getRootFunctionEntries(String verb) {
		List<String> conjugations = getConjugations(verb);

		StringBuilder sb = new StringBuilder();
		for (String c : conjugations) {
			c = c.replaceAll(" ", "_");
			sb.append(String.format("\nroot \"%s\" = \"%s\"", c, verb));
		}

		return sb.toString();
	}

	private List<String> getConjugations(String verb) {
		List<String> conjugations = new ArrayList<>();

		conjugations.add(conj.conjugate(verb, Tense.PAST, Perfect.FALSE,
				Person.FIRST));

		conjugations.add(conj.conjugate(verb, Tense.PRESENT, Perfect.TRUE,
				Person.FIRST));

		conjugations.add(conj.conjugate(verb, Tense.PRESENT, Perfect.TRUE,
				Person.THIRD));

		conjugations.add(verb);

		conjugations.add(conj.conjugate(verb, Tense.PRESENT, Perfect.FALSE,
				Person.THIRD));

		conjugations.add(conj.conjugate(verb, Tense.FUTURE, Perfect.FALSE,
				Person.FIRST));

		return conjugations;
	}

}
