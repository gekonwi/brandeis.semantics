package semantics.konwisser.ps4;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Set;

import semantics.hu_konwisser.ps5.Conjugator;
import semantics.hu_konwisser.ps5.Conjugator.Perfect;
import semantics.hu_konwisser.ps5.bvl.BVLRules;
import semantics.hu_konwisser.ps5.bvl.BVLVerb;
import simplenlg.features.Person;
import simplenlg.features.Tense;

public class HaskellLexikonWriter {

	private final Conjugator conj = new Conjugator();

	public static boolean isRelevant(BVLVerb verb) {
		// we ignore those
		if (verb.getVerb().contains("-"))
			return false;

		if (BVLRules.getApplicableRules(verb.getCodes()).isEmpty()) {
			// System.out.println("No transformation rule found for: " + verb);
			return false;
		}

		return true;
	}

	public void write(Path output, List<BVLVerb> verbs) throws IOException {
		Charset utf8 = Charset.forName("UTF-8");
		BufferedWriter bw = Files.newBufferedWriter(output, utf8);

		for (BVLVerb verb : verbs) {
			if (!isRelevant(verb))
				continue;

			String haskellRep = getHaskellRepresentation(verb);
			bw.write(haskellRep);
			bw.write("\n");
		}

		bw.flush();
		bw.close();
	}

	private String getHaskellRepresentation(BVLVerb verb) {

		StringBuilder sb = new StringBuilder();
		String infinitive = verb.getVerb();
		Set<String> codes = verb.getCodes();

		sb.append("\n");
		sb.append(getLexiconEntry(infinitive, codes, "Pres,Sg,Fst",
				"Pres,Sg,Snd", "Pres,Pl", "Infl"));

		sb.append(getLexiconEntry(codes, infinitive, Tense.PRESENT,
				Perfect.FALSE, Person.THIRD, "Pres,Sg,Thrd"));

		sb.append(getLexiconEntry(codes, infinitive, Tense.PAST, Perfect.FALSE,
				Person.THIRD, "Past"));

		sb.append(getLexiconEntry(codes, infinitive, Tense.PRESENT,
				Perfect.TRUE, Person.FIRST, "Perf,Sg,Fst", "Perf,Sg,Snd",
				"Perf,Pl"));

		sb.append(getLexiconEntry(codes, infinitive, Tense.PRESENT,
				Perfect.TRUE, Person.THIRD, "Perf,Sg,Thrd"));

		sb.append(getLexiconEntry(codes, infinitive, Tense.FUTURE,
				Perfect.FALSE, Person.THIRD, "Fut"));

		return sb.toString();
	}

	private String getLexiconEntry(Set<String> codes, String infinitive,
			Tense tense, Perfect perfect, Person person, String... inflections) {

		String conjugated = conj.conjugate(infinitive, tense, perfect, person);
		conjugated = conjugated.replaceAll(" ", "_");
		return "\n" + getLexiconEntry(conjugated, codes, inflections);
	}

	private String getLexiconEntry(String verb, Set<String> codes,
			String... inflections) {
		StringBuilder sb = new StringBuilder();
		sb.append("lexicon \"" + verb + "\" = [");

		Map<String, Set<TransformationRule>> applicables;
		applicables = BVLRules.getApplicableRules(codes);

		for (String code : applicables.keySet()) {
			for (TransformationRule rule : applicables.get(code)) {
				sb.append("\n\t");

				String ruleOutput = rule.apply(verb, code);

				// TODO this is dirty
				ruleOutput = forEachInflection(inflections, ruleOutput);

				sb.append(ruleOutput);
				sb.append(",");
			}
		}

		// delete the last comma
		if (sb.length() > 0)
			sb.deleteCharAt(sb.length() - 1);

		sb.append("\n\t]");

		return sb.toString();
	}

	private String forEachInflection(String[] inflections, String ruleOutput) {

		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < inflections.length; i++) {
			String infl = inflections[i];
			sb.append(ruleOutput.replaceFirst("\\[Infl\\]", "[" + infl + "]"));
			if (i != inflections.length - 1)
				sb.append(",\n");
		}

		return sb.toString().replaceAll("\n", "\n\t");
	}
}
