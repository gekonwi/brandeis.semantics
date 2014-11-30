package semantics.konwisser.ps4;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Set;

import semantics.konwisser.ps4.BVLRules.NoTransformationRuleFoundException;
import semantics.konwisser.ps4.BrandeisLexikonReader.Verb;
import semantics.konwisser.ps4.Conjugator.Perfect;
import simplenlg.features.Person;
import simplenlg.features.Tense;

public class HaskellLexikonWriter {

	private final Conjugator conj = new Conjugator();

	public void write(Path output, List<Verb> verbs) throws IOException {
		Charset utf8 = Charset.forName("UTF-8");
		BufferedWriter bw = Files.newBufferedWriter(output, utf8);

		for (Verb verb : verbs) {
			// TODO handle these separately
			if (verb.getVerb().contains("-"))
				continue;

			try {
				String haskellRep = getHaskellRepresentation(verb);
				bw.write(haskellRep);
				bw.write("\n");
			} catch (NoTransformationRuleFoundException e) {
				System.out.println("No transformation rule found for: " + verb);
				continue;
			}
		}

		bw.flush();
		bw.close();
	}

	private String getHaskellRepresentation(Verb verb)
			throws NoTransformationRuleFoundException {

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
			Tense tense, Perfect perfect, Person person, String... inflections)
			throws NoTransformationRuleFoundException {

		String conjugated = conj.conjugate(infinitive, tense, perfect, person);
		conjugated = conjugated.replaceAll(" ", "_");
		return "\n" + getLexiconEntry(conjugated, codes, inflections);
	}

	private String getLexiconEntry(String verb, Set<String> codes,
			String... inflections) throws NoTransformationRuleFoundException {
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
