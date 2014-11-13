package semantics.konwisser.ps4;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import semantics.konwisser.ps4.BrandeisLexikonReader.Verb;

public class HaskellLexikonWriter {
	public class NoTransformationRuleFoundException extends Exception {

		private static final long serialVersionUID = 1L;

	}

	private final List<TransformationRule> rules = new ArrayList<>();

	public HaskellLexikonWriter() {
		rules.add(RulesFactory.DO());
		rules.add(RulesFactory.IO_DO());
		rules.add(RulesFactory.DO_TONP());
		rules.addAll(RulesFactory.P_prep_NP());
	}

	public void write(Path output, List<Verb> verbs) throws IOException {
		Charset utf8 = Charset.forName("UTF-8");
		BufferedWriter bw = Files.newBufferedWriter(output, utf8);

		for (Verb verb : verbs) {
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
		sb.append("lexicon \"" + verb.getVerb() + "\" = [");

		boolean minOneRuleFound = false;

		for (String code : verb.getCodes()) {
			for (TransformationRule rule : rules) {
				if (!rule.applicable(code))
					continue;

				minOneRuleFound = true;

				String ruleOutput = rule.apply(verb.getVerb(), code);
				sb.append("\n\t");
				sb.append(ruleOutput.replaceAll("\n", "\n\t"));
				sb.append(",");
			}
		}

		if (!minOneRuleFound)
			throw new NoTransformationRuleFoundException();

		// delete the last comma
		sb.deleteCharAt(sb.length() - 1);

		sb.append("\n]\n");

		return sb.toString();
	}
}
