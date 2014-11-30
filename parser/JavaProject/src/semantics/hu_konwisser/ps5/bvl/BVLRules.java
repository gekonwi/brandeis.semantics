package semantics.hu_konwisser.ps5.bvl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import semantics.konwisser.ps4.TransformationRule;

/**
 * Manages transformation rules for the Brandeis Verb Lexikon
 * 
 * @author Shlomo Georg Konwisser, gekonwi@brandeis.edu
 * 
 */
public class BVLRules {

	private static List<TransformationRule> rules;

	static {
		rules = new ArrayList<>();

		rules.add(DO());
		rules.add(IO_DO());
		rules.add(DO_TONP());
		rules.addAll(P_prep_NP());
	}

	public static Map<String, Set<TransformationRule>> getApplicableRules(
			Set<String> codes) {

		Map<String, Set<TransformationRule>> result = new HashMap<>();

		for (String code : codes) {
			Set<TransformationRule> applicables = new HashSet<>();

			for (TransformationRule rule : rules) {
				if (rule.applicable(code))
					applicables.add(rule);
			}

			if (!applicables.isEmpty())
				result.put(code, applicables);
		}

		return result;
	}

	private static TransformationRule DO() {
		String pattern = "DO";
		String output = "Cat \"<verb>\" \"VP\" [Infl] "
				+ "[Cat \"_\" \"NP\" [AccOrDat] []]";
		return new TransformationRule(pattern, output);
	}

	private static TransformationRule IO_DO() {
		String pattern = "IO-DO";
		String output = "Cat \"<verb>\" \"VP\" [Infl] "
				+ "[Cat \"_\" \"NP\" [AccOrDat] [], Cat \"_\" \"NP\" [AccOrDat] []]";
		return new TransformationRule(pattern, output);
	}

	private static TransformationRule DO_TONP() {
		String pattern = "DO-TONP";
		String output = getPrepositionRuleOutput("to");
		return new TransformationRule(pattern, output);
	}

	private static List<TransformationRule> P_prep_NP() {
		List<TransformationRule> rules = getRegularPrepositionRules();

		String[] dirPrepositions = { "to", "on", "onto", "in", "into" };
		rules.add(getPrepositionClassRule("dir", dirPrepositions));

		String[] locPrepositions = { "at", "in", "on" };
		rules.add(getPrepositionClassRule("loc", locPrepositions));

		return rules;
	}

	private static TransformationRule getPrepositionClassRule(String className,
			String[] prepositions) {
		String pattern = "P_" + className + "-NP";

		List<String> output = new ArrayList<>(prepositions.length);
		for (String prep : prepositions)
			output.add(getPrepositionRuleOutput(prep));

		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < output.size(); i++) {
			sb.append(output.get(i));
			if (i != output.size() - 1)
				sb.append(",\n");
		}

		return new TransformationRule(pattern, sb.toString());
	}

	private static String getPrepositionRuleOutput(String preposition) {
		char first = Character.toUpperCase(preposition.charAt(0));
		String prepUpper = first + preposition.substring(1);

		String output = "Cat \"<verb>\" \"VP\" [Infl] "
				+ "[Cat \"_\" \"NP\" [AccOrDat] [], Cat \"_\" \"PP\" ["
				+ prepUpper + "] []]";

		return output;
	}

	private static List<TransformationRule> getRegularPrepositionRules() {
		String[] prepositions = { "about", "at", "by", "for", "from", "in",
				"into", "of", "on", "to", "with", "against", "over", "through",
				"onto", "upon", "like" };

		List<TransformationRule> rules = new ArrayList<>();
		for (String prep : prepositions) {
			String pattern = "P_" + prep + "-NP";

			String output = getPrepositionRuleOutput(prep);

			rules.add(new TransformationRule(pattern, output));
		}

		return rules;
	}
}
