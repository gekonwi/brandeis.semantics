package semantics.konwisser.ps4;

import java.util.ArrayList;
import java.util.List;

public class RulesFactory {
	public static TransformationRule DO() {
		String pattern = "DO";
		String output = "Cat \"<verb>\" \"VP\" [Infl] "
				+ "[Cat \"_\" \"NP\" [AccOrDat] []]";
		return getSimmpleContentRule(pattern, output);
	}

	public static TransformationRule IO_DO() {
		String pattern = "IO-DO";
		String output = "Cat \"<verb>\" \"VP\" [Infl] "
				+ "[Cat \"_\" \"NP\" [AccOrDat] [], Cat \"_\" \"NP\" [AccOrDat] []]";
		return getSimmpleContentRule(pattern, output);
	}

	public static TransformationRule DO_TONP() {
		String pattern = "DO-TONP";
		String output = "Cat \"<verb>\" \"VP\" [Infl]  [Cat \"_\" \"NP\" [AccOrDat] [], Cat \"_\" \"PP\" [To] []]";
		return getSimmpleContentRule(pattern, output);
	}

	public static List<TransformationRule> P_prep_NP() {
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

		return getSimmpleContentRule(pattern, sb.toString());
	}

	private static String getPrepositionRuleOutput(String preposition) {
		char first = Character.toUpperCase(preposition.charAt(0));
		String prepUpper = first + preposition.substring(1);

		String output = "Cat \"<verb>\" \"VP\" [Infl]  "
				+ "[Cat \"_\" \"NP\" [AccOrDat] [], Cat \"_\" \"PP\" ["
				+ prepUpper + "] []]";

		return output;
	}

	private static List<TransformationRule> getRegularPrepositionRules() {
		String[] prepositions = { "about", "at", "by", "for", "from", "in",
				"into", "of", "on", "with", "against", "over", "through",
				"onto", "upon", "like" };

		List<TransformationRule> rules = new ArrayList<>();
		for (String prep : prepositions) {
			String pattern = "P_" + prep + "-NP";

			String output = getPrepositionRuleOutput(prep);

			rules.add(getSimmpleContentRule(pattern, output));
		}

		return rules;
	}

	private static TransformationRule getSimmpleContentRule(String pattern,
			String outputWithPlaceholders) {

		SimpleRuleContent content = new SimpleRuleContent(pattern,
				outputWithPlaceholders);
		return new TransformationRule(content);
	}
}
