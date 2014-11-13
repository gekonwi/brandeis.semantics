package semantics.konwisser.ps4;

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

	public static TransformationRule P_prep_NP() {
		// TODO Auto-generated method stub
		return null;
	}

	private static TransformationRule getSimmpleContentRule(String pattern,
			String outputWithPlaceholders) {

		SimpleRuleContent content = new SimpleRuleContent(pattern,
				outputWithPlaceholders);
		return new TransformationRule(content);
	}
}
