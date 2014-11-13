package semantics.konwisser.ps4;

import semantics.konwisser.ps4.TransformationRule.Content;

// TODO find better name
public class SimpleRuleContent implements Content {

	private final String codePattern;
	private final String outputWithPlaceholders;

	/**
	 * Used to mark places where the verb should be inserted. E.g.
	 * 
	 * <pre>
	 * {@code
	 * "Cat \"<verb>\" \"VP\" [Infl] [Cat \"_\" \"NP\" [AccOrDat] []]"
	 * }
	 * </pre>
	 * 
	 * with the verb <code>"love"</code> would result in
	 * 
	 * <pre>
	 * {@code
	 * Cat "love" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []
	 * }
	 * </pre>
	 */
	public static final String VERB_PLACEHOLDER = "<verb>";

	/**
	 * 
	 * @param codePattern
	 *            regular expression. returns <code>true</code> when matched
	 *            against the whole code to be transformed by this rule
	 * @param output
	 *            the return value of {@link #apply(String, String)} with all
	 *            occurrences of {@value #VERB_PLACEHOLDER} replaced by the
	 *            actual verb at hand.
	 */
	public SimpleRuleContent(String codePattern, String outputWithPlaceholders) {
		this.codePattern = codePattern;
		this.outputWithPlaceholders = outputWithPlaceholders;
	}

	@Override
	public String getCodePattern() {
		return codePattern;
	}

	@Override
	public String apply(String verb, String code) {
		return outputWithPlaceholders.replaceAll(VERB_PLACEHOLDER, verb);
	}

}