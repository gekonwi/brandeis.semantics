package semantics.konwisser.ps4;

import java.util.regex.Pattern;

public final class TransformationRule {

	private final Pattern codePattern;
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

	public boolean applicable(String code) {
		return codePattern.matcher(code).matches();
	}

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

	public TransformationRule(String codePattern, String outputWithPlaceholders) {
		this.outputWithPlaceholders = outputWithPlaceholders;
		this.codePattern = Pattern.compile(codePattern);
	}

	public String apply(String verb, String code) {
		if (!applicable(code))
			throw new IllegalArgumentException(
					"This rule is not applicable to the code ["
							+ code
							+ "]. Avoid this exception by using applicable(...) "
							+ "before calling apply(...)");

		return outputWithPlaceholders.replaceAll(VERB_PLACEHOLDER, verb);
	}
}