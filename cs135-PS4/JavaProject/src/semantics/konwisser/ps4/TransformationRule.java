package semantics.konwisser.ps4;

import java.util.List;
import java.util.regex.Pattern;

public final class TransformationRule {

	public static interface Content {

		/**
		 * Results in <code>true</code> when matched against the whole code to
		 * be transformed by this rule
		 * 
		 * @return regular expression
		 */
		public String getCodePattern();

		/**
		 * 
		 * @param verb
		 * @param code
		 * @return one String per line
		 */
		public List<String> apply(String verb, String code);

	}

	private final Pattern codePattern;
	private final Content content;

	public boolean applicable(String code) {
		return codePattern.matcher(code).matches();
	}

	public TransformationRule(Content content) {
		this.content = content;
		this.codePattern = Pattern.compile(content.getCodePattern());
	}

	public List<String> apply(String verb, String code) {
		if (!applicable(code))
			throw new IllegalArgumentException(
					"This rule is not applicable to the code ["
							+ code
							+ "]. Avoid this exception by using applicable(...) "
							+ "before calling apply(...)");

		return content.apply(verb, code);
	}
}