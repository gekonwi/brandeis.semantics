package semantics.konwisser.ps4;

public class PrepositionEntryGenerator {

	public static void main(String[] args) {
		String[] prepositions = { "about", "at", "by", "for", "from", "in",
				"into", "of", "on", "to", "with", "against", "over", "through",
				"onto", "upon", "like" };

		for (String prep : prepositions) {
			char first = Character.toUpperCase(prep.charAt(0));
			String prepUpper = first + prep.substring(1);

			String line = "lexicon \"" + prep + "\" = [Cat \"" + prep
					+ "\" \"PREP\" [" + prepUpper + "] []]";
			System.out.println(line);
		}
	}

}
