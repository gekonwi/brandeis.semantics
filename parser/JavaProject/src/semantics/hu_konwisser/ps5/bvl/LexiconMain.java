package semantics.hu_konwisser.ps5.bvl;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

public class LexiconMain {
	public static void main(String[] args) throws IOException {
		Path input = Paths.get("static", "brandeis_verb_lexikon-cleaned.txt");
		Path ouput = Paths.get("generated", "Lexicon.hs");

		List<BVLVerb> verbs = BVLReader.readVerbs(input);

		LexiconWriter writer = new LexiconWriter();
		writer.write(ouput, verbs);
	}
}
