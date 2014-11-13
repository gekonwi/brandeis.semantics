package semantics.konwisser.ps4;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import semantics.konwisser.ps4.BrandeisLexikonReader.Verb;

public class Main {
	public static void main(String[] args) throws IOException {
		Path input = Paths.get("brandeis_verb_lexikon-cleaned.txt");
		Path ouput = Paths.get("haskell_generated.hs");

		List<Verb> verbs = BrandeisLexikonReader.readVerbs(input);

		HaskellLexikonWriter writer = new HaskellLexikonWriter();
		writer.write(ouput, verbs);
	}
}
