package semantics.hu_konwisser.ps5;

import static java.nio.file.StandardCopyOption.REPLACE_EXISTING;
import static java.nio.file.StandardOpenOption.APPEND;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import semantics.hu_konwisser.ps5.bvl.BVLVerb;
import semantics.konwisser.ps4.HaskellLexiconWriter;

/**
 * This class generates the Haskell Conjugations module containing the
 * <code>conjugations :: String -> [String]</code> function for PS5.
 * 
 * @author Shlomo Georg Konwisser, gekonwi@brandeis.edu
 * 
 */
public class ConjugationsFunctionWriter {

	private static final Path CONJUGATIONS_HEAD_PATH = Paths.get("static",
			"conjugations_head.hs");

	private final Conjugator conj = new Conjugator();

	public void write(Path ouput, List<BVLVerb> verbs) throws IOException {
		System.out.println("ConjugationsFunctionWriter: Strarting writing "
				+ ouput);

		Files.copy(CONJUGATIONS_HEAD_PATH, ouput, REPLACE_EXISTING);

		BufferedWriter bw = Files.newBufferedWriter(ouput,
				Charset.forName("UTF-8"), APPEND);

		for (BVLVerb verb : verbs) {
			if (!HaskellLexiconWriter.isRelevant(verb))
				continue;

			bw.write(getFunctionEntries(verb.getVerb()));
		}

		bw.flush();
		bw.close();

		System.out.println("ConjugationsFunctionWriter: done");
	}

	private String getFunctionEntries(String verb) {
		List<String> conjugations = conj.getConjugations(verb);

		StringBuilder sb = new StringBuilder();
		sb.append(String.format("\nconjugations \"%s\" = \"%s\"", verb,
				conjugations));

		return sb.toString();
	}
}
