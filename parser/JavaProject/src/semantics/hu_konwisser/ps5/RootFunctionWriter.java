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
import semantics.konwisser.ps4.HaskellLexikonWriter;

/**
 * This class generates the Haskell Prop module containing proposition helper
 * functions for PS5.
 * 
 * @author Shlomo Georg Konwisser, gekonwi@brandeis.edu
 * 
 */
public class RootFunctionWriter {

	private static final Path ROOT_HEAD_PATH = Paths.get("static",
			"root_head.hs");

	private final Conjugator conj = new Conjugator();

	public void write(Path ouput, List<BVLVerb> verbs) throws IOException {
		System.out.println("RootFunctionWriter: Strarting writing " + ouput);

		Files.copy(ROOT_HEAD_PATH, ouput, REPLACE_EXISTING);

		BufferedWriter bw = Files.newBufferedWriter(ouput,
				Charset.forName("UTF-8"), APPEND);

		for (BVLVerb verb : verbs) {
			if (!HaskellLexikonWriter.isRelevant(verb))
				continue;

			bw.write("\n");
			bw.write(getRootFunctionEntries(verb.getVerb()));
		}

		bw.flush();
		bw.close();

		System.out.println("RootFunctionWriter: done");
	}

	private String getRootFunctionEntries(String verb) {
		List<String> conjugations = conj.getConjugations(verb);

		StringBuilder sb = new StringBuilder();
		for (String c : conjugations) {
			c = c.replaceAll(" ", "_");
			sb.append(String.format("\nroot \"%s\" = \"%s\"", c, verb));
		}

		return sb.toString();
	}
}
