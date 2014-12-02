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
import semantics.hu_konwisser.ps5.bvl.LexiconWriter;

/**
 * This class generates the Haskell VerbRoot module containing the root function
 * for PS5.
 * 
 * @author Shlomo Georg Konwisser, gekonwi@brandeis.edu
 * 
 */
public class RootFunctionWriter {

	private static final Path ROOT_HEAD_PATH = Paths.get("static",
			"root_head.hs");

	private final Conjugator conj = new Conjugator();

	public void write(Path output, List<BVLVerb> verbs) throws IOException {
		System.out.println("RootFunctionWriter: Strarting writing " + output);

		Files.copy(ROOT_HEAD_PATH, output, REPLACE_EXISTING);

		BufferedWriter bw = Files.newBufferedWriter(output,
				Charset.forName("UTF-8"), APPEND);

		for (BVLVerb verb : verbs) {
			if (!LexiconWriter.isRelevant(verb))
				continue;

			bw.write("\n");
			bw.write(getRootFunctionEntries(verb.getVerb()));
		}

		bw.write("\n\n");
		bw.write("root x = error $ \"root for [\" ++ (show x) ++ \"] is not defined in "
				+ output.getFileName() + ". Maybe it is just commented out?\"");

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
