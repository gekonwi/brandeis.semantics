package semantics.hu_konwisser.ps5;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import semantics.hu_konwisser.ps5.bvl.BVLReader;
import semantics.hu_konwisser.ps5.bvl.BVLVerb;

public class ConjugatoinsFunctionMain {
	public static void main(String[] args) throws IOException {
		Path input = Paths.get("static", "brandeis_verb_lexikon-cleaned.txt");
		Path ouput = Paths.get("generated", "Conjugations.hs");

		List<BVLVerb> verbs = BVLReader.readVerbs(input);

		ConjugationsFunctionWriter writer = new ConjugationsFunctionWriter();
		writer.write(ouput, verbs);
	}
}
