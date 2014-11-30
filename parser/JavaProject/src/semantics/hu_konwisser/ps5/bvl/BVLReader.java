package semantics.hu_konwisser.ps5.bvl;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class BVLReader {
	public static void main(String[] args) throws IOException {
		Path input = Paths.get("brandeis_verb_lexikon-cleaned.txt");

		List<BVLVerb> verbs = readVerbs(input);

		Set<String> mergedCodes = getMergedCodes(verbs);

		printCodesSorted(mergedCodes);
	}

	public static List<BVLVerb> readVerbs(Path input) throws IOException {
		Charset utf8 = Charset.forName("UTF-8");
		BufferedReader br = Files.newBufferedReader(input, utf8);

		List<BVLVerb> verbs = new ArrayList<>();

		String line;
		while ((line = br.readLine()) != null) {
			BVLVerb verb = parseVerb(line);
			verbs.add(verb);
		}

		return verbs;
	}

	private static BVLVerb parseVerb(String line) {
		String[] parts = line.split("\\s+");

		String verbString = parts[0].toLowerCase();

		Set<String> codes = new HashSet<>();
		for (int i = 1; i < parts.length; i++)
			codes.add(parts[i]);

		BVLVerb verb = new BVLVerb(verbString, codes);
		return verb;
	}

	private static Set<String> getMergedCodes(List<BVLVerb> verbs) {
		Set<String> mergedCodes = new HashSet<>();

		for (BVLVerb verb : verbs) {
			for (String code : verb.getCodes()) {
				code = code.replaceAll("P_[a-z]+", "P_<preposition>");
				mergedCodes.add(code);
			}
		}

		return mergedCodes;
	}

	private static void printCodesSorted(Set<String> subCatCodes) {
		System.out.println(subCatCodes.size() + " subcategory codes found:\n");

		List<String> codesList = new ArrayList<>(subCatCodes);
		Collections.sort(codesList);

		for (String code : codesList)
			System.out.println(code);
	}

}
