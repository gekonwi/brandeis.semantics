package semantics.konwisser.ps4;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;

public class BrandeisLexikonReader {
	public static void main(String[] args) throws IOException {
		Charset utf8 = Charset.forName("UTF-8");
		Path input = Paths.get("chosen_verbs_120");
		BufferedReader br = Files.newBufferedReader(input, utf8);

		HashSet<String> subCatCodes = readCodes(br);

		printCodes(subCatCodes);
	}

	private static HashSet<String> readCodes(BufferedReader br)
			throws IOException {
		HashSet<String> subCatCodes = new HashSet<>();
		String line;
		while ((line = br.readLine()) != null) {
			String[] parts = line.split("\\s+");
			System.out.println(Arrays.toString(parts));

			String verb = parts[0].toLowerCase();
			for (int i = 1; i < parts.length; i++)
				subCatCodes.add(parts[i]);
		}
		return subCatCodes;
	}

	private static void printCodes(HashSet<String> subCatCodes) {
		System.out.println("\n\n\n" + subCatCodes.size()
				+ " subcategory codes found:\n");
		List<String> codesList = new ArrayList<>(subCatCodes);
		Collections.sort(codesList);
		for (String code : codesList)
			System.out.println(code);
	}

}
