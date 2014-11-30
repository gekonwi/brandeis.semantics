package semantics.hu_konwisser.ps5.bvl;

import java.util.Collections;
import java.util.Set;

public class BVLVerb {
	private final String verb;
	private final Set<String> codes;

	public BVLVerb(String verb, Set<String> codes) {
		this.verb = verb;
		this.codes = codes;
	}

	public String getVerb() {
		return verb;
	}

	public Set<String> getCodes() {
		return Collections.unmodifiableSet(codes);
	}

	@Override
	public String toString() {
		return verb + " " + codes;
	}
}
