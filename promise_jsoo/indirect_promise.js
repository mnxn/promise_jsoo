class IndirectPromise {
	constructor(promise) {
		this.underlying = promise;
	}

	static wrap(promise) {
		if (
			promise !== undefined &&
			promise !== null &&
			typeof promise.then === "function"
		) {
			return new IndirectPromise(promise);
		}
		return promise;
	}

	static unwrap(promise) {
		if (promise instanceof globalThis.IndirectPromise) {
			return promise.underlying;
		}
		return promise;
	}
}

globalThis.IndirectPromise = IndirectPromise;
