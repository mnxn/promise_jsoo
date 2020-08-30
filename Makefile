build:
	dune build
.PHONY: build

watch:
	dune build -w @default @doc @runtest
.PHONY: watch

clean:
	dune clean
	rm -rf ./docs
.PHONY: clean

test:
	dune test
.PHONY: test

doc:
	dune build @doc
	rm -rf ./docs
	cp -R ./_build/default/_doc/_html ./docs
.PHONY: doc
