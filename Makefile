build:
	dune build
.PHONY: build

watch:
	dune build -w @default @doc @runtest
.PHONY: watch

clean:
	dune clean
.PHONY: clean

test:
	dune test
.PHONY: test

doc:
	dune build @doc
	cp -r _build/default/_doc/_html docs
.PHONY: doc
