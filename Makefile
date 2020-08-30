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
.PHONY: doc
