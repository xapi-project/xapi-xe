.PHONY: build
build: dist/setup
	obuild build

dist/setup: xe.obuild
	obuild configure

.PHONY: clean
clean:
	rm -rf dist
