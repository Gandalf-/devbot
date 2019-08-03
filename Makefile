all:
	stack build

release:
	stack install --flag apocrypha:release --flag devbot:release

.PHONY: test
test:
	stack test --flag apocrypha:release --flag devbot:release --coverage
