# This Makefile attempts to follow the best-practice set out by Alexis King
# in "An opinionated guide to Haskell in 2018" where we build developer tooling
# as part of the project environment rather than globally. This ensures that
# tools like `ghcmod` are using the same version of GHC as our target runtime to
# get the most relevant results.
#
# https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/
BIN_PATH=$(shell stack path | grep bin-path | awk -F\  '{ print $$2 }' | head -n1)
export PATH := $(BIN_PATH):$(PATH)

clean:
	@stack clean

# This should only need to be done once per developer machine.
setup: clean
	stack build --no-docker --copy-compiler-tool stylish-haskell hlint apply-refact

_HLINT=hlint --refactor --refactor-options -i {} \;
hlint:
	@find {src,app}/ -name "*.hs" -exec $(_HLINT)

_STYLISH=stack --no-docker exec -- stylish-haskell -i {} \;
stylish-haskell:
	@find {src,app}/ -name "*.hs" -exec $(_STYLISH)

test:
	stack test --test-arguments "--color always"
