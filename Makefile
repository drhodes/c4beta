# clever help rule for auto docs swiped from here.

# http://marmelab.com/blog/2016/02/29/auto-documented-makefile.html
.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk \
	'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

FORCE:

build: ## build
	stack build

test: FORCE ## test
	#stack exec c4beta-exe ./xv6-src/xv6-master/types.h
	#stack exec c4beta-exe ./test/test-cases/if1.c
	#stack exec c4beta-exe ./test/test-cases/if2.c
	#stack exec c4beta-exe ./test/test-cases/square1.c
	#	stack exec c4beta-exe ./test/test-cases/dosquare1.c
	stack exec c4beta-exe ./test/test-cases/first.c

clean: ## clean all the things
	sh clean.sh

work: ## open all files in editor
	emacs -nw Makefile src/*.hs *cabal

get_bsim: ## clone the bsim emulator
	git clone https://github.com/6004x/6.004_tools bsim

add: clean ## add files to the git repo
	git add -A :/

commit: ## git commit -a
	git commit -a

check_for_programs:  ## check to see if all the programs needed are installed
	git --version
	stack --version
	node --version # for testing.
	grunt --version
	@echo
	@echo "Looks ok.  nb: haven't checked versions"

