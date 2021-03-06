PACKAGE := $(shell grep '^Package:' DESCRIPTION | sed -E 's/^Package:[[:space:]]+//')
RSCRIPT = Rscript --no-init-file

all: install

test:
	${RSCRIPT} -e 'library(methods); devtools::test()'

test_all:
	REMAKE_TEST_INSTALL_PACKAGES=true make test

doc:
	@mkdir -p man
	${RSCRIPT} -e "library(methods); devtools::document()"

install: doc build
	R CMD INSTALL . && rm *.tar.gz

build:
	R CMD build .

check: build
	_R_CHECK_CRAN_INCOMING_=FALSE R CMD check --as-cran --no-manual `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -f `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -rf ${PACKAGE}.Rcheck

check_all:
	REMAKE_TEST_INSTALL_PACKAGES=true make check

check_windows:
	${RSCRIPT} -e "devtools::check_win_devel(quiet=TRUE); devtools::check_win_release(quiet=TRUE)"

readme:
	${RSCRIPT} -e "knitr::knit('README.Rmd')"
		
# No real targets!
.PHONY: all test doc install
