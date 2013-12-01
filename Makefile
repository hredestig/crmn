pkg = "crmn"
local_library := $(shell Rscript -e "cat(.libPaths()[1])")

all: clean build check install

install:
	R CMD INSTALL pkg

check:
	R CMD check $(pkg)*.tar.gz

build:
	Rscript -e "library(roxygen2);roxygenize('pkg');"
	R CMD build pkg --resave-data

clean:
	rm -rf $(local_library)/00LOCK-pkg
	rm -rf pkg/src/*.{so,o,rds}
	rm -f $(pkg)*.tar.gz
	rm -rf *.Rcheck
	find -name .Rhistory -exec rm {} \; 
	find -name "*~" -exec rm {} \; 

checkpkg: clean
	R CMD check pkg
