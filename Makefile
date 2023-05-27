CANNETTE_BUILD=jpm_tree/lib/cannette/init.janet
CANNETTE_TEST_DEP=jpm_tree/bin/judge
CANNETTE_TEST_CMD=jpm_tree/bin/judge

all: build test

$(CANNETTE_BUILD): project.janet src/c.janet src/init.janet src/janet.janet src/lib.janet src/macros.janet src/xml.janet
	jpm install --local

$(CANNETTE_TEST_DEP):
	jpm install --local git::https://github.com/ianthehenry/judge.git::v2.5.0

build: $(CANNETTE_BUILD)

clean:
	rm -rf jpm_tree/

test: build $(CANNETTE_TEST_DEP)
	jpm -l exec $(CANNETTE_TEST_CMD) test/

.PHONY: all build clean

