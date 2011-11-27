LEX=$(BUILD_DIR)/flex/bin/flex
YACC=$(BUILD_DIR)/bison/bin/bison -y

CXXFLAGS+=-O3 -I gen
CFLAGS+=-O3 -I gen -std=c99

.PHONY: ALL clean

ALL : gen/cxxfilter

clean:
	rm -rf gen

statemachine = gen/luhncheck.xparse
sources = gen/cxxfilter.cpp gen/xparse_generated.cpp gen/xparse_runtime.cpp
headers = gen/xparse_runtime.h gen/xparse_generated.h
compiler_sources = gen/y.tab.c gen/lex.yy.c gen/xparse.c
compiler_headers = gen/xparse.h gen/y.tab.h
compiler = gen/xparse

$(compiler) : $(compiler_sources)
	$(CC) $(CFLAGS) -o $(compiler) $(compiler_sources)

gen/cxxfilter : gen $(headers) $(sources:.cpp=.o)
	$(CXX) $(CXXFLAGS) -o gen/cxxfilter $(sources:.cpp=.o)

gen :
	mkdir gen

gen/cxxfilter.cpp : gen/README.md
	notangle -L -Rcxxfilter.cpp gen/README.md > gen/cxxfilter.cpp

gen/xparse_runtime.cpp : gen/README.md
	notangle -L -Rxparse_runtime.cpp gen/PARSER.md > gen/xparse_runtime.cpp

gen/xparse_runtime.h : gen/PARSER.md
	notangle -L -Rxparse_runtime.h gen/PARSER.md > gen/xparse_runtime.h

gen/luhncheck.xparse : gen/README.md
	notangle -L -Rluhncheck.xparse gen/README.md > gen/luhncheck.xparse

gen/xparse_generated.cpp : $(statemachine)
gen/xparse_generated.h : $(statemachine)

$(statemachine) : gen/README.md $(compiler)
	notangle -Rluhncheck.xparse gen/README.md > gen/luhncheck.xparse
	(cd gen && cat luhncheck.xparse | ./xparse )

gen/y.tab.c : gen/xparse.y
	(cd gen && $(YACC) -d -v xparse.y)

gen/lex.yy.c : gen/xparse.l
	(cd gen && $(LEX) xparse.l)

gen/xparse.y : gen/PARSER.md gen/xparse.h
	notangle -Rxparse.y gen/PARSER.md > gen/xparse.y

gen/xparse.l : gen/PARSER.md
	notangle -Rxparse.l gen/PARSER.md > gen/xparse.l.in
	cat gen/xparse.l.in | sed 's/^    //g' > gen/xparse.l

gen/xparse.h : gen/PARSER.md
	notangle -L -Rxparse.h gen/PARSER.md > gen/xparse.h

gen/xparse.c : gen/PARSER.md
	notangle -L -Rxparse.c gen/PARSER.md > gen/xparse.c

gen/%.md : %.md
	cat $*.md | sed 's/^    \(<<[^>]*>>=\)/\1/g' | sed 's/^    \(@\)/\1/g' > gen/$*.md

