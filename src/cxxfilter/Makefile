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

gen/cxxfilter.cpp : README.md
	notangle -L -Rcxxfilter.cpp README.md > gen/cxxfilter.cpp

gen/xparse_runtime.cpp : README.md
	notangle -L -Rxparse_runtime.cpp PARSER.md > gen/xparse_runtime.cpp

gen/xparse_runtime.h : PARSER.md
	notangle -L -Rxparse_runtime.h PARSER.md > gen/xparse_runtime.h

gen/luhncheck.xparse : README.md
	notangle -L -Rluhncheck.xparse README.md > gen/luhncheck.xparse

gen/xparse_generated.cpp : $(statemachine)
gen/xparse_generated.h : $(statemachine)

$(statemachine) : README.md $(compiler)
	notangle -Rluhncheck.xparse README.md > gen/luhncheck.xparse
	(cd gen && cat luhncheck.xparse | ./xparse )

gen/y.tab.c : gen/xparse.y
	(cd gen && $(YACC) -d -v xparse.y)

gen/lex.yy.c : gen/xparse.l
	(cd gen && $(LEX) xparse.l)

gen/xparse.y : PARSER.md gen/xparse.h
	notangle -Rxparse.y PARSER.md > gen/xparse.y

gen/xparse.l : PARSER.md
	notangle -Rxparse.l PARSER.md > gen/xparse.l.in
	cat gen/xparse.l.in | sed 's/^    //g' > gen/xparse.l

gen/xparse.h : PARSER.md
	notangle -L -Rxparse.h PARSER.md > gen/xparse.h

gen/xparse.c : PARSER.md
	notangle -L -Rxparse.c PARSER.md > gen/xparse.c

