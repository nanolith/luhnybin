.PHONY: ALL

ALL : cxxfilter

sources = gen/cxxfilter.cpp
cxxfilter : $(sources:.cpp=.o) $(headers:.h=.o)

gen :
	mkdir gen

gen/cxxfilter.cpp : README.md
	notangle -Rcxxfilter.cpp README.md > gen/cxxfilter.cpp
