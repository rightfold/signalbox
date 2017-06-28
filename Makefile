F95=gfortran
F95FLAGS=-std=f2008 -Wall -Wextra -Wpedantic -fbounds-check -fno-underscoring

CXX=clang
CXXFLAGS=-std=c++14 -Wall -Wextra -Wpedantic -D_GLIBCXX_DEBUG -Ivendor

LD=gcc
LDFLAGS=-lgfortran -lstdc++ -lboost_system -lboost_filesystem -lzmq

F95_SOURCES=$(shell find src -name '*.f95')
F95_OBJECTS=$(patsubst src/%.f95,build/src/%.f95.o,${F95_SOURCES})

CXX_SOURCES=$(shell find src -name '*.cpp' -not -path src/signalboxd.cpp)
CXX_HEADERS=$(shell find src -name '*.hpp')
CXX_OBJECTS=$(patsubst src/%.cpp,build/src/%.cpp.o,${CXX_SOURCES})

CXX_TEST_SOURCES=$(shell find test -name '*.cpp' -not -path test/signalboxt.cpp)
CXX_TEST_HEADERS=$(shell find test -name '*.hpp')
CXX_TEST_OBJECTS=$(patsubst test/%.cpp,build/test/%.cpp.o,${CXX_TEST_SOURCES})

all: build/signalboxd

.PHONY: test
test: build/signalboxt
	build/signalboxt

.PHONY: clean
clean:
	rm -rf build

build/signalboxd: build/src/signalboxd.cpp.o ${CXX_OBJECTS} ${F95_OBJECTS}
	mkdir -p $(dir $@)
	${LD} ${LDFLAGS} $^ -o $@

build/signalboxt: build/test/signalboxt.cpp.o ${CXX_OBJECTS} ${F95_OBJECTS} ${CXX_TEST_OBJECTS}
	mkdir -p $(dir $@)
	${LD} ${LDFLAGS} $^ -o $@

build/%.f95.o: %.f95
	mkdir -p $(dir $@)
	${F95} ${F95FLAGS} -c $< -o $@

build/%.cpp.o: %.cpp ${CXX_HEADERS}
	mkdir -p $(dir $@)
	${CXX} ${CXXFLAGS} -c $< -o $@
