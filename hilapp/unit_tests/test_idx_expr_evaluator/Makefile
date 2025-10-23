CC=clang++
CFLAGS= -ggdb -std=c++17 -Wall -pedantic -Wextra -fsanitize=address

.DEFAULT_GOAL := test_idx_eval

CFLAGS += $(shell llvm-config --cflags) $(shell llvm-config --libs) $(shell llvm-config --ldflags)

DISTRO := $(shell cat /etc/*-release | grep -iE "^(ID=)" | cut -f2 -d"=")
CLANG_LIBS := \
	-Wl,--start-group \
	-lclangTooling \
	-lclangASTMatchers \
	-lclangFrontend \
	-lclangDriver \
	-lclangParse \
	-lclangSerialization \
	-lclangSema \
	-lclangEdit \
	-lclangAnalysis \
	-lclangTooling \
	-lclangToolingCore \
	-lclangRewrite \
	-lclangLex \
	-lclangBasic \
	-lclangFrontendTool \
	-lclangRewriteFrontend \
	-lclangStaticAnalyzerFrontend \
	-lclangStaticAnalyzerCheckers \
	-lclangStaticAnalyzerCore \
	-lclangAST \
	-lclangToolingSyntax

ifeq ($(DISTRO), arch)
ifeq ($(STATIC),)
	CLANG_LIBS = -lclang-cpp
endif
endif


test_idx_eval: test_idx_eval.cpp tracked_var.h
	$(CC) $(CFLAGS) $(CLANG_LIBS) test_idx_eval.cpp -o test_idx_eval 
