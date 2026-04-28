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


test_idx_eval: test_idx_eval.cpp tracked_var.h ../../src/idx_expr_evaluator.cpp
	$(CC) $(CFLAGS) $(CLANG_LIBS) -DTEST_IDX_EVAL test_idx_eval.cpp -o $@

idx_eval_file: test_idx_eval.cpp ../../src/idx_expr_evaluator.cpp
	$(CC) $(CFLAGS) $(CLANG_LIBS) -DIDX_EVAL_FILE test_idx_eval.cpp -o $@

# helper to run idx_eval_file with hila headers
STD_INCLUDE_DIRS := $(addprefix --extra-arg=-I, $(shell echo | g++ -xc++ --std=c++17 -Wp,-v - 2>&1 | grep "^ "))
HILA_INCLUDES = \
  --extra-arg=-I$(MY_HILA_DIR) \
  --extra-arg=-I$(MY_HILA_DIR)/libraries \
  --extra-arg=-I$(MY_HILA_DIR)/libraries/plumbing \
  --extra-arg=-I$(MY_HILA_DIR)/libraries/datatypes \
  --extra-arg=-I$(MY_HILA_DIR)/libraries/tools 

.PHONY: run_test_input
run_test_input:
	./idx_eval_file --dot $(STD_INCLUDE_DIRS) $(HILA_INCLUDES) test_input.cpp