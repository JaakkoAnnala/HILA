#ifndef IDX_EXPR_EVALUATOR_H_
#define IDX_EXPR_EVALUATOR_H_

/**
 * @file idx_expr_evaluator.h
 * @brief Utility for analysing integer 'index like' expressions during AST analysis. Evaluates
 * integer expressions (as much as it can) and accumulates possible different values variables and
 * expressions obtain during evaluation.
 * @details
 * @note Should be tested if there are major changes to llvm. Unit tests for Idx_expr_evaluator
 * are in ../unit_tests/test_idx_expr_evaluator. Tested with llvm 18, 20.1.8, 22.1.3
 *
 */

// Limitations:

// ###### Things to NOTE / TODO ######

// TODO: check llvm version requirement...

// TODO: At the moment we give up every time that we encounter a control flow that we cannot
// determine which branch to take. However, if we only care about for example ´x´ in:
//  " // y = some unknown runtime value
//  " int x,i;
//  " for(i=0;i<10;++i){
//  "     x++;
//  "     if(y > 0) do_something();
//  " }
// if we figure out that `do_something()` does not affect the value of `x` we could still evaluate
// the above... probably needs a quite a bit of figuring out...


#include <map>
#include <memory>
#include <set>
#include <optional>
#include <unordered_map>
#include <vector>
#include "clang/Analysis/CFG.h"

using namespace clang;


#define MAX_BLOCK_VISITS 100 // TODO: make this cofigurable
// Default for Eval_state::max_block_states used by exec_WLF()
#define DEFAULT_MAX_BLOCK_STATES 10000
// Maximum depth of nested constexpr function calls we evaluate
#define MAX_CALL_DEPTH 64

using Value_set = std::set<std::optional<int64_t>>;

struct Eval_result {
    std::optional<int64_t> val = std::nullopt;
};

// exec_WLF(): results reused between calls to the same constexpr function. Owned by
// Idx_expr_evaluator, as the cached pointers are only valid for one ASTContext.
struct Function_eval_cache {
    std::unordered_map<const FunctionDecl *, std::unique_ptr<CFG>> cfgs;
    // (function, argument values) -> return value
    std::map<std::pair<const FunctionDecl *, std::vector<std::optional<int64_t>>>,
             std::optional<int64_t>>
        return_vals;
};

struct Eval_state {

    // Selects the worklist CFG traversal of exec_WLF() also for called constexpr function bodies.
    // The legacy evaluator leaves this false and continues to use eval_CFG_block().
    bool use_worklist = false;

    // exec_WLF(): the maximum number of distinct states (variable values) allowed to enter a single
    // CFG block. More means e.g. a loop with an unknown or a too large trip count, in which case the
    // evaluation is stopped and does not succeed.
    uint64_t max_block_states = DEFAULT_MAX_BLOCK_STATES;

    // exec_WLF(): shared with the Eval_states of called constexpr functions, may be nullptr.
    Function_eval_cache *func_cache = nullptr;

    // Depth of nested constexpr function calls
    unsigned call_depth = 0;

    ////////////////////////////////////////////
    // The state during evaluation:

    // Map of all variables we are tracking ´VarDecl*` to their current value
    // ´std::optional<int64_t>´ during the evaluation.
    // A value of std::nullopt means we dont know the value.
    std::unordered_map<const VarDecl *, std::optional<int64_t>> vars;
    // When the evaluation ends in a return statement the return value is put here.
    // If the value could not be evaluated, or the function does not return anything
    // return_val is nullopt.
    std::optional<int64_t> return_val = std::nullopt;

    // Store the last evaluated expr used for short circuiting in (x?a:b) , || , && expressions.
    Eval_result last_res{};

    std::unordered_map<const Expr *, std::optional<int64_t>> block_expr_state;

    // Track how many times we have visited each block in the control flow graph `CFG`.
    // This needs to be done so we can limit the amount of times we allow the evaluation to evaluate
    // e.g. loops.
    std::unordered_map<const CFGBlock *, uint64_t> block_visited;
    //
    CFGBlock *exit_block;

    ////////////////////////////////////////////
    // The results that we care about in the end

    // maps the Expr* to all its possible values it obtains during evaluation
    std::unordered_map<const Expr *, Value_set> possible_vals_for_expr;
    // set to true if `possible_vals_for_expr` has reliable results
    bool eval_succeeded = false;

    // list all possible values of all variables we encountered during evaluation
    std::unordered_map<const VarDecl *, Value_set> all_possible_vals;
};


struct Idx_expr_evaluator {

    bool init(const FunctionDecl *func, Stmt *statement_in_the_func, ASTContext *ASTctx);

    void add_expr_to_tracking_list(const Expr *E);

    // return false if we encountered something we dont handle.
    bool exec();

    // Like exec(), but explores all execution paths with a worklist: a condition with an unknown
    // value follows both branches instead of failing. Sets main_state.eval_succeeded. Returns false
    // if we encountered something we dont handle or more than main_state.max_block_states distinct
    // states reach a CFG block; the results are not reliable then.
    bool exec_WLF();

    /**
     * @param E the Expr* we want to get the possible values for.
     * @return pair.first = const Value_set *, pointer to the set of possible values, nullptr if the
     * Expr *E is not in the tracked expressions. pair.second = bool, false if E not found or if the
     * set of possible values contains unknown values (std::nullopt), meaning that we could not
     * track the value.
     * note: Empty set is a valid return.
     */
    std::pair<const Value_set *, bool> get_vals_for_expr(Expr *E) const;


    void print_vals_for_expr(llvm::raw_ostream &os, Expr *E) const;
    void print_all_vals_vars(llvm::raw_ostream &os) const;
    void print_all_vals_exprs(llvm::raw_ostream &os) const;
    void print_eval_diags(llvm::raw_ostream &os) const;
    std::string &get_eval_diags_string();

    Eval_state main_state{};

//  private:
    std::unique_ptr<CFG> cfg;
    ASTContext *ASTctx;
    Function_eval_cache func_cache;
    CFGBlock *main_entry_block = nullptr;
    CFGBlock *main_exit_block = nullptr;
};

#endif // IDX_EXPR_EVALUATOR_H_
