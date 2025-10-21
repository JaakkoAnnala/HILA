#ifndef IDX_EXPR_EVALUATOR_H_
#define IDX_EXPR_EVALUATOR_H_


#include <clang/StaticAnalyzer/Frontend/FrontendActions.h>

#include "clang/Analysis/CFG.h"

using namespace clang;

// TODO: check llvm version requirement...

#define MAX_BLOCK_VISITS 100 // TODO: make this cofigurable

struct Eval_state {

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

    // Track how many times we have visited each block in the control flow graph `CFG`.
    // This needs to be done so we can limit the amount of times we allow the evaluation to evaluate
    // e.g. loops.
    std::unordered_map<const CFGBlock *, uint64_t> block_visited;
    //
    CFGBlock *exit_block;

    ////////////////////////////////////////////
    // The results that we care about in the end

    // maps the Expr* to all its possible values it obtains during evaluation
    std::unordered_map<const Expr *, std::set<std::optional<int64_t>>> possible_vals_for_expr;
    // set to true if `possible_vals_for_expr` has reliable results
    bool eval_succeeded = false;

    // list all possible values of all variables we encountered during evaluation
    std::unordered_map<const VarDecl *, std::set<std::optional<int64_t>>> all_possible_vals;
};

struct Eval_result {
    std::optional<int64_t> val = std::nullopt;
};

struct Idx_expr_evaluator {

    bool init(const FunctionDecl *func, Stmt *statement_in_the_func, ASTContext *ASTctx);

    void add_expr_to_tracking_list(const Expr *E);

    bool exec();

    // bool get_possible_vals_for_expr(Expr *E, const std::set<std::optional<int64_t>> &vals) const;

    void print_eval_diags(llvm::raw_ostream &os);
    std::string &get_eval_diags_string();

    Eval_state main_state{};

  private:
    std::unique_ptr<CFG> cfg;
    ASTContext *ASTctx;
    CFGBlock *main_entry_block = nullptr;
    CFGBlock *main_exit_block = nullptr;
};

#endif // IDX_EXPR_EVALUATOR_H_