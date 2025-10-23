#include "idx_expr_evaluator.h"

#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"

using namespace clang;
using namespace llvm;

#define BRED_STR(str) "\033[41m" str "\033[0m"
#define RED_STR(str) "\033[0;31m" str "\033[0m"

static std::string diag_msg;
static raw_string_ostream diags(diag_msg);
static constexpr raw_string_ostream::Colors RED = raw_string_ostream::RED;
static constexpr raw_string_ostream::Colors BLUE = raw_string_ostream::BLUE;
// static constexpr raw_string_ostream::Colors BLACK = raw_string_ostream::BLACK;
static constexpr raw_string_ostream::Colors GREEN = raw_string_ostream::GREEN;
// static constexpr raw_string_ostream::Colors YELLOW = raw_string_ostream::YELLOW;
// static constexpr raw_string_ostream::Colors MAGENTA = raw_string_ostream::MAGENTA;
static constexpr raw_string_ostream::Colors CYAN = raw_string_ostream::CYAN;
// static constexpr raw_string_ostream::Colors WHITE = raw_string_ostream::WHITE;
// static constexpr raw_string_ostream::Colors BRIGHT_BLACK = raw_string_ostream::BRIGHT_BLACK;
// static constexpr raw_string_ostream::Colors BRIGHT_RED = raw_string_ostream::BRIGHT_RED;
// static constexpr raw_string_ostream::Colors BRIGHT_GREEN = raw_string_ostream::BRIGHT_GREEN;
// static constexpr raw_string_ostream::Colors BRIGHT_YELLOW = raw_string_ostream::BRIGHT_YELLOW;
// static constexpr raw_string_ostream::Colors BRIGHT_BLUE = raw_string_ostream::BRIGHT_BLUE;
// static constexpr raw_string_ostream::Colors BRIGHT_MAGENTA = raw_string_ostream::BRIGHT_MAGENTA;
// static constexpr raw_string_ostream::Colors BRIGHT_CYAN = raw_string_ostream::BRIGHT_CYAN;
// static constexpr raw_string_ostream::Colors BRIGHT_WHITE = raw_string_ostream::BRIGHT_WHITE;
// static constexpr raw_string_ostream::Colors SAVEDCOLOR = raw_string_ostream::SAVEDCOLOR;
// static constexpr raw_string_ostream::Colors RESET = raw_string_ostream::RESET;

static inline raw_string_ostream &color_msg(raw_string_ostream &os,
                                            raw_string_ostream::Colors color, const char *msg,
                                            bool bold = true) {
    os.changeColor(color, bold);
    os << msg;
    os.resetColor();
    return os;
}

static inline raw_ostream &color_msg(raw_ostream &os, raw_ostream::Colors color, const char *msg,
                                     bool bold = true, const char *file = nullptr, int line = -1) {
    if (file)
        os << file;
    if (line != -1)
        os << ":" << line << " ";
    os.changeColor(color, bold);
    os << msg;
    os.resetColor();
    return os;
}

#ifdef DIAGS_TO_STR
#define INTERNAL_NOTE color_msg(diags, BLUE, "INTERNAL NOTE: ")
#define INTERNAL_ERROR color_msg(diags, RED, "INTERNAL ERROR: ")
#define DEBUG_OUT color_msg(diags, RED, "INTERNAL DEBUG: ")
#else
#define INTERNAL_NOTE color_msg(llvm::outs(), BLUE, "INTERNAL NOTE: ", true, __FILE__, __LINE__)
#define INTERNAL_ERROR color_msg(llvm::outs(), RED, "INTERNAL ERROR: ", true, __FILE__, __LINE__)
#define DEBUG_OUT color_msg(llvm::outs(), CYAN, "INTERNAL DEBUG: ", true, __FILE__, __LINE__)
#endif


void print_var_state(Eval_state &state, const VarDecl *var, ASTContext &ASTctx) {
    if (state.vars.find(var) != state.vars.end()) {

        llvm::outs() << var->getSourceRange().getBegin().printToString(ASTctx.getSourceManager())
                     << " " << var->getNameAsString() << " = " << state.vars[var] << "\n";
    } else {
        llvm::outs() << var->getSourceRange().getBegin().printToString(ASTctx.getSourceManager())
                     << " " << var->getNameAsString() << " : The variable is not being tracked.\n";
    }
}

#define loc_str(S) (S)->getSourceRange().getBegin().printToString(ASTctx.getSourceManager())

bool eval_CFG_block(const CFGBlock &block, Eval_state &state, ASTContext &ASTctx);
bool eval_stmt(const Stmt *S, Eval_state &state, ASTContext &ASTctx);
bool eval_expr(Eval_result &result, const Expr *E, Eval_state &state, ASTContext &ASTctx);

// Helper that adds the result of Expr E to the state.possible_vals_for_expr
// if we are tracking the Expr E
inline void track_expr(Eval_result &result, const Expr *E, Eval_state &state) {
    auto vals = state.possible_vals_for_expr.find(E);
    if (vals == state.possible_vals_for_expr.end())
        return; // we are not tracking this Expr

    vals->second.insert(result.val);
}

// recursively eval
bool eval_expr(Eval_result &result, const Expr *E, Eval_state &state, ASTContext &ASTctx) {
    result.val = std::nullopt;

    if (state.block_expr_state.find(E) != state.block_expr_state.end()) {
        DEBUG_OUT << " trying to eval twice.. " << loc_str(E) << "\n";
        // we have already evaluated this expression
        result.val = state.block_expr_state[E];
        return true;
    }

    // Every time we 'successfully' evaluate something we possibly need to save the result of this
    // expression to state.possible_vals_for_expr, and return true: `goto successful_return` does
    // this.

    if (const auto *IL = dyn_cast<IntegerLiteral>(E)) {

        result.val = IL->getValue().getSExtValue();
        goto successful_return;
    } else if (const auto *default_arg = dyn_cast<CXXDefaultArgExpr>(E)) {
        // This takes care of functions with default arguments
        Eval_result default_val{};
        if (!eval_expr(default_val, default_arg->getExpr(), state, ASTctx))
            return false;
        result = default_val;
        goto successful_return;

    } else if (const auto *PE = dyn_cast<ParenExpr>(E)) {
        Eval_result paren_expr_val{};
        if (!eval_expr(paren_expr_val, PE->getSubExpr(), state, ASTctx))
            return false;
        result = paren_expr_val;
        goto successful_return;

    } else if (const auto *DRE = dyn_cast<DeclRefExpr>(E->IgnoreParenImpCasts())) {

        // check if its an enum constant
        if (const auto *EC = dyn_cast<EnumConstantDecl>(DRE->getDecl())) {
            result.val = EC->getInitVal().getSExtValue();
            goto successful_return;
        }

        const auto *VD = dyn_cast<VarDecl>(DRE->getDecl());
        if (!VD) {
            INTERNAL_ERROR << " No VarDecl found for DeclRefExpr " << loc_str(DRE) << "\n";
            return false;
        }

        if (state.vars.find(VD) == state.vars.end()) {
            INTERNAL_NOTE << " the variable has an unknown value " << loc_str(DRE) << "\n";
            diags << "             : VarDecl " << loc_str(VD) << " " << VD << "\n";
            goto successful_return;
        }
        result.val = state.vars[VD];
        goto successful_return;

    } else if (const auto *BO = dyn_cast<BinaryOperator>(E)) {
        const auto *LHS = BO->getLHS();
        const auto *RHS = BO->getRHS();
        if (BO->isAssignmentOp()) {
            // TODO: for now LHS has to be a simple VarRefDecl
            const auto *dreLHS = dyn_cast<DeclRefExpr>(LHS->IgnoreParenImpCasts());
            if (!dreLHS) {
                INTERNAL_ERROR
                    << "Expecting binary assignment operators LHS to be DeclRefExpr: insted it is `"
                    << LHS->IgnoreParenBaseCasts()->getStmtClassName() << "` " << loc_str(LHS)
                    << "\n";
                return false;
            }
            const auto *VD = dyn_cast<VarDecl>(dreLHS->getDecl());
            if (!VD) {
                INTERNAL_ERROR << "Expecting binary assignment operators LHS DeclRefExpr to have "
                                  "an associated VarDecl."
                               << loc_str(dreLHS) << "\n";
                return false;
            }

            Eval_result rhs_res{};
            if (!eval_expr(rhs_res, RHS, state, ASTctx))
                return false;
            // if rhs has no value should set and return nullopt in all cases
            if (!rhs_res.val.has_value()) {
                state.vars[VD] = std::nullopt;
                result.val = std::nullopt;
                goto successful_return;
            }

            auto op = BO->getOpcode();
            switch (op) {
                //// [C99 6.5.16] Assignment operators.
                // BO_Assign // "="
            case BO_Assign: {
                state.vars[VD] = rhs_res.val;
                result = rhs_res;
                // var changes
                state.all_possible_vals[VD].insert(state.vars[VD]);
                goto successful_return;
            }
// temp helper macro to generate all the similar cases
#define BOP_ASSIGN(OP)                                                                             \
    {                                                                                              \
        if (state.vars[VD].has_value())                                                            \
            state.vars[VD].value() OP rhs_res.val.value();                                         \
        result.val = state.vars[VD];                                                               \
        state.all_possible_vals[VD].insert(state.vars[VD]);                                        \
        goto successful_return;                                                                    \
    }
                // clang-format off
            case BO_MulAssign: BOP_ASSIGN(*=); break;
            case BO_DivAssign: BOP_ASSIGN(/=); break;
            case BO_RemAssign: BOP_ASSIGN(%=); break;
            case BO_AddAssign: BOP_ASSIGN(+=); break;
            case BO_SubAssign: BOP_ASSIGN(-=); break;
            case BO_ShlAssign: BOP_ASSIGN(<<=); break;
            case BO_ShrAssign: BOP_ASSIGN(>>=); break;
            case BO_AndAssign: BOP_ASSIGN(&=); break;
            case BO_XorAssign: BOP_ASSIGN(^=); break;
            case BO_OrAssign:  BOP_ASSIGN(|=); break;
#undef BOP_ASSIGN
            // clang-format on
            default:
                INTERNAL_ERROR << " eval_expr BO opcode not implemented `" << BO->getOpcodeStr()
                               << "` " << loc_str(BO) << "\n";
                return false;
            }
        } else {
            // TODO: !!! check c++ specs if the order of evaluation is defined... LHS evaluated
            // first
            auto op = BO->getOpcode();

            Eval_result lhs_res{};
            if (!eval_expr(lhs_res, LHS, state, ASTctx))
                return false;
            if (!lhs_res.val.has_value())
                goto successful_return; // We dont know the values just return the nullopt

            Eval_result rhs_res{};
            // special cases || and &&
            if (op == BO_LAnd) {
                //// [C99 6.5.13] Logical AND operator.
                // eval rhs only if lhs is true
                if (lhs_res.val) {
                    if (!eval_expr(rhs_res, RHS, state, ASTctx))
                        return false;
                    result.val = lhs_res.val.value() && rhs_res.val.value();
                } else {
                    result.val = lhs_res.val.value();
                }
                goto successful_return;
            } else if (op == BO_LOr) {
                //// [C99 6.5.14] Logical OR operator.
                // eval rhs only if lhs is false
                if (!lhs_res.val) {
                    if (!eval_expr(rhs_res, RHS, state, ASTctx))
                        return false;
                    result.val = lhs_res.val.value() || rhs_res.val.value();
                } else {
                    result.val = lhs_res.val.value();
                }
                goto successful_return;
            }

            if (!eval_expr(rhs_res, RHS, state, ASTctx))
                return false;
            if (!rhs_res.val.has_value())
                goto successful_return; // We dont know the values just return the nullopt

            switch (op) {

// temp helper macro to generate all the similar cases
#define BOP_EXPR(OP)                                                                               \
    {                                                                                              \
        result.val = lhs_res.val.value() OP rhs_res.val.value();                                   \
        goto successful_return;                                                                    \
    }
                // clang-format off
            case BO_Mul: BOP_EXPR(*); break;
            case BO_Div: BOP_EXPR(/); break;
            case BO_Rem:  BOP_EXPR(%); break;
                //// [C99 6.5.6] Additive operators.
            case BO_Add:  BOP_EXPR(+); break;
            case BO_Sub:  BOP_EXPR(-); break;
                //// [C99 6.5.7] Bitwise shift operators.
            case BO_Shl:  BOP_EXPR(<<); break;
            case BO_Shr:  BOP_EXPR(>>); break;
#if __cplusplus >= 202002L
            //// C++20 [expr.spaceship] Three-way comparison operator.
            case BO_Cmp:  BOP_EXPR(<=>); break;
#endif
                //// [C99 6.5.8] Relational operators.
            case BO_LT:  BOP_EXPR(<); break;
            case BO_GT:  BOP_EXPR(>); break;
            case BO_LE:  BOP_EXPR(<=); break;
            case BO_GE:  BOP_EXPR(>=); break;
            //// [C99 6.5.9] Equality operators.
            case BO_EQ:  BOP_EXPR(==); break;
            case BO_NE:  BOP_EXPR(!=); break;
                //// [C99 6.5.10] Bitwise AND operator.
            case BO_And:  BOP_EXPR(&); break;
                //// [C99 6.5.11] Bitwise XOR operator.
            case BO_Xor:  BOP_EXPR(^); break;
                //// [C99 6.5.12] Bitwise OR operator.
            case BO_Or:  BOP_EXPR(|); break;
            // clang-format on
            //// [C99 6.5.17] Comma operator.
            // BO_Comma // ","
            case BO_Comma: { // comma operator evals both and returns the rhs
                result.val = (lhs_res.val.value(), rhs_res.val.value());
                goto successful_return;
            }
#undef BOP_EXPR
            default:
                INTERNAL_ERROR << " TODO: eval_expr BO opcode not implemented `"
                               << BO->getOpcodeStr() << "` " << loc_str(BO) << "\n";
                return false;
            }
        }
    } else if (const auto *UO = dyn_cast<UnaryOperator>(E)) {
        const auto *operand = UO->getSubExpr();
        auto op = UO->getOpcode();

        if (op == UO_PostInc || op == UO_PostDec || op == UO_PreInc || op == UO_PreDec) {
            // TODO: for now operand has to be a simple VarRefDecl
            const auto *dre = dyn_cast<DeclRefExpr>(operand->IgnoreParenImpCasts());
            if (!dre) {
                INTERNAL_ERROR
                    << "Expecting unary operator operands to be DeclRefExpr: insted it is `"
                    << operand->IgnoreParenBaseCasts()->getStmtClassName() << "` "
                    << loc_str(operand) << "\n";
                return false;
            }
            const auto *VD = dyn_cast<VarDecl>(dre->getDecl());
            if (!VD) {
                INTERNAL_ERROR << "Expecting unary operator operands DeclRefExpr to have an "
                                  "associated VarDecl."
                               << loc_str(operand) << "\n";
                return false;
            }

// temp helper macro to generate all the similar cases
#define UOP_ASSIGN_POST(OP)                                                                        \
    {                                                                                              \
        result.val = state.vars[VD]; /* result is the value before inc/dec */                      \
        if (state.vars[VD].has_value())                                                            \
            state.vars[VD].value() OP;                                                             \
        state.all_possible_vals[VD].insert(state.vars[VD]);                                        \
        goto successful_return;                                                                    \
    }
#define UOP_ASSIGN_PRE(OP)                                                                         \
    {                                                                                              \
        if (state.vars[VD].has_value())                                                            \
            OP state.vars[VD].value();                                                             \
        result.val = state.vars[VD]; /* result is the value after inc/dec */                       \
        state.all_possible_vals[VD].insert(state.vars[VD]);                                        \
        goto successful_return;                                                                    \
    }
            // clang-format off
            switch (op) {
                // [C99 6.5.2.4] Postfix increment and decrement
            case UO_PostInc: UOP_ASSIGN_POST(++); break;
            case UO_PostDec: UOP_ASSIGN_POST(--); break;
                // [C99 6.5.3.1] Prefix increment and decrement
            case UO_PreInc: UOP_ASSIGN_PRE(++); break;
            case UO_PreDec: UOP_ASSIGN_PRE(--); break;
                // clang-format on
#undef UOP_ASSIGN_POST
#undef UOP_ASSIGN_PRE
            default:
                // Unreachable
                INTERNAL_ERROR << " TODO: eval_expr UO opcode not implemented `"
                               << UO->getOpcodeStr(op) << "` " << loc_str(UO) << "\n";
                return false;
            }

        } else {

            // eval operand
            Eval_result operand_res{};
            if (!eval_expr(operand_res, operand, state, ASTctx))
                return false;

            // now we dont know, so return the nullopt
            if (!operand_res.val.has_value())
                goto successful_return;

            // clang-format off
            switch (op) {
                // [C99 6.5.3.2] Address and indirection
                // UO_AddrOf "&"
                // UO_Deref "*"
                // [C99 6.5.3.3] Unary arithmetic
            case UO_Plus:  { result.val = +operand_res.val.value(); goto successful_return; } break;
            case UO_Minus: { result.val = -operand_res.val.value(); goto successful_return; } break;
            case UO_Not:   { result.val = ~operand_res.val.value(); goto successful_return; } break;
            case UO_LNot:  { result.val = !operand_res.val.value(); goto successful_return; } break;
                // "__real expr"/"__imag expr" Extension.
                // UO_Real "__real"
                // UO_Imag "__imag"
                // __extension__ marker.
                // UO_Extension "__extension__"
                // [C++ Coroutines] co_await operator
                // UO_Coawait "co_await"
                // clang-format on
            default:
                INTERNAL_ERROR << " TODO: eval_expr UO opcode not implemented `"
                               << UO->getOpcodeStr(op) << "` " << loc_str(UO) << "\n";
                return false;
            }
        }
    } else if (const auto *SCE = dyn_cast<CXXStaticCastExpr>(E)) {
        // TODO: Interpret static_cast as a nop.. This might not be correct..
        const auto *e = SCE->getSubExpr()->IgnoreImpCasts();
        if (!eval_expr(result, e, state, ASTctx))
            return false;
        goto successful_return;
    } else if (const auto *IC = dyn_cast<ImplicitCastExpr>(E)) {
        // TODO: Interpret Implicit casts as a nop.. This might not be correct..
        if (!eval_expr(result, IC->IgnoreImpCasts(), state, ASTctx))
            return false;
        goto successful_return;
    } else if (const auto *CE = dyn_cast<CallExpr>(E)) {
        const auto *func = CE->getDirectCallee();

        bool do_eval = true;
        if (!func->isConstexpr()) {
            color_msg(diags, BLUE, "INTERNAL NOTE: ")
                << "Call to a function that is not a constexpr " << loc_str(CE) << "\n";
            do_eval = false;
        }

        // the call needs a separate Eval_state
        Eval_state call_state;

        // TODO: FIXME: Probably needs to be fixed for c++ methods since they have an implicit this
        // pointer..
        // NOTE: !!! This assumes CE->arguments() and func->parameters() are in the same
        // order... They probably are not in the case of class method calls..
        // Prepare the argument values to the function call
        int i = 0;
        for (auto arg_expr : CE->arguments()) {
            // the ->arguments() contains CXXDefaultArgExpr which are now taken care of the
            // eval_expr. So default arguments are taken care of.
            Eval_result arg_val{};
            // Even if we dont want to eval the call we want to try eval the argument expressions
            // which can affect the result..
            if (!eval_expr(arg_val, arg_expr, state, ASTctx))
                return false;
            if (do_eval) {
                auto vd = dyn_cast<VarDecl>(func->parameters()[i]);
                call_state.vars[vd] = arg_val.val.value();
            }
            i++;
        }

        if (do_eval) {
            auto *func_body = func->getBody();
            if (!func_body) {
                INTERNAL_ERROR << " no body found for function " << loc_str(func) << "\n";
                return false;
            }

            // Build CFG for the function
            CFG::BuildOptions CFG_BO;
            auto CFG = CFG::buildCFG(func, func_body, &ASTctx, CFG_BO);
            auto entry_block = CFG->getEntry();
            auto exit_block = CFG->getExit();
            call_state.exit_block = &exit_block;
            // eval the call
            if (!eval_CFG_block(entry_block, call_state, ASTctx))
                return false;

            result.val = call_state.return_val.value();
            goto successful_return;
        }
        goto successful_return;

    } else if (const auto *LAM = dyn_cast<LambdaExpr>(E)) {
        // TODO: Some lambda functions could be analysed
        // But things get a bit tricky
        // clang-format off
        //     int dir = 0; // = the variable we wish to track
        //     auto sneaky = [&]() { dir = dir + 1; };
        //     for (int i = 0; i < 10; ++i) {
        //         // If we encounter an function call that does not take any of the tracked variables by ref
        //         // or pointer we can safely assume they do not affect the variables we care about.
        //         // HOWEVER: if the call is lambda function call: CXX_something_something then it may take
        //         // the variables we care about by ref and change them. We need to detect this. Need to
        //         // analyze the function call and see if it modifies any of the variables we need to track.
        //         // For simplicity we can just not allow these kind of non constexpr lambdas for now. TODO.
        //         sneaky();
        //     }
        // clang-format on
        INTERNAL_ERROR << " TODO: lambda functions not implemented yet" << loc_str(LAM) << "\n";
        return false;
    } else if (const auto *constructor_expr = dyn_cast<CXXConstructExpr>(E)) {
        // The possible constructor argument have to be evaluated.
        // For example if we encounter something crazy like `some_constructor( x=1 );` where `x` is
        // a variable we care about.
        for (auto arg_expr : constructor_expr->arguments()) {
            Eval_result arg_val{};
            if (!eval_expr(arg_val, arg_expr, state, ASTctx))
                return false;
        }

        // TODO(CXXConstructExpr):  ... not trying to eval these for now
        INTERNAL_NOTE << " Not trying to eval CXXConstructExpr. Ignoring for now "
                      << loc_str(constructor_expr) << "\n";
        goto successful_return;
    } else if (const auto *array_subscript = dyn_cast<ArraySubscriptExpr>(E)) {
        // the LHS[RHS] expressions have to be evaluated since they might affect variables we care
        // about E.g. `array[x++]`
        auto *LHS = array_subscript->getLHS();
        Eval_result lhs_res{};
        if (!eval_expr(lhs_res, LHS, state, ASTctx))
            return false;
        Eval_result rhs_res{};
        auto *RHS = array_subscript->getRHS();
        if (!eval_expr(rhs_res, RHS, state, ASTctx))
            return false;

        // TODO: ... not trying to eval these for now
        INTERNAL_NOTE << " Not trying to eval ArraySubscriptExpr. Ignoring for now "
                      << loc_str(array_subscript) << "\n";
        goto successful_return;
    }

    INTERNAL_ERROR << "TODO: UNHANDELED EXPR: eval_expr() " << loc_str(E) << "\n";
    E->dump(diags, ASTctx);
    return false;

successful_return:
    track_expr(result, E, state);
    state.block_expr_state[E] = result.val;
    return true;
}


bool eval_stmt(const Stmt *S, Eval_state &state, ASTContext &ASTctx) {


    if (const auto *CS = dyn_cast<CompoundStmt>(S)) {
        for (const auto s : CS->children()) {
            if (!eval_stmt(s, state, ASTctx))
                return false;
        }
        return true;

    } else if (const auto *DS = dyn_cast<DeclStmt>(S)) {
        // TODO: we should ignore all Decls that have a type thats underlying primitive
        // representation is not an integer

        // DeclStmt may have more than one decl:  int x,y=0,z;
        for (const Decl *decl : DS->decls()) {
            if (const auto *VD = dyn_cast<VarDecl>(decl)) {
                if (VD->hasInit()) {
                    const Expr *init = VD->getInit();
                    Eval_result res{};
                    if (!eval_expr(res, init, state, ASTctx))
                        return false;
                    state.vars[VD] = res.val;
                    // var changes
                    state.all_possible_vals[VD].insert(state.vars[VD]);
                    return true;
                } else {
                    // if no init it is std::nullopt
                    state.vars[VD] = std::nullopt;
                    // var changes
                    state.all_possible_vals[VD].insert(state.vars[VD]);
                    return true;
                }
            }
        }
    } else if (const auto *RE = dyn_cast<ReturnStmt>(S)) {
        const auto *ret_expr = RE->getRetValue();
        if (!ret_expr) {
            return true;
        }
        Eval_result ret_val{};
        if (!eval_expr(ret_val, ret_expr, state, ASTctx))
            return false;
        state.return_val = ret_val.val;

        return true;

    } else if (const auto *E = dyn_cast<Expr>(S)) {
        Eval_result res{};
        if (!eval_expr(res, E, state, ASTctx))
            return false;
        return true;
    }
    INTERNAL_ERROR << " UNHANDELED STMT eval_stmt():\n" << loc_str(S) << "\n";
    S->dump(diags, ASTctx);
    return false;
}

bool debug_print_CFG_block(const CFGBlock &block, Eval_state &state, ASTContext &ASTctx) {
    bool ret = true; // set this to false if we encounter something we don't handle

    if (state.block_visited.find(&block) == state.block_visited.end())
        state.block_visited[&block] = 0;
    if (state.block_visited[&block] > 1 /*visit each block only once*/)
        return true;
    state.block_visited[&block]++;

    const auto *trm_stmt = block.getTerminatorStmt();
    const Stmt *cond = nullptr;
    bool is_binaryOperator_LAnd_Lor = false;
    if (trm_stmt) {
        if (const auto *IF = dyn_cast<IfStmt>(trm_stmt)) {
            cond = IF->getCond();
        } else if (const auto *FOR = dyn_cast<ForStmt>(trm_stmt)) {
            cond = FOR->getCond();
        } else if (const auto *WHILE = dyn_cast<WhileStmt>(trm_stmt)) {
            cond = WHILE->getCond();
        } else if (const auto *DO = dyn_cast<DoStmt>(trm_stmt)) {
            cond = DO->getCond();
        } else if (const auto *SW = dyn_cast<SwitchStmt>(trm_stmt)) {
            cond = SW->getCond();

            // TODO:
            //} else if (const auto *CS = dyn_cast<ConditionalOperator>(trm_stmt)) {
            //    cond = CS->getCond();
        } else if (const auto *BO = dyn_cast<BinaryOperator>(trm_stmt)) {
            is_binaryOperator_LAnd_Lor = true;

        } else if (const auto *BRK = dyn_cast<BreakStmt>(trm_stmt)) {
            // BreakStmt does not have any associated condition, so we dont need anything
            (void)BRK; // Unused
        } else if (const auto *CON = dyn_cast<ContinueStmt>(trm_stmt)) {
            // ContinueStmt does not have any associated condition, so we dont need anything
            (void)CON; // Unused
        } else if (const auto *GOTO = dyn_cast<GotoStmt>(trm_stmt)) {
            // ContinueStmt does not have any associated condition, so we dont need anything
            (void)GOTO; // Unused
        } else {
            llvm::outs() << BRED_STR("TODO:") " Unhandeled terminatorStmt class "
                         << loc_str(trm_stmt) << " : " << trm_stmt->getStmtClassName() << "\n";
            ret = false;
        }
    }

    llvm::outs() << "------------------------------\n";
#define INDENT0 "            : "
    //          "- block[n]  :
    llvm::outs() << "- block[" << block.getBlockID() << "] " << " : \n";

    const auto &last_element = *block.rbegin();
    int eidx = 0;
    for (const CFGElement &element : block) {

        if (&last_element == &element)
            llvm::outs() << "LAST ELEMENT\n";
        llvm::outs() << INDENT0 "element[" << eidx << "]:  ";
        if (element.getKind() == CFGElement::Statement) {
            const auto *stmt = element.castAs<CFGStmt>().getStmt();
            // stmt->dumpColor();
            llvm::outs() << stmt->getStmtClassName() << " `";
            element.dump();
#define INDENT1 "            "
            llvm::outs() << INDENT1 INDENT0 << loc_str(stmt);

            if (cond && cond == stmt) {
                llvm::outs() << " cond " << cond << " stmt " << stmt << "\n";
                llvm::outs() << RED_STR(" [Terminator Expr] : ") << stmt->getStmtClassName();
            } else if (cond && is_binaryOperator_LAnd_Lor && &last_element == &element) {
                llvm::outs() << " cond " << cond << " stmt " << stmt << "\n";
                llvm::outs() << RED_STR(" [Terminator Expr] : is_binaryOperator_LAnd_Lor=true ")
                             << stmt->getStmtClassName();
            }
            llvm::outs() << "\n";

            if (const auto *E = dyn_cast<Expr>(stmt)) {
                if (const auto *_ = dyn_cast<CallExpr>(E)) {

                    llvm::outs() << INDENT1 INDENT0 << "Expr ";
                    auto paren = ASTctx.getParents(*E);
                    if (!paren.empty()) {
                        llvm::outs() << "paren len " << paren.size() << "  paren[0] "
                                     << (paren[0].getSourceRange().getBegin().printToString(
                                            ASTctx.getSourceManager()));
                        const auto *pe = paren[0].get<Expr>();
                        llvm::outs() << " Expr: " << E << " paren: " << pe;
                        if (pe != nullptr) {
                            llvm::outs() << RED_STR(" Extra control flow call ");
                        }
                    }
                    llvm::outs() << "\n";
                }
            }

        } else {
            element.dump();
            llvm::outs() << INDENT1 INDENT0 " Unhandled CFGElement Kind: " << element.getKind()
                         << "\n";
            ret = false;
        }
        eidx++;
    }

    // auto term = block.getTerminator();
    if (trm_stmt) {
        llvm::outs() << INDENT0 RED_STR(" [Terminator Stmt] :  ") << loc_str(trm_stmt) << " : "
                     << trm_stmt->getStmtClassName() << " " << trm_stmt << "\n";
        // term.getStmt()->dumpColor();
    }

    uint64_t i = 0;
    for (auto it = block.succ_begin(); it < block.succ_end(); it++, i++) {
        llvm::outs() << INDENT0 " -> succ[" << i << "] = block[" << (*it)->getBlockID() << "]\n";
    }

    for (const CFGBlock *succ : block.succs()) {
        bool r = debug_print_CFG_block(*succ, state, ASTctx);
        if (!r)
            ret = false;
    }
    return ret;
}

// We depend on the successor ordering defined by CFGBlock, see comment above class CFGBlock in
// CFG.h in llvm sources
bool eval_CFG_block(const CFGBlock &block, Eval_state &state, ASTContext &ASTctx) {
    // reset block specific state
    state.block_expr_state.clear();

    if (state.block_visited.find(&block) == state.block_visited.end())
        state.block_visited[&block] = 0;
    if (state.block_visited[&block] > MAX_BLOCK_VISITS)
        return true;
    state.block_visited[&block]++;

    // the exit_block is just an empty block signaling that we are done
    if (&block == state.exit_block)
        return true;

    const auto *trm_stmt = block.getTerminatorStmt();
    Eval_result cond_res{};
    const CFGElement &last_element = *block.rbegin();
    // Eval all the elements in the block
    for (const CFGElement &element : block) {
        if (element.getKind() == CFGElement::Statement) {
            const auto *stmt = element.castAs<CFGStmt>().getStmt();
            // If there is a terminator statement the last statement is in most cases the condition
            // expression that is used to choose which successor to go to
            if (trm_stmt && &element == &last_element) {
                if (const auto *E = dyn_cast<Expr>(stmt)) {
                    if (!eval_expr(cond_res, E, state, ASTctx))
                        return false;
                    DEBUG_OUT << " cond_res " << cond_res.val << " " << loc_str(E) << " "
                              << E->getStmtClassName() << "\n";
                } else {
                    INTERNAL_ERROR << " Expecting the last element of an block with terminator "
                                      "stmt to be an expression "
                                   << loc_str(stmt) << "\n";
                    return false;
                }
            } else {
                if (!eval_stmt(stmt, state, ASTctx))
                    return false;
            }
        } else {
            INTERNAL_ERROR << " TODO: Unhandled CFGElement Kind: " << element.getKind() << "\n";
            return false;
        }
    }

#if 0
    const Expr *cond = nullptr;
    if (trm_stmt) {
        if (const auto *BO = dyn_cast<BinaryOperator>(trm_stmt)) {
            // Handles short circuiting
            if (BO->getOpcode() == BO_LAnd || BO->getOpcode() == BO_LOr) {
                DEBUG_OUT << " BO terminator statement ´" << BO->getOpcodeStr() << "` "
                          << loc_str(BO) << "\n";
                cond = BO; // the condition is the value of the BinaryOperator itself.
            } else {
                INTERNAL_ERROR << " Unhandeled BO terminator statement ´" << BO->getOpcodeStr()
                               << "` \n";
                return false;
            }

        } else if (const auto *CS = dyn_cast<ConditionalOperator>(trm_stmt)) {
            // this handles the ternary operator: cond ? true_case : false_case;
            INTERNAL_ERROR << " ConditionalOperator is not implemented yet, and thus no ternary "
                              "operators can be analysed"
                           << loc_str(CS) << "\n";
            return false;
            // clang-format off
        // TODO: these are a bit of a pain, since the generated CFG 
        // for e.g. `res = x ? 1 : 0` looks like:
        // - block[4]  : 
        //             : element: <other elements>
        //             : element:  `x` [Terminator Stmt] : ConditionalOperator
        //             :  -> succ[0] = block[2]
        //             :  -> succ[1] = block[3]
        // ------------------------------
        // - block[2]  : 
        //             : element:  `1`
        //             :  -> succ[0] = block[1]
        // ------------------------------
        // - block[3]  : 
        //             : element:  `0`
        //             :  -> succ[0] = block[1]
        // ------------------------------
        // - block[1]  : 
        //             : element:  `x ? 1 : 0`
        //             : element:  ` res = x ? 1 : 0
        //             :  -> succ[0] = block[0]
        // ? So we would need to add some functionality so that blocks can return a value,
        //   and ignore ConditionalOperators as we evaluate the Expr recursively...
        //   Or figure out something else..
            // clang-format on
        }
    }
#endif

    // Now based on the terminatorStmt result pick which block we go to
    auto succ = block.succ_begin();
    if (trm_stmt == nullptr // if no terminator statement or terminator is one of these:
        || dyn_cast<BreakStmt>(trm_stmt) // break, continue, goto we should have only one successor
        || dyn_cast<ContinueStmt>(trm_stmt) //
        || dyn_cast<GotoStmt>(trm_stmt)     //
    ) {
        if (block.succ_empty()) {
            // we have reached an early return
            return true;
        }
        if (block.succ_size() != 1) {
            INTERNAL_ERROR << " Expected block[" << block.getBlockID()
                           << "] to have 1 successors but it has succ_size " << block.succ_size()
                           << " terminator StmtClass: `" << trm_stmt->getStmtClassName() << "` \n";
            if (block.Elements.size() > 0)
                block.Elements.begin()->dump();
        }
        CFGBlock *s0 = succ[0]; // this is a cast, succ[0] is actually an AdjacentBlock
        return eval_CFG_block(*s0, state, ASTctx);
    } else if (dyn_cast<IfStmt>(trm_stmt)       //
               || dyn_cast<ForStmt>(trm_stmt)   //
               || dyn_cast<WhileStmt>(trm_stmt) //
               || dyn_cast<DoStmt>(trm_stmt)    //
    ) {
        if (block.succ_size() != 2) {
            INTERNAL_ERROR << "ERROR: Expected block[" << block.getBlockID()
                           << "] to have 2 successors but it has succ_size " << block.succ_size()
                           << " terminator StmtClass: `" << trm_stmt->getStmtClassName() << "` \n";
            return false;
        }
        CFGBlock *true_branch = succ[0];
        CFGBlock *false_branch = succ[1];
        // This might be a redundant check..
        if (!cond_res.val.has_value()) {
            // TODO: this might be relevant error for the user
            INTERNAL_ERROR << " 2 Could not determine the value of the condition. "
                              "Cant follow the control flow correctly. Giving up. "
                           << loc_str(trm_stmt) << "\n";
            return false;
        }
        if (cond_res.val.value()) {
            return eval_CFG_block(*true_branch, state, ASTctx);
        } else {
            return eval_CFG_block(*false_branch, state, ASTctx);
        }
        // TODO:
        //} else if (const auto *BO = dyn_cast<BinaryOperator>(trm_stmt)) {
        //    if (block.succ_size() != 2) {
        //        INTERNAL_ERROR << "ERROR: Expected block[" << block.getBlockID()
        //                       << "] to have 2 successors but it has succ_size " <<
        //                       block.succ_size()
        //                       << " terminator StmtClass: `BinaryOperator` \n";
        //        return false;
        //    }
        //    // if BinaryOperator || or && :
        //    // true_branch = short circuit branch,
        //    // false_branch= RHS of the binop

    } else if (const auto *SWITCH = dyn_cast<SwitchStmt>(trm_stmt)) {
        // switch statement needs special handling
        // has as many successor as there are cases (+1 if there is no default case present)

        CFGBlock *default_block = block.succ_end()[-1];
        for (auto it = block.succ_begin(); it != block.succ_end(); it++) {
            const CFGBlock *case_block = *it;
            auto case_label_stmt = case_block->getLabel();
            if (case_label_stmt == nullptr) {
                // this is hit if there is no `default:` in the source code
            } else if (auto cs = dyn_cast<CaseStmt>(case_label_stmt)) {

                auto case_val_expr = cs->getLHS();
                Expr::EvalResult result;
                if (!case_val_expr->EvaluateAsConstantExpr(result, ASTctx)) {
                    // probably not valid c++ and should not happen
                    INTERNAL_ERROR << " the case label is not an constant expression"
                                   << loc_str(case_val_expr) << "\n";
                    return false;
                }
                uint64_t case_val = result.Val.getInt().getExtValue();
                if (!cond_res.val.has_value()) {
                    // TODO: this might be relevant error for the user
                    INTERNAL_ERROR << " Could not determine the value of the switch variable. "
                                      "Cant follow the control flow correctly. Giving up. "
                                   << loc_str(trm_stmt) << "\n";
                    return false;
                }
                if ((uint64_t)cond_res.val.value() == case_val) {
                    return eval_CFG_block(*case_block, state, ASTctx);
                }

            } else if (auto _ = dyn_cast<DefaultStmt>(case_label_stmt)) {
                // do nothing
            } else {
                INTERNAL_ERROR
                    << " case is not CaseStmt or DefaultStmt or null, should be unreachable\n ";
                return false;
            }
        }
        //  Nothing has matched: eval default
        return eval_CFG_block(*default_block, state, ASTctx);
        //    } else if (const auto *BO = dyn_cast<BinaryOperator>(trm_stmt)) {

    } else {
        INTERNAL_ERROR << " TODO: Unhandeled terminatorStmt class " << loc_str(trm_stmt) << " : "
                       << trm_stmt->getStmtClassName() << "\n";
        return false;
    }

    // Should be unreachable
    INTERNAL_ERROR << " Something unhandeled eval_CFG_block fell through\n";
    return false;
}


bool Idx_expr_evaluator::init(const FunctionDecl *func, Stmt *statement_in_the_func,
                              ASTContext *ctx) {
    assert(func != nullptr);
    assert(statement_in_the_func != nullptr);

    CFG::BuildOptions CFG_opts;
    cfg = CFG::buildCFG(func, statement_in_the_func, ctx, CFG_opts);

    if (!cfg) {
        ASTContext &ASTctx = *ctx;
        INTERNAL_ERROR << " Could not build a CFG for statement starting at "
                       << loc_str(statement_in_the_func) << "\n";
        diags << "               : The supplied FunctionDecl is at " << loc_str(func) << "\n";
        return false;
    }
    ASTctx = ctx;
    main_entry_block = &cfg->getEntry();
    main_exit_block = &cfg->getExit();
    main_state.exit_block = main_exit_block;

    diags.enable_colors(true);

    return true;
}

void Idx_expr_evaluator::add_expr_to_tracking_list(const Expr *E) {
    main_state.possible_vals_for_expr[E] = {}; // init as empty set
}

bool Idx_expr_evaluator::exec() {
    assert(main_entry_block != nullptr);
    return eval_CFG_block(*main_entry_block, main_state, *ASTctx);
}

// bool Idx_expr_evaluator::get_possible_vals_for_expr(Expr *E, const
// std::set<std::optional<int64_t>> &vals) const {}

void Idx_expr_evaluator::print_eval_diags(llvm::raw_ostream &os) {
    os << diag_msg;
};

#if 0
// Testing
using namespace clang::ast_matchers;
class FunctionAnalyzer : public MatchFinder::MatchCallback {
    //    const ArraySubscriptExpr *expr_to_track = nullptr;
  public:
    // Testing:
    void run(const MatchFinder::MatchResult &Result) override {
        diags.enable_colors(true);
        ASTContext &ASTctx = *Result.Context;
        Eval_state state;

        // the function we analyse for testing:
        const FunctionDecl *Func = Result.Nodes.getNodeAs<FunctionDecl>("func");
        if (!Func || !Func->hasBody()) return;
        llvm::errs() << "Found function `test`\n";
        auto *FB = Func->getBody();

        // flag the expr we care about
        auto matcher = stmt(hasDescendant(arraySubscriptExpr().bind("arrayAccess")));
        auto Results = match(matcher, *FB, ASTctx);
        if (!Results.empty()) {
            const BoundNodes &Nodes = Results[0];
            const auto *arr_acc = Nodes.getNodeAs<ArraySubscriptExpr>("arrayAccess");
            llvm::outs() << "Found array subscript expr: " << loc_str(arr_acc) << "\n";
        } else {
            llvm::outs() << "No array subscript exprs.... \n";
        }

        CFG::BuildOptions CFG_BO;
        auto CFG = CFG::buildCFG(Func, FB, &ASTctx, CFG_BO);
        auto entry_block = CFG->getEntry();
        auto exit_block = CFG->getExit();
        state.exit_block = &exit_block;
        state.exit_block->dump();

        Eval_state state_print;
        debug_print_CFG_block(entry_block, state_print, ASTctx);

        if (!Results.empty()) {
            auto *arr_acc = Results[0].getNodeAs<ArraySubscriptExpr>("arrayAccess");
            auto idx_expr = arr_acc->getRHS();
            state.possible_vals_for_expr[idx_expr] = {}; // init as empty set
        }

        bool ret = eval_CFG_block(entry_block, state, ASTctx);
        llvm::outs() << "eval_CFG_block " << (ret ? "true" : "false") << "\n";

        llvm::outs() << "------- state.all_possible_vals -------\n";
        for (auto var_vals : state.all_possible_vals) {
            llvm::outs() << loc_str(var_vals.first) << " `" << var_vals.first->getNameAsString()
                         << "` = [ ";
            for (auto val : var_vals.second) { llvm::outs() << val << ", "; }
            llvm::outs() << " ]\n";
        }

        llvm::outs() << "------- state.possible_vals_for_expr -------\n";
        for (auto expr_vals : state.possible_vals_for_expr) {
            llvm::outs() << loc_str(expr_vals.first) << " = [ ";
            for (auto val : expr_vals.second) { llvm::outs() << val << ", "; }
            llvm::outs() << " ]\n";
        }

        llvm::outs() << diag_msg;

    }
};

static cl::OptionCategory LoopVarTrackerOpts("LoopVarTrackerOpts options");

int main(int argc, const char **argv) {
    auto OptionsParser = CommonOptionsParser::create(argc, argv, LoopVarTrackerOpts);
    if (!OptionsParser) { exit(1); }
    auto &optParser = OptionsParser.get();
    ClangTool Tool(optParser.getCompilations(), optParser.getSourcePathList());
    FunctionAnalyzer Analyzer;
    MatchFinder Finder;
    Finder.addMatcher(functionDecl(hasName("test")).bind("func"), &Analyzer);
    return Tool.run(newFrontendActionFactory(&Finder).get());
}
#endif