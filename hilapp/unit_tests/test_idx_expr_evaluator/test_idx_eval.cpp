
// Some unit tests for Idx_expr_evaluator

#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Tooling/Tooling.h"
#include "clang/Tooling/CompilationDatabase.h"

using namespace clang;
using namespace clang::tooling;
using namespace clang::ast_matchers;

#include "tracked_var.h"

#include "../../src/idx_expr_evaluator.h"
#define DIAGS_TO_STR
#include "../../src/idx_expr_evaluator.cpp" // yes this is intentional, diag_msg

/////////////////////////////////////////////////////////////////
// Setup stuff for tests

bool g_print_results = false;
bool g_make_dot_graph = false;
bool g_print_all_possible_vals = false;

class TestFunctionAnalyzer : public MatchFinder::MatchCallback {
  public:
    Idx_expr_evaluator evaluator{};

    std::unordered_map<uintptr_t, std::string> names;

    bool exec_ret;

  public:
    void run(const MatchFinder::MatchResult &Result) override {
        diags.enable_colors(true);
        diag_msg.clear();
        ASTContext &ASTctx = *Result.Context;

        // the function we analyse for testing:
        const FunctionDecl *func = Result.Nodes.getNodeAs<FunctionDecl>("func");
        if (!func || !func->hasBody())
            return;
        auto *FB = func->getBody();

        evaluator = {};
        evaluator.init(func, FB, &ASTctx);

        if (g_make_dot_graph)
            evaluator.cfg->viewCFG(LangOptions());

        exec_ret = evaluator.exec();

        // After ToolInvocation the Compiler context may get freed so store the names now
        // Use the pointers as keys
        for (auto &all_vals : evaluator.main_state.all_possible_vals) {
            names[(uintptr_t)all_vals.first] = all_vals.first->getNameAsString();
        }

        if (g_print_all_possible_vals) {
            llvm::outs() << "------- state.all_possible_vals -------\n";
            for (auto var_vals : evaluator.main_state.all_possible_vals) {
                llvm::outs() << loc_str(var_vals.first) << " `" << var_vals.first->getNameAsString()
                             << "` = [ ";
                for (auto val : var_vals.second) {
                    llvm::outs() << val << ", ";
                }
                llvm::outs() << " ]\n";
            }
        }

        if (!exec_ret) {
            llvm::errs() << diag_msg;
        }
    }
};

#define TEST_EVAL_COMMON_                                                                          \
    std::string fname = "input.cpp";                                                               \
    std::vector<std::string> args = {"clang-tool", "-std=c++17", fname};                           \
    FixedCompilationDatabase Compilations(".", args);                                              \
    IntrusiveRefCntPtr<llvm::vfs::InMemoryFileSystem> InMemoryFS(                                  \
        new llvm::vfs::InMemoryFileSystem());                                                      \
    InMemoryFS->addFile(fname, 0, llvm::MemoryBuffer::getMemBuffer(code));                         \
    FileSystemOptions FileSysOpts;                                                                 \
    FileManager *Files = new FileManager(FileSysOpts, InMemoryFS);                                 \
    TestFunctionAnalyzer test_analyzer;                                                            \
    MatchFinder Finder;                                                                            \
    Finder.addMatcher(functionDecl(hasName("test")).bind("func"), &test_analyzer);                 \
    auto factory = newFrontendActionFactory(&Finder);                                              \
    std::unique_ptr<FrontendAction> action(factory->create());                                     \
    ToolInvocation Invocation(args, std::move(action),                                             \
                              Files); /*this seems to free Files, .. eventhoug a comment says it   \
                                         does not take ownership..*/                               \
    const auto &test_state = test_analyzer.evaluator.main_state;                                   \
    const auto &test_names = test_analyzer.names;                                                  \
    Invocation.run();

// Eval the compound statement given in string
#define TEST_EVAL_STRING_CSTMT(str)                                                                \
    std::string code = "void test() {" str "}";                                                    \
    TEST_EVAL_COMMON_

// Eval the compound statement given in string
#define TEST_EVAL_STRING_TOPLVL(str)                                                               \
    std::string code = str;                                                                        \
    TEST_EVAL_COMMON_

const std::set<std::optional<int64_t>> *
get_possible_values_for_var(std::string var_name, const Eval_state &state,
                            const std::unordered_map<uintptr_t, std::string> &names) {
    for (auto &all_vals : state.all_possible_vals) {
        auto name = names.find((uintptr_t)all_vals.first);
        if (name != names.end()) {
            if (var_name == name->second) {
                return &all_vals.second;
            }
        }
    }
    return nullptr;
}

template <typename T>
inline void print_set(raw_ostream &os, const std::set<T> &set) {
    os << "[ ";
    for (auto val : set) {
        os << val << ", ";
    }
    os << "]\n";
}

template <typename T>
inline bool compare_var_expect(std::string var_name, TrackedVar<T> &var_expected,
                               const Eval_state &state,
                               const std::unordered_map<uintptr_t, std::string> &names) {
    const auto *var_vals = get_possible_values_for_var(var_name, state, names);
    if (!var_vals) {
        llvm::outs() << "Did not find var in test_state with name `" << var_name << "`\n";
        return false;
    }
    // quick hack, convert std::optionals to int
    std::set<T> vars;
    for (auto v : *var_vals) {
        if (!v.has_value()) {
            llvm::outs() << "Found a nullopt in possible values for var `" << var_name
                         << "` in test_state \n";
            return false;
        }
        vars.insert(v.value());
    }

    if (!(vars == var_expected.values)) {
        return false;
    }
    return true;
}

#define print_var_result(VAR)                                                                      \
    do {                                                                                           \
        auto *a_vals = get_possible_values_for_var(VAR, test_state, test_names);                   \
        if (!a_vals) {                                                                             \
            llvm::outs() << "Var result: Did not find var `" VAR "`\n";                            \
        } else {                                                                                   \
            llvm::outs() << "Var result: `" VAR "` = ";                                            \
            print_set(llvm::outs(), *a_vals);                                                      \
        }                                                                                          \
    } while (0)
#define print_var_expect(VAR)                                                                      \
    do {                                                                                           \
        llvm::outs() << "Var expect: `" #VAR "` = ";                                               \
        print_set(llvm::outs(), (VAR).values);                                                     \
    } while (0)

#define COMPARE_AND_SET_TEST_PASS(VAR)                                                             \
    do {                                                                                           \
        if (!compare_var_expect(#VAR, VAR, test_state, test_names)) {                              \
            test_pass = false;                                                                     \
            color_msg(llvm::outs(), RED, "Results differ for var ", true, __FILE__, __LINE__)      \
                << "`" #VAR "`\n";                                                                 \
            print_var_result(#VAR);                                                                \
            print_var_expect(VAR);                                                                 \
        } else if (g_print_results) {                                                              \
            print_var_result(#VAR);                                                                \
            print_var_expect(VAR);                                                                 \
        }                                                                                          \
    } while (0)

#define TEST_CASE_BEGIN bool test_pass = true;

#define TEST_CASE_END                                                                              \
    if (!test_pass) {                                                                              \
        color_msg(llvm::outs(), RED, " TEST FAILED: ", true, __FILE__, __LINE__)                   \
            << __func__ << "\n";                                                                   \
    } else {                                                                                       \
        color_msg(llvm::outs(), GREEN, " TEST PASSED: ", true, __FILE__, __LINE__)                 \
            << __func__ << "\n";                                                                   \
    }                                                                                              \
    return test_pass

/////////////////////////////////////////////////////////////////
// test cases

bool test_basic_if() {
    TEST_CASE_BEGIN;
    /////////////////////////////////////////////
    // Our ast_eval it
    TEST_EVAL_STRING_CSTMT(R"(
    int a = 0;
    int i = 0;
    if( i < 10) a = 10;
    if( i < -10 ) a = -10;
    )")
    /////////////////////////////////////////////
    // clang-format off
    // Really evaluate it 
    TrackedVar<int> a = 0;
    TrackedVar<int> i = 0;
    if( i < 10) a = 10;
    if( i < -10 ) a = -10;
    // clang-format on
    /////////////////////////////////////////////
    // Compare results
    COMPARE_AND_SET_TEST_PASS(a);
    COMPARE_AND_SET_TEST_PASS(i);

    TEST_CASE_END;
}

bool test_basic_if_else() {
    TEST_CASE_BEGIN;
    /////////////////////////////////////////////
    // Our ast_eval it
    TEST_EVAL_STRING_CSTMT(R"(
    int a = 0;
    int i = 0;
    if( i < 10) {a = 10;}
    else        {a = -10;}
    )")
    /////////////////////////////////////////////
    // clang-format off
    // Really evaluate it 
    TrackedVar<int> a = 0;
    TrackedVar<int> i = 0;
    if( i < 10) {a = 10;}
    else        {a = -10;}
    // clang-format on
    /////////////////////////////////////////////
    // Compare results
    COMPARE_AND_SET_TEST_PASS(a);
    COMPARE_AND_SET_TEST_PASS(i);

    TEST_CASE_END;
}

bool test_basic_if_elseifs() {
    TEST_CASE_BEGIN;
    /////////////////////////////////////////////
    // Our ast_eval it
    TEST_EVAL_STRING_CSTMT(R"(
    int a = 0;
    int i = 0;
    if( i < 10) {a = 10;}
    else if(i < -10) {a = -10;}
    else             {a = -999;}
    )")
    /////////////////////////////////////////////
    // clang-format off
    // Really evaluate it 
    TrackedVar<int> a = 0;
    TrackedVar<int> i = 0;
    if( i < 10) {a = 10;}
    else if(i < -10) {a = -10;}
    else             {a = -999;}
    // clang-format on
    /////////////////////////////////////////////
    // Compare results
    COMPARE_AND_SET_TEST_PASS(a);
    COMPARE_AND_SET_TEST_PASS(i);

    TEST_CASE_END;
}

bool test_basic_if_decl() {
    TEST_CASE_BEGIN;
    /////////////////////////////////////////////
    // Our ast_eval it
    TEST_EVAL_STRING_CSTMT(R"(
    int a = 0;
    int i = 0;
    if( int x = 0 ) {a = 10;}
    else {a = 1234;}
    )")
    /////////////////////////////////////////////
    // clang-format off
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-but-set-variable"
// your code for which the warning gets suppressed 
    // Really evaluate it 
    TrackedVar<int> a = 0;
    TrackedVar<int> i = 0;
    if( int x = 0 ) {a = 10;}
    else {a = 1234;}
    // clang-format on
    /////////////////////////////////////////////
#pragma clang diagnostic pop
    // Compare results
    COMPARE_AND_SET_TEST_PASS(a);
    COMPARE_AND_SET_TEST_PASS(i);

    TEST_CASE_END;
}

bool test_basic_for_loop() {
    TEST_CASE_BEGIN;
    /////////////////////////////////////////////
    // Our ast_eval it
    TEST_EVAL_STRING_CSTMT(R"(
    int a = 0;
    int i = -1;
    for(i = 0; i < 10; ++i){
        a += i;
    }
    )")
    /////////////////////////////////////////////
    // clang-format off
    // Really evaluate it 
    TrackedVar<int> a = 0;
    TrackedVar<int> i = -1;
    for(i = 0; i < 10; ++i){
        a += i;
    }
    // clang-format on
    /////////////////////////////////////////////
    // Compare results
    COMPARE_AND_SET_TEST_PASS(a);
    COMPARE_AND_SET_TEST_PASS(i);

    TEST_CASE_END;
}

bool test_basic_for_loop_break() {
    TEST_CASE_BEGIN;
    /////////////////////////////////////////////
    // Our ast_eval it
    TEST_EVAL_STRING_CSTMT(R"(
    int a = 0;
    int i = -1;
    for(i = 0; i < 10; ++i){
        a += i;
        if(i > 5) break;
    }
    )")
    /////////////////////////////////////////////
    // clang-format off
    // Really evaluate it 
    TrackedVar<int> a = 0;
    TrackedVar<int> i = -1;
    for(i = 0; i < 10; ++i){
        a += i;
        if(i > 5) break;
    }
    // clang-format on
    /////////////////////////////////////////////
    // Compare results
    COMPARE_AND_SET_TEST_PASS(a);
    COMPARE_AND_SET_TEST_PASS(i);

    TEST_CASE_END;
}

bool test_basic_for_loop_continue() {
    TEST_CASE_BEGIN;
    /////////////////////////////////////////////
    // Our ast_eval it
    TEST_EVAL_STRING_CSTMT(R"(
    int a = 0;
    int i = -1;
    for(i = 0; i < 10; ++i){
        a += i;
        if(i > 5) continue;
        a++;
    }
    )")
    /////////////////////////////////////////////
    // clang-format off
    // Really evaluate it 
    TrackedVar<int> a = 0;
    TrackedVar<int> i = -1;
    for(i = 0; i < 10; ++i){
        a += i;
        if(i > 5) continue;
        a++;
    }
    // clang-format on
    /////////////////////////////////////////////
    // Compare results
    COMPARE_AND_SET_TEST_PASS(a);
    COMPARE_AND_SET_TEST_PASS(i);

    TEST_CASE_END;
}

bool test_basic_while() {
    TEST_CASE_BEGIN;
    /////////////////////////////////////////////
    // Our ast_eval it
    TEST_EVAL_STRING_CSTMT(R"(
    int a = 1;
    int i = 0;
    while(i < 10){
        a += i + 2;
        ++i;
    }
    )")
    /////////////////////////////////////////////
    // clang-format off
    // Really evaluate it 
    TrackedVar<int> a = 1;
    TrackedVar<int> i = 0;
    while(i < 10){
        a += i + 2;
        ++i;
    }
    // clang-format on
    /////////////////////////////////////////////
    // Compare results
    COMPARE_AND_SET_TEST_PASS(a);
    COMPARE_AND_SET_TEST_PASS(i);

    TEST_CASE_END;
}

bool test_basic_while_break() {
    TEST_CASE_BEGIN;
    /////////////////////////////////////////////
    // Our ast_eval it
    TEST_EVAL_STRING_CSTMT(R"(
    int a = 1;
    int i = 0;
    while(i < 10){
        a += i + 2;
        ++i;
        if(i < 6) break;
    }
    )")
    /////////////////////////////////////////////
    // clang-format off
    // Really evaluate it 
    TrackedVar<int> a = 1;
    TrackedVar<int> i = 0;
    while(i < 10){
        a += i + 2;
        ++i;
        if(i < 6) break;
    }
    // clang-format on
    /////////////////////////////////////////////
    // Compare results
    COMPARE_AND_SET_TEST_PASS(a);
    COMPARE_AND_SET_TEST_PASS(i);

    TEST_CASE_END;
}

bool test_basic_while_continue() {
    TEST_CASE_BEGIN;
    /////////////////////////////////////////////
    // Our ast_eval it
    TEST_EVAL_STRING_CSTMT(R"(
    int a = 1;
    int i = 0;
    while(i < 10){
        a += i + 2;
        ++i;
        if(i < 3) continue;
        a += 100;
    }
    )")
    /////////////////////////////////////////////
    // clang-format off
    // Really evaluate it 
    TrackedVar<int> a = 1;
    TrackedVar<int> i = 0;
    while(i < 10){
        a += i + 2;
        ++i;
        if(i < 3) continue;
        a += 100;
    }
    // clang-format on
    /////////////////////////////////////////////
    // Compare results
    COMPARE_AND_SET_TEST_PASS(a);
    COMPARE_AND_SET_TEST_PASS(i);

    TEST_CASE_END;
}

bool test_basic_do_while() {
    TEST_CASE_BEGIN;
    /////////////////////////////////////////////
    // Our ast_eval it
    TEST_EVAL_STRING_CSTMT(R"(
    int a = 1;
    int i = 0;
    do {
        a *= ++i;
    } while(i < 10);
    )")
    /////////////////////////////////////////////
    // clang-format off
    // Really evaluate it 
    TrackedVar<int> a = 1;
    TrackedVar<int> i = 0;
    do {
        a *= ++i;
    } while(i < 10);
    // clang-format on
    /////////////////////////////////////////////
    // Compare results
    COMPARE_AND_SET_TEST_PASS(a);
    COMPARE_AND_SET_TEST_PASS(i);

    TEST_CASE_END;
}

bool test_basic_do_while_break() {
    TEST_CASE_BEGIN;
    /////////////////////////////////////////////
    // Our ast_eval it
    TEST_EVAL_STRING_CSTMT(R"(
    int a = 1;
    int i = 0;
    do {
        a *= ++i;
        if(i > 4) break;
    } while(i < 10);
    )")
    /////////////////////////////////////////////
    // clang-format off
    // Really evaluate it 
    TrackedVar<int> a = 1;
    TrackedVar<int> i = 0;
    do {
        a *= ++i;
        if(i > 4) break;
    } while(i < 10);
    // clang-format on
    /////////////////////////////////////////////
    // Compare results
    COMPARE_AND_SET_TEST_PASS(a);
    COMPARE_AND_SET_TEST_PASS(i);

    TEST_CASE_END;
}

bool test_basic_do_while_continue() {
    TEST_CASE_BEGIN;
    /////////////////////////////////////////////
    // Our ast_eval it
    TEST_EVAL_STRING_CSTMT(R"(
    int a = 1;
    int i = 0;
    do {
        a *= ++i;
        if(i > 4) continue;
        a += 1;
    } while(i < 10);
    )")
    /////////////////////////////////////////////
    // clang-format off
    // Really evaluate it 
    TrackedVar<int> a = 1;
    TrackedVar<int> i = 0;
    do {
        a *= ++i;
        if(i > 4) continue;
        a += 1;
    } while(i < 10);
    // clang-format on
    /////////////////////////////////////////////
    // Compare results
    COMPARE_AND_SET_TEST_PASS(a);
    COMPARE_AND_SET_TEST_PASS(i);

    TEST_CASE_END;
}

bool test_basic_switch() {
    TEST_CASE_BEGIN;
    /////////////////////////////////////////////
    // Our ast_eval it
    TEST_EVAL_STRING_CSTMT(R"(
    int a = -999;
    int i = 0;
    switch(i){
        case 1: a = 1; break;
        case 2: a = 2; break;
        case 0: a = 0; break;
        default: a = 999; break;
    }
    )")
    /////////////////////////////////////////////
    // clang-format off
    // Really evaluate it 
    TrackedVar<int> a = -999;
    TrackedVar<int> i = 0;
    switch(i){
        case 1: a = 1; break;
        case 2: a = 2; break;
        case 0: a = 0; break;
        default: a = 999; break;
    }
    // clang-format on
    /////////////////////////////////////////////
    // Compare results
    COMPARE_AND_SET_TEST_PASS(a);
    COMPARE_AND_SET_TEST_PASS(i);

    TEST_CASE_END;
}

bool test_basic_switch_no_default() {
    TEST_CASE_BEGIN;
    /////////////////////////////////////////////
    // Our ast_eval it
    TEST_EVAL_STRING_CSTMT(R"(
    int a = -999;
    int i = 0;
    switch(i){
        case 1: a = 1; break;
        case 2: a = 2; break;
        case 0: a = 0; break;
    }
    a = 123;
    )")
    /////////////////////////////////////////////
    // clang-format off
    // Really evaluate it 
    TrackedVar<int> a = -999;
    TrackedVar<int> i = 0;
    switch(i){
        case 1: a = 1; break;
        case 2: a = 2; break;
        case 0: a = 0; break;
    }
    a = 123;
    // clang-format on
    /////////////////////////////////////////////
    // Compare results
    COMPARE_AND_SET_TEST_PASS(a);
    COMPARE_AND_SET_TEST_PASS(i);

    TEST_CASE_END;
}

bool test_basic_switch_no_default_fallthrough() {
    TEST_CASE_BEGIN;
    /////////////////////////////////////////////
    // Our ast_eval it
    TEST_EVAL_STRING_CSTMT(R"(
    int a = -999;
    int i = 0;
    switch(i){
        case 1: a = 1; break;
        case 2: a = 2; break;
    }
    a = 123;
    )")
    /////////////////////////////////////////////
    // clang-format off
    // Really evaluate it 
    TrackedVar<int> a = -999;
    TrackedVar<int> i = 0;
    switch(i){
        case 1: a = 1; break;
        case 2: a = 2; break;
    }
    a = 123;
    // clang-format on
    /////////////////////////////////////////////
    // Compare results
    COMPARE_AND_SET_TEST_PASS(a);
    COMPARE_AND_SET_TEST_PASS(i);

    TEST_CASE_END;
}

bool test_basic_goto() {
    TEST_CASE_BEGIN;
    /////////////////////////////////////////////
    // Our ast_eval it
    TEST_EVAL_STRING_CSTMT(R"(
    int a = 1;
    int i = 0;
    if( i == 0 ) goto test_basic_goto_label_1;
    a++;
test_basic_goto_label_1:
    a = 123;
    )")
    /////////////////////////////////////////////
    // clang-format off
    // Really evaluate it 
    TrackedVar<int> a = 1;
    TrackedVar<int> i = 0;
    if( i == 0 ) goto test_basic_goto_label_1;
    a++;
test_basic_goto_label_1:
    a = 123;
    // clang-format on
    /////////////////////////////////////////////
    // Compare results
    COMPARE_AND_SET_TEST_PASS(a);
    COMPARE_AND_SET_TEST_PASS(i);

    TEST_CASE_END;
}

bool test_basic_LOr() {
    TEST_CASE_BEGIN;
    /////////////////////////////////////////////
    // Our ast_eval it
    TEST_EVAL_STRING_CSTMT(R"(
    int a = 11;
    int i = 0;
    int c = 0;
    a = i || ++i;
    if(a) c++;
    a = i || ++i;
    if(a) c++;
    )")
    /////////////////////////////////////////////
    // clang-format off
    // Really evaluate it 
    TrackedVar<int> a = 11;
    TrackedVar<int> i = 0;
    TrackedVar<int> c = 0;
    a = i || ++i;
    if(a) c++;
    a = i || ++i;
    if(a) c++;
    // clang-format on
    /////////////////////////////////////////////
    // Compare results
    COMPARE_AND_SET_TEST_PASS(a);
    COMPARE_AND_SET_TEST_PASS(i);
    COMPARE_AND_SET_TEST_PASS(c);

    TEST_CASE_END;
}

bool test_basic_LAnd() {
    TEST_CASE_BEGIN;
    /////////////////////////////////////////////
    // Our ast_eval it
    TEST_EVAL_STRING_CSTMT(R"(
    int a = 123;
    int i = 0;
    int c = 0;
    a = (++i && ++i);
    if(a) c++;
    a = ( (i-2) && ++i);
    if(a) c++;
    )")
    /////////////////////////////////////////////
    // clang-format off
    // Really evaluate it 
    TrackedVar<int> a = 123;
    TrackedVar<int> i = 0;
    TrackedVar<int> c = 0;
    a = (++i && ++i);
    if(a) c++;
    a = ( (i-2) && ++i);
    if(a) c++;
    // clang-format on
    /////////////////////////////////////////////
    // Compare results
    COMPARE_AND_SET_TEST_PASS(a);
    COMPARE_AND_SET_TEST_PASS(i);
    COMPARE_AND_SET_TEST_PASS(c);

    TEST_CASE_END;
}

bool test_basic_LAnd_LOr() {
    TEST_CASE_BEGIN;
    /////////////////////////////////////////////
    // Our ast_eval it
    TEST_EVAL_STRING_CSTMT(R"(
    int x = 123;
    int a = 10;
    int i = 0;
    int c = 0;
    a = ( !(x && ++i) || (++i && a) ) && ++i;
    if(a) c++;
    a = (x++ && (x-123)) && ( (x=0) && ++i );
    if(a) c++;
    )")
    /////////////////////////////////////////////
    // clang-format off
    // Really evaluate it 
    TrackedVar<int> x = 123;
    TrackedVar<int> a = 10;
    TrackedVar<int> i = 0;
    TrackedVar<int> c = 0;
    a = ( !(x && ++i) || (++i && a) ) && ++i;
    if(a) c++;
    a = (x++ && (x-123)) && ( (x=0) && ++i );
    if(a) c++;
    // clang-format on
    /////////////////////////////////////////////
    // Compare results
    COMPARE_AND_SET_TEST_PASS(x);
    COMPARE_AND_SET_TEST_PASS(a);
    COMPARE_AND_SET_TEST_PASS(i);
    COMPARE_AND_SET_TEST_PASS(c);

    TEST_CASE_END;
}


bool test_basic_ternary() {
    TEST_CASE_BEGIN;
    /////////////////////////////////////////////
    // Our ast_eval it
    TEST_EVAL_STRING_CSTMT(R"(
    int x = 0;
    int y = 0;
    int a = x ? y++ : y--;
    a = (x+1) ? ++x + 1 : --x - 1;
    )")
    /////////////////////////////////////////////
    // clang-format off
    // Really evaluate it 
    TrackedVar<int> x = 0;
    TrackedVar<int> y = 0;
    TrackedVar<int> a = x ? y++ : y--;
    a = (x+1) ? ++x + 1 : --x - 1;
    // clang-format on
    /////////////////////////////////////////////
    // Compare results
    COMPARE_AND_SET_TEST_PASS(x);
    COMPARE_AND_SET_TEST_PASS(y);
    COMPARE_AND_SET_TEST_PASS(a);

    TEST_CASE_END;
}

bool test_lots_of_control_flow_things_1() {
    TEST_CASE_BEGIN;
    /////////////////////////////////////////////
    // Our ast_eval it
    TEST_EVAL_STRING_CSTMT(R"(
    int i = 0;
    int sum = 0;
    int i2 = 0;
    for(i = 1; i < 100; ++i){
        if(i % 2 == 0) sum += i;
        if(sum == 3){
            while(i2 < 10 + i){
                sum -= i2;
                if(i2 > 8) break;
                i2++;
            }
        }
        if(i%3 == 0) continue;
        else sum += 1;
        sum += 1;
    }
    )")
    /////////////////////////////////////////////
    // clang-format off
    // Really evaluate it 
    TrackedVar<int> i = 0;
    TrackedVar<int> sum = 0;
    TrackedVar<int> i2 = 0;
    for(i = 1; i < 100; ++i){
        if(i % 2 == 0) sum += i;
        if(sum == 3){
            while(i2 < 10 + i){
                sum -= i2;
                if(i2 > 8) break;
                i2++;
            }
        }
        if(i%3 == 0) continue;
        else sum += 1;
        sum += 1;
    }
    // clang-format on
    /////////////////////////////////////////////
    // Compare results
    COMPARE_AND_SET_TEST_PASS(i);
    COMPARE_AND_SET_TEST_PASS(sum);
    COMPARE_AND_SET_TEST_PASS(i2);

    TEST_CASE_END;
}

bool test_lots_of_control_flow_things_2() {
    TEST_CASE_BEGIN;
    /////////////////////////////////////////////
    // Our ast_eval it
    TEST_EVAL_STRING_CSTMT(R"(
    int i = 0;
    int sum = 0;
    int i2 = 0;
    for(i = 1; i < 100; ++i){
        if(i % 2 == 0 || i % 3 == 0) sum += i;
        if(sum == 3){
            while(i2 < 10 + i){
                sum -= i2;
                if(i2 > 8 && i > 10) break;
                i2++;
            }
        }
        if(i%3 == 0) continue;
        else sum += 1;
        sum += 1;
        if(sum > 2000 && i%5==0) sum += 10000;
    }
    )")
    /////////////////////////////////////////////
    // clang-format off
    // Really evaluate it 
    TrackedVar<int> i = 0;
    TrackedVar<int> sum = 0;
    TrackedVar<int> i2 = 0;
    for(i = 1; i < 100; ++i){
        if(i % 2 == 0 || i % 3 == 0) sum += i;
        if(sum == 3){
            while(i2 < 10 + i){
                sum -= i2;
                if(i2 > 8 && i > 10) break;
                i2++;
            }
        }
        if(i%3 == 0) continue;
        else sum += 1;
        sum += 1;
        if(sum > 2000 && i%5==0) sum += 10000;
    }
    // clang-format on
    /////////////////////////////////////////////
    // Compare results
    COMPARE_AND_SET_TEST_PASS(i);
    COMPARE_AND_SET_TEST_PASS(sum);
    COMPARE_AND_SET_TEST_PASS(i2);

    TEST_CASE_END;
}

bool test_constexpr_func() {
    TEST_CASE_BEGIN;
    /////////////////////////////////////////////
    // Our ast_eval it
    TEST_EVAL_STRING_TOPLVL(R"(
constexpr int func1(int a){
    return a + 2;
}

void test(){
    int a = 1;
    int i = 0;
    for(i = 0; i < 10; ++i){
        a = func1(a);
    }
}
    )")
    /////////////////////////////////////////////
    // clang-format off
    // Really evaluate it 
// use a lambda so we can keep it contained in the test_constexpr_func() ...
// constexpr int func1(int a){
constexpr auto func1 = [](int a){
    return a + 2;
};
//void test(){
    TrackedVar<int> a = 1;
    TrackedVar<int> i = 0;
    for(i = 0; i < 10; ++i){
        a = func1(a);
    }
//}
    // clang-format on
    /////////////////////////////////////////////
    // Compare results
    COMPARE_AND_SET_TEST_PASS(a);
    COMPARE_AND_SET_TEST_PASS(i);

    TEST_CASE_END;
}

////////////////////////////////////////////////////////
// test things that should fail exec

bool test_should_fail_1() {
    TEST_CASE_BEGIN;

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-variable"
    /////////////////////////////////////////////
    // Our ast_eval it
    TEST_EVAL_STRING_TOPLVL(R"(

void test(int unknown){
    int a = 0;
    for(int i = 0; i < unknown; ++i){
        a++;
    }
}
    )")
#pragma clang diagnostic pop

    if (test_analyzer.exec_ret)
        test_pass = false;

    TEST_CASE_END;
}

////////////////////////////////////////////////////////
// test things that should succeed to exec but fails to track some value

// TODO

typedef bool (*test_func)();
static std::vector<test_func> test_cases = {
    // clang-format off
    // for
    test_basic_for_loop,
    test_basic_for_loop_break,
    test_basic_for_loop_continue,

    // if
    test_basic_if,
    test_basic_if_else,
    test_basic_if_elseifs,
    test_basic_if_decl,

    // while
    test_basic_while,
    test_basic_while_break,
    test_basic_while_continue,

    // do while
    test_basic_do_while,
    test_basic_do_while_break,
    test_basic_do_while_continue,

    // switch
    test_basic_switch,
    test_basic_switch_no_default,
    test_basic_switch_no_default_fallthrough,

    // goto
    test_basic_goto,

    // LOr, LAnd
    test_basic_LOr,
    test_basic_LAnd,
    test_basic_LAnd_LOr,

    // ternary
    test_basic_ternary,

    //
    test_lots_of_control_flow_things_1,
    test_lots_of_control_flow_things_2,


    // calling constexpr functions
    test_constexpr_func,
    
    // should fail 
    test_should_fail_1,

    // weird stuff
    // test_Lor_in_switch,
    // clang-format on
};

#if TEST_IDX_EVAL
// Run all the test cases above
int main(int argc, char **argv) {
    (void)argv; // Unused
    if (argc > 1)
        g_print_results = true;

    int n_failed = 0;
    for (auto test_case : test_cases) {
        if (!test_case())
            n_failed++;
    }

    if (n_failed != 0) {
        llvm::outs() << "Test statistics: " << n_failed << " / " << test_cases.size();
        color_msg(llvm::outs(), RED, " test cases failed.\n");
    } else {
        llvm::outs() << "Test statistics: " << n_failed << " / " << test_cases.size()
                     << " test cases failed.\n";
        color_msg(llvm::outs(), GREEN, " All test cases passed.\n");
    }

    return n_failed;
}

#elif IDX_EVAL_FILE
// For manual testing.
// Runs the idx_expr_evaluator on an input file and analyse a function with name `test`.

#include "clang/Tooling/CommonOptionsParser.h"
static cl::OptionCategory LoopVarTrackerOpts("LoopVarTrackerOpts options");

static cl::opt<bool> make_dot_graph(
    "dot",                                  // The flag name
    cl::desc("Make .dot graph from CFG."),  // Description for --help
    cl::init(false),                        // Default value
    cl::cat(LoopVarTrackerOpts)             // Associate with your category
);

int main(int argc, const char **argv) {
    auto OptionsParser = CommonOptionsParser::create(argc, argv, LoopVarTrackerOpts);
    if (!OptionsParser) {
        exit(1);
    }
    auto &optParser = OptionsParser.get();
    ClangTool Tool(optParser.getCompilations(), optParser.getSourcePathList());

    TestFunctionAnalyzer Analyzer;
    MatchFinder Finder;

    Finder.addMatcher(functionDecl(hasName("test")).bind("func"), &Analyzer);

    g_print_results = false;
    g_print_all_possible_vals = true;
    if(make_dot_graph) g_make_dot_graph = true;

    return Tool.run(newFrontendActionFactory(&Finder).get());
}
#endif