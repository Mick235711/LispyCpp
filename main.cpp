#include <iostream>
#include <cstdlib>
#include <cstring>
#include <string>
#include <utility>
#include <stdexcept>
#include <vector>
#include <variant>
#include <map>
#include <functional>

// editline
#include <editline/readline.h>

// mpc
#include "mpc.h"

using namespace std::literals;
using std::cin;
using std::cout;
using std::endl;
using std::string;
using std::unique_ptr;
using std::shared_ptr;
using std::make_unique;
using std::make_shared;
using std::move;
using std::exception;
using std::size_t;
using std::get;
using std::in_place_index;
using std::ostream;
using std::to_string;
using std::map;
using std::vector;

// lval ASSERT
#define LASSERT(cond, msg) \
    if (!(cond)) return lval(msg, l_err)
#define LASSERT_NUM(func, value, expect) \
    LASSERT(value.cell().size() == expect, "Function '"s + func + "' passed wrong number of arguments. " +\
                                           "Expected " + to_string(expect) + ", got " + to_string(value.cell().size()))
#define LASSERT_TYPE(func, value, argnum, expect, expstring) \
    LASSERT(value.type() == expect, "Function '"s + func + "' passed incorrect type in argument " + to_string(argnum) +\
                                    ". Expected " + expstring + ", got " + to_string(value))
#define LASSERT_EMPTY(func, value) \
    LASSERT(!value.cell().empty(), "Function '"s + func + "' passed {}.")

// mpc_parser_t deleter
// uses mpc_cleanup
struct mpc_parser_deleter
{
public:
    void operator()(mpc_parser_t* p) const noexcept
    {
        mpc_cleanup(1, p);
    }
};

// mpc_ast_t deleter
// uses mpc_ast_delete
struct mpc_ast_deleter
{
public:
    void operator()(mpc_ast_t* p) const noexcept
    {
        mpc_ast_delete(p);
    }
};

// mpc_err_t deleter
// uses mpc_err_delete
struct mpc_err_deleter
{
public:
    void operator()(mpc_err_t* p) const noexcept
    {
        mpc_err_delete(p);
    }
};

// dummy to_string
string to_string(const string& s) noexcept
{
    return s;
}

// Lispy value
struct lval;

// Lispy environment
using lenv_t = map<string, lval>;
struct lenv
{
    shared_ptr<lenv> parent;
    lenv_t env;
};

// Lispy lambda
struct lambda_t;

// unique_ptr declarations
using mpc_pt = unique_ptr<mpc_parser_t, mpc_parser_deleter>;
using mpc_at = unique_ptr<mpc_ast_t, mpc_ast_deleter>;
using mpc_et = unique_ptr<mpc_err_t, mpc_err_deleter>;

// map of builtin functions
using builtin_t = std::function<lval(lenv&, const lval&)>;
using mp_builtin_t = map<string, builtin_t>;

// integer type
using LL = long long;

// float type
using D = double;

// unused tag types
struct lval_err_t {}; const lval_err_t l_err;
struct lval_sym_t {}; const lval_sym_t l_sym;
struct lval_s_t {}; const lval_s_t l_sexpr;
struct lval_q_t {}; const lval_q_t l_qexpr;

// Lispy lambda
struct lambda_t
{
    builtin_t fun;
    lenv env;
    shared_ptr<lval> formals;
    shared_ptr<lval> body;
    lambda_t(const lval& f, const lval& b, const builtin_t& fu = builtin_t(), const lenv& e = lenv()) noexcept;
    lambda_t(const lambda_t&) noexcept;
    lambda_t& operator=(const lambda_t&) noexcept;
    lambda_t(lambda_t&&) noexcept = default;
    lambda_t& operator=(lambda_t&&) noexcept = default;
};

// Lispy value
struct lval
{
public:
    // Enum and type declarations
    enum {LVAL_NUM = 0, LVAL_ERR, LVAL_SYM, LVAL_STR, LVAL_SEXPR, LVAL_QEXPR, LVAL_FUN, LVAL_LAMBDA}; // lval types
    typedef vector<lval> cell_t;

private:
    // Member declarations
    std::variant<LL, string, string, string, cell_t, cell_t, builtin_t, lambda_t> va;

private:
    // Helper functions
    // got cell by S-Expr or Q-Expr
    const cell_t& cell_in() const // noexcept(false)
    {
        try
        {
            return get<LVAL_SEXPR>(va);
        }
        catch (std::bad_variant_access&)
        {
            return get<LVAL_QEXPR>(va);
        }
    }
    cell_t& cell_in() // noexcept(false)
    {
        return const_cast<cell_t&>(static_cast<const lval&>(*this).cell_in());
    }

public:
    // Member accessing (others are all noexcept(false))
    const size_t type() const noexcept {return va.index();}
    LL& num() {return get<LVAL_NUM>(va);}
    const LL num() const {return get<LVAL_NUM>(va);}
    string& err() {return get<LVAL_ERR>(va);}
    const string& err() const {return get<LVAL_ERR>(va);}
    string& sym() {return get<LVAL_SYM>(va);}
    const string& sym() const {return get<LVAL_SYM>(va);}
    string& str() {return get<LVAL_STR>(va);}
    const string& str() const {return get<LVAL_STR>(va);}
    cell_t& cell() {return cell_in();}
    const cell_t& cell() const {return cell_in();}
    builtin_t& fun() {return get<LVAL_FUN>(va);}
    const builtin_t& fun() const {return get<LVAL_FUN>(va);}
    lambda_t& lambda() {return get<LVAL_LAMBDA>(va);}
    const lambda_t& lambda() const {return get<LVAL_LAMBDA>(va);}

public:
    // Assistance functions
    // read a number from AST
    static lval read_num(mpc_ast_t* a) noexcept
    {
        try
        {
            return lval(stoll(string(a->contents)));
        }
        catch (exception&)
        {
            return lval("Invalid number "s + a->contents, l_err);
        }
    }
    // read a string from AST
    static lval read_str(mpc_ast_t* a) noexcept
    {
        // Cut the first and final "
        a->contents[strlen(a->contents) - 1] = '\0';
        auto str = static_cast<char*>(malloc(strlen(a->contents + 1) + 1));
        strcpy(str, a->contents + 1);
        str = static_cast<char*>(mpcf_unescape(static_cast<mpc_val_t*>(str)));
        lval res{str};
        free(str);
        return res;
    }
    // add a lval to the list
    void add(const lval& other) // noexcept(false)
    {
        cell_in().push_back(other);
    }
    // print S-Expr
    void expr_print(ostream& out, char start = '(', char end = ')', char delim = ' ') const // noexcept(false)
    {
        out << start;
        for (size_t i = 0; i < cell_in().size(); ++i)
        {
            // print each value
            out << cell_in()[i];

            // prevent trailing space
            if (i != cell_in().size() - 1)
                out << delim;
        }
        out << end;
    }
    // pop an element
    lval pop(cell_t::const_iterator cit) // noexcept(false)
    {
        lval res = *cit;
        cell_in().erase(cit);
        return res;
    }

public:
    // Constructors
    // default constructor
    lval() noexcept :lval(l_sexpr) {}
    // create a value lval
    explicit lval(LL n) noexcept :va(n) {}
    // create an error lval
    lval(const string& e, lval_err_t) noexcept :va(in_place_index<LVAL_ERR>, e) {}
    // create a symbol lval
    lval(const string& s, lval_sym_t) noexcept :va(in_place_index<LVAL_SYM>, s) {}
    // create a string lval
    explicit lval(const string& s) noexcept :va(in_place_index<LVAL_STR>, s) {}
    // create a S-Expr lval
    explicit lval(lval_s_t, const cell_t& c = cell_t()) noexcept :va(in_place_index<LVAL_SEXPR>, c) {}
    // create a Q-Expr lval
    explicit lval(lval_q_t, const cell_t& c = cell_t()) noexcept :va(in_place_index<LVAL_QEXPR>, c) {}
    // create a function lval
    explicit lval(const builtin_t& func) noexcept :va(func) {}
    // create a lambda lval
    lval(const lval& f, const lval& b) noexcept :va(lambda_t(f, b)) {}
    // create lval from an AST
    explicit lval(mpc_ast_t* a) noexcept
    {
        // symbol, number or string: defer to the constructor
        if (string(a->tag).find("number") != string::npos) *this = read_num(a);
        if (string(a->tag).find("symbol") != string::npos) *this = lval(a->contents, l_sym);
        if (string(a->tag).find("string") != string::npos) *this = read_str(a);

        // root (>) or S-Expr: create empty list
        if (a->tag == ">"s || string(a->tag).find("sexpr") != string::npos) *this = lval(l_sexpr);
        // Q-Expr: create empty list
        if (string(a->tag).find("qexpr") != string::npos) *this = lval(l_qexpr);

        // Fill the list
        for (int i = 0; i < a->children_num; ++i)
        {
            auto child = a->children[i];
            if (child->contents == "("s || child->contents == ")"s || // S-Expr
                child->contents == "{"s || child->contents == "}"s || // Q-Expr
                string(child->tag).find("comment") != string::npos ||
                child->tag == "regex"s) continue;
            add(lval(child));
        }
    }
    
public:
    // Operators
    friend ostream& operator<<(ostream&, const lval&) noexcept;
    friend bool operator==(const lval&, const lval&) noexcept;
    friend bool operator!=(const lval&, const lval&) noexcept;
};

// lambda constructor
lambda_t::lambda_t(const lval& f, const lval& b, const builtin_t& fu, const lenv& e) noexcept
:formals(make_shared<lval>(f)), body(make_shared<lval>(b)), fun(fu), env(e)
{}

// lambda copy member
lambda_t::lambda_t(const lambda_t& other) noexcept
:fun(other.fun), env(other.env),
 formals(other.formals ? make_shared<lval>(*other.formals) : nullptr),
 body(other.body ? make_shared<lval>(*other.body) : nullptr)
{}
lambda_t& lambda_t::operator=(const lambda_t& other) noexcept
{
    fun = other.fun; env = other.env;
    if (other.formals) formals = make_shared<lval>(*other.formals);
    if (other.body) body = make_shared<lval>(*other.body);
    return *this;
}

// lambda operators
bool operator==(const lambda_t& lhs, const lambda_t& rhs) noexcept
{
    return *lhs.formals == *rhs.formals && *lhs.body == *rhs.body;
}
bool operator!=(const lambda_t& lhs, const lambda_t& rhs) noexcept
{
    return !(lhs == rhs);
}

// lval operators
bool operator==(const lval& lhs, const lval& rhs) noexcept
{
    if (lhs.type() != rhs.type()) return false;
    switch (lhs.type())
    {
        case lval::LVAL_NUM:
            return lhs.num() == rhs.num();
        case lval::LVAL_ERR:
            return lhs.err() == rhs.err();
        case lval::LVAL_SYM:
            return lhs.sym() == rhs.sym();
        case lval::LVAL_STR:
            return lhs.str() == rhs.str();
        case lval::LVAL_QEXPR:
        case lval::LVAL_SEXPR:
            return lhs.cell() == rhs.cell();
        case lval::LVAL_FUN:
            return lhs.fun().target<lval(lenv &,const lval&)>() ==
                rhs.fun().target<lval(lenv &,const lval&)>();
        case lval::LVAL_LAMBDA:
            return lhs.lambda() == rhs.lambda();
        default:
            return false;
    }
}
bool operator!=(const lval& lhs, const lval& rhs) noexcept
{
    return !(lhs == rhs);
}

// Turn a lval into a string
// @param v = the lval
// @return the corresponding string
string to_string(const lval& v) noexcept
{
    switch (v.type())
    {
        case lval::LVAL_NUM: return "Number";
        case lval::LVAL_ERR: return "Error";
        case lval::LVAL_SYM: return "Symbol";
        case lval::LVAL_STR: return "String";
        case lval::LVAL_QEXPR: return "Q-Expression";
        case lval::LVAL_SEXPR: return "S-Expression";
        case lval::LVAL_FUN: return "Function";
        case lval::LVAL_LAMBDA: return "Lambda";
        default: return "Unknown";
    }
}

// Output a lval
// @param out = output stream
// @param v = the lispy value
// @return stream after output
ostream& operator<<(ostream& out, const lval& v) noexcept
{
    char* str = nullptr;
    switch (v.type())
    {
        case lval::LVAL_NUM:
            // v is a number, print it
            out << v.num();
            break;

        case lval::LVAL_ERR:
            // v is an error, print it
            out << "Lispy error: " << v.err();
            break;

        case lval::LVAL_SYM:
            // v is a symbol, print it
            out << v.sym();
            break;

        case lval::LVAL_STR:
            // v is a string, print it
            str = static_cast<char*>(malloc(v.str().size() + 1));
            strcpy(str, v.str().c_str());
            str = static_cast<char*>(mpcf_escape(static_cast<mpc_val_t*>(str)));
            out << "\"" << str << "\"";
            free(str);
            break;

        case lval::LVAL_SEXPR:
            // v is a S-Expr, print it
            v.expr_print(out);
            break;

        case lval::LVAL_QEXPR:
            // v is a Q-Expr, print it
            v.expr_print(out, '{', '}');
            break;

        case lval::LVAL_FUN:
            // v is a Function, print it
            out << "<builtin function>";
            break;

        case lval::LVAL_LAMBDA:
            // v is a Lambda, print it
            out << "(\\ " << *v.lambda().formals << ' ' << *v.lambda().body << ')';
            break;

        //default: break;
    }
    return out;
}

// Get a value from lenv
// @param e = the lenv object
// @param k = the required key
// @return the corresponding value
lval get(const lenv& e, const lval& k) // noexcept(false)
{
    auto cit = e.env.find(k.sym());
    if (cit != e.env.cend())
        return cit->second;
    if (e.parent) return get(*e.parent, k);
    return lval("Unknown symbol " + k.sym(), l_err);
}

// Put a key-value pair into lenv
// @param e = the lenv object
// @param k = key, v = value
void put(lenv& e, const lval& k, const lval& v) // noexcept(false)
{
    e.env[k.sym()] = v;
}

// Define a key-value pair into global scope
// @param e = the lenv object
// @param k = key, v = value
void def(lenv& e, const lval& k, const lval& v) // noexcept(false)
{
    if (!e.parent)
    {
        put(e, k, v);
        return;
    }
    auto p = e.parent;
    while (p->parent) p = p->parent;
    put(*p, k, v);
}

// Create a mpc parser
// @param name = parser name
auto make_parser(const string& name) noexcept
{
    return mpc_pt(mpc_new(name.c_str()));
}

// Evaluate operators: +(unary/binary) -(unary/binary) * /
lval builtin_op(lenv&, const lval& v, const string& op) // noexcept(false)
{
    // Ensure all arguments are number
    for (size_t i = 0; i < v.cell().size(); ++i)
    {
        const auto& a = v.cell()[i];
        LASSERT_TYPE(op, a, "1." + to_string(i + 1), lval::LVAL_NUM, "Number");
    }

    // pop the first element
    lval res = v;
    lval lhs = res.pop(res.cell().cbegin());

    // no other arguments: unary +/-
    if (res.cell().empty() && op == "-")
        lhs.num() = -lhs.num();

    // Process other arguments
    while (!res.cell().empty())
    {
        // pop next element
        lval rhs = res.pop(res.cell().cbegin());

        // process operation
        if (op == "+") lhs.num() += rhs.num();
        if (op == "-") lhs.num() -= rhs.num();
        if (op == "*") lhs.num() *= rhs.num();
        if (op == "/") // Detecting division by zero
        {
            if (rhs.num() == 0)
            {
                lhs = lval("Division by zero!", l_err);
                break;
            }
            lhs.num() /= rhs.num();
        }
        if (op == "%") // Detecting modulo by zero
        {
            if (rhs.num() == 0)
            {
                lhs = lval("Modulo by zero!", l_err);
                break;
            }
            lhs.num() %= rhs.num();
        }
    }

    return lhs;
}

// builtin function head
// take the first element of a Q-Expr
lval builtin_head(lenv&, const lval& v) // noexcept(false)
{
    // Check error conditions
    LASSERT_NUM("head", v, 1);
    LASSERT_TYPE("head", v.cell()[0], 1, lval::LVAL_QEXPR, "Q-Expression");
    LASSERT_EMPTY("head", v.cell()[0]);

    // Take the first element
    lval res = v.cell()[0];
    while (res.cell().size() > 1)
        res.cell().pop_back();
    return res;
}

// builtin function tail
// remove the first element of a Q-Expr
lval builtin_tail(lenv&, const lval& v) // noexcept(false)
{
    // Check error conditions
    LASSERT_NUM("tail", v, 1);
    LASSERT_TYPE("tail", v.cell()[0], 1, lval::LVAL_QEXPR, "Q-Expression");
    LASSERT_EMPTY("tail", v.cell()[0]);

    // Remove the first element
    lval res = v.cell()[0];
    res.pop(res.cell().cbegin());
    return res;
}

// builtin function list
// convert S-Expression to Q-Expression
lval builtin_list(lenv&, const lval& v) // noexcept(false)
{
    lval res = lval(l_qexpr, v.cell());
    return res;
}

lval eval(lenv&, const lval&);

// builtin function eval
// convert Q-Expression to S-Expression
lval builtin_eval(lenv& e, const lval& v) // noexcept(false)
{
    // Check error conditions
    LASSERT_NUM("eval", v, 1);
    LASSERT_TYPE("eval", v.cell()[0], 1, lval::LVAL_QEXPR, "Q-Expression");

    // Do the convert
    lval res = lval(l_sexpr, v.cell()[0].cell());
    return eval(e, res);
}

// Join two lval
lval join(lval& x, const lval& y) // noexcept(false)
{
    // from each cell of y add to x
    lval tx = x, ty = y;
    while (!ty.cell().empty())
        tx.add(ty.pop(ty.cell().cbegin()));
    return tx;
}

// builtin function join
// join several Q-Expressions
lval builtin_join(lenv&, const lval& v) // noexcept(false)
{
    // Check error conditions
    for (size_t i = 0; i < v.cell().size(); ++i)
    {
        const auto& a = v.cell()[i];
        LASSERT_TYPE("join", a, i + 1, lval::LVAL_QEXPR, "Q-Expression");
    }

    // Join all the sub-expressions
    lval res = v;
    lval x = res.pop(res.cell().cbegin());
    while (!res.cell().empty())
        x = join(x, res.pop(res.cell().cbegin()));

    return x;
}

// builtin function lambda(\)
// make a lambda lval
lval builtin_lambda(lenv& e, const lval& v) // noexcept(false)
{
    // Check error conditions
    LASSERT_NUM("lambda", v, 2);
    LASSERT_TYPE("lambda", v.cell()[0], 1, lval::LVAL_QEXPR, "Q-Expression");
    LASSERT_TYPE("lambda", v.cell()[1], 2, lval::LVAL_QEXPR, "Q-Expression");

    // Fetch symbol list
    const lval& syms = v.cell()[0];

    // Ensure syms is a list of symbols
    for (size_t i = 0; i < syms.cell().size(); ++i)
    {
        const auto& a = syms.cell()[i];
        LASSERT_TYPE("lambda", a, "1." + to_string(i + 1), lval::LVAL_SYM, "Symbol");
    }

    return lval(syms, v.cell()[1]);
}

// builtin function var (def/=)
lval builtin_var(lenv& e, const lval& v, const string& func) // noexcept(false)
{
    // Fetch symbol list
    const lval& syms = v.cell()[0];
    LASSERT_TYPE(func, syms, 1, lval::LVAL_QEXPR, "Q-Expression");

    // Ensure syms is a list of symbols
    for (size_t i = 0; i < syms.cell().size(); ++i)
    {
        const auto& a = syms.cell()[i];
        LASSERT_TYPE(func, a, "1." + to_string(i + 1), lval::LVAL_SYM, "Symbol");
    }

    // Ensure symbols and values are paired
    LASSERT(v.cell().size() == syms.cell().size() + 1, "Function '"s + func + "' cannot define unpaired symbol-values. " +
                                                       "There are " + to_string(v.cell().size() - 1) + " value(s), " +
                                                       "but there are " + to_string(syms.cell().size()) + " symbol(s).");

    // Assign the symbols
    for (size_t i = 0; i < syms.cell().size(); ++i)
        if (func == "def")
            def(e, syms.cell()[i], v.cell()[i + 1]);
        else put(e, syms.cell()[i], v.cell()[i + 1]);

    return lval();
}

// builtin function ord (</>/<=/>=)
lval builtin_ord(lenv&, const lval& v, const string& func) // noexcept(false)
{
    // Check error conditions
    LASSERT_NUM(func, v, 2);
    LASSERT_TYPE(func, v.cell()[0], 1, lval::LVAL_NUM, "Number");
    LASSERT_TYPE(func, v.cell()[1], 2, lval::LVAL_NUM, "Number");

    // Do the arithmetic comparision
    bool result = false;
    int lhs = v.cell()[0].num(), rhs = v.cell()[1].num();
    if (func == ">") result = lhs > rhs;
    if (func == "<") result = lhs < rhs;
    if (func == ">=") result = lhs >= rhs;
    if (func == "<=") result = lhs <= rhs;
    return lval(result);
}

// builtin function cmp(==/!=)
lval builtin_cmp(lenv&, const lval& v, const string& func) // noexcept(false)
{
    // Check error conditions
    LASSERT_NUM(func, v, 2);

    // Do the comparision
    bool result = false;
    lval lhs = v.cell()[0], rhs = v.cell()[1];
    if (func == "==") result = lhs == rhs;
    if (func == "!=") result = lhs != rhs;
    return lval(result);
}

// builtin function if
// simulate the if statement
lval builtin_if(lenv& e, const lval& v) // noexcept(false)
{
    // Check error conditions
    LASSERT_NUM("if", v, 3);
    LASSERT_TYPE("if", v.cell()[0], 1, lval::LVAL_NUM, "Number");
    LASSERT_TYPE("if", v.cell()[1], 2, lval::LVAL_QEXPR, "Q-Expression");
    LASSERT_TYPE("if", v.cell()[2], 3, lval::LVAL_QEXPR, "Q-Expression");

    // Get two expressions and change type
    lval lhs{l_sexpr, v.cell()[1].cell()}, rhs{l_sexpr, v.cell()[2].cell()};

    // Do the calculation
    lval res;
    if (v.cell()[0].num())
        res = eval(e, lhs);
    else res = eval(e, rhs);
    return res;
}

// builtin function load
// load a file and evaluate
lval builtin_load(lenv& e, const lval& v, mpc_pt& p) // noexcept(false)
{
    LASSERT_NUM("load", v, 1);
    LASSERT_TYPE("load", v.cell()[0], 1, lval::LVAL_STR, "String");

    // Parse the file
    mpc_result_t r;
    if (mpc_parse_contents(v.cell()[0].str().c_str(), p.get(), &r))
    {
        // Success. Read contents.
        auto a = mpc_at(static_cast<mpc_ast_t*>(r.output));
        lval expr{a.get()};

        // Evaluate each expression
        for (const auto& l : expr.cell())
        {
            lval x = eval(e, l);
            if (x.type() == lval::LVAL_ERR)
                cout << x << '\n';
        }

        return lval();
    }
    else
    {
        // Error
        auto err = mpc_et(r.error);
        string msg = mpc_err_string(err.get());
        return lval("Could not load file : " + msg, l_err);
    }
}

// builtin function print
// print a lval from files
lval builtin_print(lenv& e, const lval& v) // noexcept(false)
{
    // Print the arguments
    for (const auto& a : v.cell())
        cout << a << ' ';
    cout << '\n';
    return lval();
}

// builtin function error
// show an error from files
lval builtin_err(lenv& e, const lval& v) // noexcept(false)
{
    LASSERT_NUM("error", v, 1);
    LASSERT_TYPE("error", v.cell()[0], 1, lval::LVAL_STR, "String");
    return lval(v.cell()[0].str(), l_err);
}

// call a function/lambda
lval call(lenv& e, const lval& function, const lval& value) // noexcept(false)
{
    lval fun = function, v = value;

    // Call builtin function
    if (fun.type() == lval::LVAL_FUN) return fun.fun()(e, v);

    // Record argument number
    size_t given = v.cell().size(), total = fun.lambda().formals->cell().size();

    // While there are still arguments
    while (!v.cell().empty())
    {
        const auto& f = fun.lambda().formals;

        // If run out of count
        if (f->cell().empty())
            return lval("Function passed too many arguments. "s +
                        "Expected " + to_string(total) + ", got " + to_string(given), l_err);

        // Get the first symbol
        const lval& sym = f->pop(f->cell().cbegin());

        // Special case: & = variable argument
        if (sym.sym() == "&")
        {
            // Ensure there's another symbol after &
            if (f->cell().size() != 1)
                return lval("Function format invalid: no single symbol after &.", l_err);

            // Bound remaining arguments
            lval nsym = f->pop(f->cell().cbegin());
            put(fun.lambda().env, nsym, builtin_list(e, v));
            break;
        }

        // Get the next argument
        const lval& val = v.pop(v.cell().cbegin());

        // Bind a copy
        put(fun.lambda().env, sym, val);
    }

    const auto& f = fun.lambda().formals;

    // Special case: bind & to empty list
    if (!f->cell().empty() && f->cell()[0].sym() == "&")
    {
        // Check & have been passed correctly
        if (f->cell().size() != 2)
            return lval("Function format invalid: no single symbol after &.", l_err);

        // Pop &
        f->pop(f->cell().cbegin());

        // Pop symbol and create list
        lval sym = f->pop(f->cell().cbegin()), val = lval(l_qexpr);

        // Bind to environment
        put(fun.lambda().env, sym, val);
    }

    // If all argument are bound
    if (f->cell().empty())
    {
        // Set parent environment
        fun.lambda().env.parent = shared_ptr<lenv>(shared_ptr<lenv>(), &e);

        // Evaluate
        lval res;
        res.add(*fun.lambda().body);
        return builtin_eval(fun.lambda().env, res);
    }

    return fun;
}

// Add a builtin function
void add_builtin(lenv& e, const string& name, const builtin_t& func) noexcept
{
    put(e, lval(name, l_sym), lval(func));
}

// Add all the builtins
void add_builtins(lenv& e, mpc_pt& p) noexcept
{
    // List functions
    add_builtin(e, "list", builtin_list);
    add_builtin(e, "head", builtin_head);
    add_builtin(e, "tail", builtin_tail);
    add_builtin(e, "join", builtin_join);
    add_builtin(e, "eval", builtin_eval);

    // Define functions
    add_builtin(e, "def", [](lenv& e, const lval& v){return builtin_var(e, v, "def");});
    add_builtin(e, "=", [](lenv& e, const lval& v){return builtin_var(e, v, "=");});
    add_builtin(e, "\\", builtin_lambda);

    // Arithmetic functions
    add_builtin(e, "+", [](lenv& e, const lval& v){return builtin_op(e, v, "+");});
    add_builtin(e, "-", [](lenv& e, const lval& v){return builtin_op(e, v, "-");});
    add_builtin(e, "*", [](lenv& e, const lval& v){return builtin_op(e, v, "*");});
    add_builtin(e, "/", [](lenv& e, const lval& v){return builtin_op(e, v, "/");});
    add_builtin(e, "%", [](lenv& e, const lval& v){return builtin_op(e, v, "%");});

    // Comparision functions
    add_builtin(e, "<", [](lenv& e, const lval& v){return builtin_ord(e, v, "<");});
    add_builtin(e, ">", [](lenv& e, const lval& v){return builtin_ord(e, v, ">");});
    add_builtin(e, "<=", [](lenv& e, const lval& v){return builtin_ord(e, v, "<=");});
    add_builtin(e, ">=", [](lenv& e, const lval& v){return builtin_ord(e, v, ">=");});
    add_builtin(e, "==", [](lenv& e, const lval& v){return builtin_cmp(e, v, "==");});
    add_builtin(e, "!=", [](lenv& e, const lval& v){return builtin_cmp(e, v, "!=");});
    add_builtin(e, "if", builtin_if);

    // Load functions
    add_builtin(e, "load", [&p](lenv& e, const lval& v){return builtin_load(e, v, p);});
    add_builtin(e, "print", builtin_print);
    add_builtin(e, "error", builtin_err);
}

// Evaluate a S-Expr lval
lval eval_sexpr(lenv& e, const lval& v) // noexcept(false)
{
    lval res = v;

    // Evaluate all children
    for (auto& a : res.cell())
        a = eval(e, a);

    // Check errors
    for (auto cit = res.cell().cbegin(); cit != res.cell().cend(); ++cit)
        if (cit->type() == lval::LVAL_ERR)
            return res.pop(cit);

    // Empty S-Expr
    if (res.cell().empty()) return res;

    // Single S-Expr
    if (res.cell().size() == 1) return res.pop(res.cell().cbegin());

    // Ensure first element is function/lambda
    lval l = res.pop(res.cell().cbegin());
    if (l.type() != lval::LVAL_FUN && l.type() != lval::LVAL_LAMBDA)
        return lval("S-Expr have incorrect type for element 1. "s +
                    "Expected Function/Lambda, got " + to_string(l), l_err);

    // Evaluate function
    return call(e, l, res);
}

// Evaluate lval
lval eval(lenv& e, const lval& v) // noexcept(false)
{
    // Symbol: put into environment
    if (v.type() == lval::LVAL_SYM)
        return get(e, v);

    // S-Expr: evaluate
    if (v.type() == lval::LVAL_SEXPR)
        return eval_sexpr(e, v);

    // other: return
    return v;
}

// Main function
int main(int argc, char* argv[]) // noexcept(false)
{
    // Create mpc parsers
    auto Number = make_parser("number"),
         Symbol = make_parser("symbol"),
         String = make_parser("string"),
         Comment = make_parser("comment"),
         SExpr = make_parser("sexpr"),
         QExpr = make_parser("qexpr"),
         Expr = make_parser("expr"),
         Lispy = make_parser("lispy");

    // Define parsers with language
    const auto lang = R"*******(
        number  : /(+|-)?[0-9]+/ ;
        symbol  : /[a-zA-Z0-9_+\-*\/%\\=<>!&]+/ ;
        string  : /"(\\.|[^"])*"/ ;
        comment : /;[^\r\n]*/ ;
        sexpr   : '(' <expr>* ')' ;
        qexpr   : '{' <expr>* '}' ;
        expr    : <number> | <symbol> | <string> | <comment> | <sexpr> | <qexpr> ;
        lispy   : /^/ <expr>* /$/ ;
        )*******"s;
    mpca_lang(MPCA_LANG_DEFAULT,
        lang.c_str(), Number.get(), Symbol.get(), String.get(), Comment.get(),
        SExpr.get(), QExpr.get(), Expr.get(), Lispy.get());

    // Create environment
    lenv e;
    add_builtins(e, Lispy);

    // Execute files
    if (argc >= 2)
    {
        // loop over filenames
        for (int i = 1; i < argc; ++i)
        {
            // Create argument list
            lval args;
            args.add(lval(argv[i]));

            // Load the file
            lval x = builtin_load(e, args, Lispy);
            if (x.type() == lval::LVAL_ERR)
                cout << x << '\n';
        }
        return 0;
    }

    // Print version and exit information
    const auto lispy_ver = "1.0.2"s;
    cout << "Lispy Version " << lispy_ver << '\n';
    cout << "Press Ctrl-C to exit." << endl;

    // main infinite loop
    while (lispy_ver == "1.0.2"s)
    {
        // Output prompt and got input
        string input = readline("lispy> ");

        // Add input to history
        add_history(input.c_str());

        // Parse the input
        mpc_result_t r;
        if (mpc_parse("<stdin>", input.c_str(), Lispy.get(), &r))
        {
            // Success. Print the evaluated result
            auto a = mpc_at(static_cast<mpc_ast_t*>(r.output));
            cout << eval(e, lval(a.get())) << endl;
        }
        else
        {
            // Error
            auto err = mpc_et(r.error);
            mpc_err_print(err.get());
        }
    }
    return 0;
}
