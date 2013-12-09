/* File: ast_stmt.h
 * ----------------
 * The Stmt class and its subclasses are used to represent
 * statements in the parse tree.  For each statment in the
 * language (for, if, return, etc.) there is a corresponding
 * node class for that construct.
 *
 * pp4: You will need to extend the Stmt classes to implement
 * code generation for statements.
 */

#ifndef _H_ast_stmt
#define _H_ast_stmt

#include "list.h"
#include "ast.h"
#include "hashtable.h"
#include <stack>

class Decl;
class VarDecl;
class Expr;
class CodeGenerator;
class Location;

class Scope
{
  public:
    Hashtable<Decl*> *table;

  public:
    Scope();

    void AddDecl(Decl *d);
    friend ostream& operator<<(ostream& out, Scope *s);
};

class Program : public Node
{
  public:
    static Scope *gScope;
    static stack<const char*> *gBreakLabels;

  protected:
     List<Decl*> *decls;
     CodeGenerator *codeGenerator;

  public:
     Program(List<Decl*> *declList);
     void Check();
     void Emit();

  private:
    void BuildScope();
};

class Stmt : public Node
{
  public:
    Stmt();
    Stmt(yyltype loc);

    virtual void BuildScope() = 0;
    virtual Location* Emit(CodeGenerator *cg) = 0;
    virtual int GetMemBytes() = 0;
};

class StmtBlock : public Stmt
{
  protected:
    List<VarDecl*> *decls;
    List<Stmt*> *stmts;

  public:
    StmtBlock(List<VarDecl*> *variableDeclarations, List<Stmt*> *statements);

    void BuildScope();
    Location* Emit(CodeGenerator *cg);
    int GetMemBytes();
};

class ConditionalStmt : public Stmt
{
  protected:
    Expr *test;
    Stmt *body;

  public:
    ConditionalStmt(Expr *testExpr, Stmt *body);

    virtual void BuildScope() = 0;
};

class LoopStmt : public ConditionalStmt
{
  public:
    LoopStmt(Expr *testExpr, Stmt *body)
            : ConditionalStmt(testExpr, body) {}

    virtual void BuildScope() = 0;
};

class ForStmt : public LoopStmt
{
  protected:
    Expr *init, *step;

  public:
    ForStmt(Expr *init, Expr *test, Expr *step, Stmt *body);

    void BuildScope();
    Location* Emit(CodeGenerator *cg);
    int GetMemBytes();
};

class WhileStmt : public LoopStmt
{
  public:
    WhileStmt(Expr *test, Stmt *body) : LoopStmt(test, body) {}

    void BuildScope();
    Location* Emit(CodeGenerator *cg);
    int GetMemBytes();
};

class IfStmt : public ConditionalStmt
{
  protected:
    Stmt *elseBody;

  public:
    IfStmt(Expr *test, Stmt *thenBody, Stmt *elseBody);

    void BuildScope();
    Location* Emit(CodeGenerator *cg);
    int GetMemBytes();
};

class BreakStmt : public Stmt
{
  public:
    BreakStmt(yyltype loc) : Stmt(loc) {}

    void BuildScope() { /* Empty */ }
    Location* Emit(CodeGenerator *cg);
    int GetMemBytes();
};

class ReturnStmt : public Stmt
{
  protected:
    Expr *expr;

  public:
    ReturnStmt(yyltype loc, Expr *expr);

    void BuildScope();
    Location* Emit(CodeGenerator *cg);
    int GetMemBytes();
};

class PrintStmt : public Stmt
{
  protected:
    List<Expr*> *args;

  public:
    PrintStmt(List<Expr*> *arguments);

    void BuildScope();
    Location* Emit(CodeGenerator *cg);
    int GetMemBytes();
};

#endif
