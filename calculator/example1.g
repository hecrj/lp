#header
<<
#include <string>
#include <iostream>
#include <map>
using namespace std;

// struct to store information about tokens
typedef struct {
  string kind;
  string text;
} Attrib;

// function to fill token information (predeclaration)
void zzcr_attr(Attrib *attr, int type, char *text);

// fields for AST nodes
#define AST_FIELDS string kind; string text;
#include "ast.h"

// macro to create a new AST node (and function predeclaration)
#define zzcr_ast(as,attr,ttype,textt) as=createASTnode(attr,ttype,textt)
AST* createASTnode(Attrib* attr, int ttype, char *textt);
>>

<<
#include <cstdlib>
#include <cmath>

// Map to store variables
map<string, int> m; 

// function to fill token information
void zzcr_attr(Attrib *attr, int type, char *text) {
  if (type == NUM) {
    attr->kind = "intconst";
    attr->text = text;
  }
  else if(type == ID)
  {
    attr->kind = "variable";
    attr->text = text;
  }
  else {
    attr->kind = text;
    attr->text = "";
  }
}

// function to create a new AST node
AST* createASTnode(Attrib* attr, int type, char* text) {
  AST* as = new AST;
  as->kind = attr->kind; 
  as->text = attr->text;
  as->right = NULL; 
  as->down = NULL;
  return as;
}

/// get nth child of a tree. Count starts at 0.
/// if no such child, returns NULL
AST* child(AST *a,int n) {
 AST *c=a->down;
 for (int i=0; c!=NULL && i<n; i++) c=c->right;
 return c;
} 

/// print AST, recursively, with indentation
void ASTPrintIndent(AST *a,string s)
{
  if (a==NULL) return;

  cout<<a->kind;
  if (a->text!="") cout<<"("<<a->text<<")";
  cout<<endl;

  AST *i = a->down;
  while (i!=NULL && i->right!=NULL) {
    cout<<s+"  \\__";
    ASTPrintIndent(i,s+"  |"+string(i->kind.size()+i->text.size(),' '));
    i=i->right;
  }
  
  if (i!=NULL) {
      cout<<s+"  \\__";
      ASTPrintIndent(i,s+"   "+string(i->kind.size()+i->text.size(),' '));
      i=i->right;
  }
}

/// print AST 
void ASTPrint(AST *a)
{
  while (a!=NULL) {
    cout<<" ";
    ASTPrintIndent(a,"");
    a=a->right;
  }
}

int evaluate(AST *a)
{
  if(a == NULL)
    return 0;

  if(a->kind == "intconst")
    return atoi(a->text.c_str());

  if(a->kind == "variable")
    return m[a->text];

  if(a->kind == "+")
    return evaluate(child(a, 0)) + evaluate(child(a, 1));

  if(a->kind == "-")
    return evaluate(child(a, 0)) - evaluate(child(a, 1));

  if(a->kind == "*")
    return evaluate(child(a, 0)) * evaluate(child(a, 1));

  if(a->kind == "/")
    return evaluate(child(a, 0)) / evaluate(child(a, 1));
}

void execute(AST *a)
{
  if (a == NULL)
    return;
  else if (a->kind == ":=")
    m[child(a,0)->text] = evaluate(child(a,1));
  else // a->kind == "write"
    cout << evaluate(child(a,0)) << endl;

  execute(a->right);
}

int main() {
  AST *root = NULL;
  ANTLR(program(&root), stdin);
  ASTPrint(root);
  execute(root);
}
>>

#lexclass START
#token WRITE "write"
#token ID "[a-zA-Z]"
#token ASIG ":="
#token NUM "[0-9]+"
#token PLUS "\+"
#token MINUS "\-"
#token MULT "\*"
#token DIV "\/"
#token PARO "\("
#token PARC "\)"
#token SPACE "[\ \n]" << zzskip();>>

program: (instruction)* ;
instruction: ID ASIG^ expr | WRITE^ expr;
expr: multExpr ((PLUS^|MINUS^) multExpr)* ;
multExpr: divExpr (MULT^ divExpr)* ;
divExpr: parExpr (DIV^ parExpr)* ;
parExpr: NUM | ID | PARO! expr PARC! ;
