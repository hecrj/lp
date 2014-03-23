/**
 * compres grammar and interpreter
 *
 * By default the interpreter will be compiled in interactive mode.
 * This means that its going to read the input stream line by line,
 * executing the instructions and saving data in memory like a real
 * interpreter.
 * If you define the constant NO_INTERACTIVE at compile time
 * (with -DNO_INTERACTIVE) the resulting interpreter will read the
 * entire input and then it will only evaluate the nodes where some
 * information needs to be printed, using the provided findASTCompraDef
 * function.
 *
 * I've decided to do an interactive version because I think that trying to
 * imitate real interpreters is more interesting and challenging.
 *
 * I've also added another way to define lists:
 * { key: number, ... }
 *
 * PD: I'm currently coursing compilers (CL) too.
 */
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
#define AST_FIELDS string kind; string text; int type;
#include "ast.h"

// macro to create a new AST node (and function predeclaration)
#define zzcr_ast(as,attr,ttype,textt) as=createASTnode(attr,ttype,textt)
AST* createASTnode(Attrib* attr, int ttype, char *textt);
>>

<<
#include <cstdlib>
#include <cmath>
#include <map>
#include <set>

//global structures
AST *root;

// function to fill token information
void zzcr_attr(Attrib *attr, int type, char *text) {
  if (type == ID) {
    attr->kind = "id";
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

  // I've added this to be able to use defined tokens to identify nodes
  as->type = type;
  
  as->right = NULL; 
  as->down = NULL;
  return as;
}


/// create a new "list" AST node with one element
AST* createASTlist(AST *child) {
 AST *as=new AST;
 as->kind="list";
 as->right=NULL;
 as->down=child;
 return as;
}

AST *findASTCompraDef(string id) {
  AST *n = root->down;
  while (n != NULL and (n->kind != "=" or n->down->text != id)) n = n->right;
if (id != n->down->text) {cout << "MISMATCH: " << id << " " << n->down->text << endl;}
  return n;
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

/*############################################*/
/**
 * List class
 */
class List
{
  map<string, int> items;

public:
  int references;

  /**
   * In order to register every new list as possible garbage
   * we need to implement the constructors later.
   */
  List();
  List(List *l);

  void put(string product, int amount)
  {
    items[product] = amount;
  }

  int size()
  {
    return items.size();
  }

  int units()
  {
    int units = 0;
    map<string, int>::iterator it = items.begin();

    while(it != items.end())
    {
      units += it->second;
      ++it;
    }

    return units;
  }

  double stdev()
  {
    int num_items = size();
    
    if(num_items < 2)
      return 0;

    double mean = ((double) units()) / num_items;
    double sum = 0;
    double factor;

    map<string, int>::iterator it = items.begin();

    while(it != items.end())
    {

      factor = it->second - mean;
      sum += factor*factor;

      ++it;
    }

    return sqrt(sum / (num_items - 1));
  }

  List* minus(List *l)
  {
    List *result = new List(this);

    map<string, int>::iterator current = l->items.begin();
    map<string, int>::iterator it;

    while(current != l->items.end())
    {
      // I know that we are not supposed to check this
      // But I'd like to throw an exception if the operation can not
      // be performed in order to make the interpreter more stable
      it = result->items.find(current->first);

      if(it == result->items.end())
        throw "First operand does not contain: " + current->first;

      if(it->second < current->second)
        throw "First operand contains less '" + it->first + "' than second " +
              "operand";

      it->second -= current->second;
      ++current;
    }

    return result;
  }

  List* join(List *l)
  {
    List *result = new List(this);

    map<string, int>::iterator current = l->items.begin();
    map<string, int>::iterator it;

    while(current != l->items.end())
    {
      result->items[current->first] += current->second;
      ++current;
    }

    return result;
  }

  List* mult(int m)
  {
    List *result = new List(this);

    map<string, int>::iterator current = result->items.begin();

    while(current != result->items.end())
    {
      current->second *= m;
      ++current;
    }

    return result;
  }

  void print()
  {
    cout << "[";
    map<string, int>::iterator current = items.begin();
    bool first = true;

    while(current != items.end())
    {
      if(first) first = false;
      else cout << ", ";

      cout << "(" << current->second << ", " << current->first << ")";
      ++current;
    }

    cout << "]" << endl;
  }
};

/**
 * Memory map
 * Only used on the interactive version
 */
#ifndef NO_INTERACTIVE
map<string, List*> memory;
#endif

/**
 * Stores possible garbage to clean when dereferenced.
 * We use a set to avoid double deletion (scary).
 */
set<List*> garbage;

/**
 * List constructors
 */
// Empty list constructor
List::List()
{
  references = 0;

  // Mark the new list as possible garbage
  garbage.insert(this);
}

// Copy constructor
List::List(List *l)
{
  items = l->items;
  references = 0;

  garbage.insert(this);
}


/**
 * An atom can be a parenthesized expression.
 * We need to define the eval_expr header in order to use recursion.
 */
List* eval_expr(AST *node);

List* eval_product_list(AST *node)
{
  List* l = new List();
  AST* current = node->down;

  while(current != NULL)
  {
    l->put(current->down->text, atoi(current->kind.c_str()));
    current = current->right;
  }

  return l;
}

List* eval_id(AST *node)
{
#ifdef NO_INTERACTIVE
  return eval_expr(child(findASTCompraDef(node->text), 1));
#else
  map<string, List*>::iterator it = memory.find(node->text);

  if(it == memory.end())
    throw "Undefined variable: " + node->text;

  return it->second;
#endif
}

List* eval_atom(AST *node)
{
  if(node->type == BRACKO)
    return eval_product_list(node);

  if(node->type == ID)
    return eval_id(node);

  return eval_expr(node);
}

List* eval_mult_expr(AST *node)
{
  if(node->type == MULT)
  {
    int intconst = atoi(child(node, 0)->kind.c_str());
    return eval_mult_expr(child(node, 1))->mult(intconst);
  }

  return eval_atom(node);
}

List* eval_expr(AST *node)
{
  if(node->type == MINUS)
    return eval_expr(child(node, 0))->minus(eval_expr(child(node, 1)));

  if(node->type == AND)
    return eval_expr(child(node, 0))->join(eval_expr(child(node, 1)));

  return eval_mult_expr(node);
}

double stdev(AST *node)
{
  return eval_expr(child(node, 0))->stdev();
}

int unitats(AST *node)
{
  return eval_expr(child(node, 0))->units();
}

int productes(AST *node)
{
  return eval_expr(child(node, 0))->size();
}

void eval_print(AST *node)
{
  eval_expr(child(node, 0))->print();
}

#ifndef NO_INTERACTIVE
void eval_assign(AST *node)
{
  string id = child(node, 0)->text;
  List* l = eval_expr(child(node, 1));

  map<string, List*>::iterator it = memory.find(id);

  // Memory overwrite
  if(it != memory.end())
  {
    it->second->references -= 1;
    garbage.insert(it->second);
  }

  memory[id] = l;
  l->references += 1;
}
#endif

void eval(AST *root)
{
  AST *current = root->down;

  while(current != NULL)
  {
    switch(current->type)
    {
      case STDEV:
        cout << stdev(current) << endl; break;

      case UNITS:
        cout << unitats(current) << endl; break;

      case PRODS:
        cout << productes(current) << endl; break;

      case PRINT:
        eval_print(current); break;

#ifndef NO_INTERACTIVE
      case EQUAL:
        eval_assign(current); break;
#endif
    }

    current = current->right;
  }
}

/**
 * To avoid memory leaks we need to free the lists that
 * can not be referenced anymore.
 */
void collect_garbage()
{
  set<List*>::iterator it = garbage.begin();

  while(it != garbage.end())
  {
    List *l = *it;

    if(l->references < 1)
      delete l;

    // This can be used to clear the set at the same time we iterate it
    garbage.erase(it++);
  }
}

/**
 * Deletes an entire AST
 */
void ASTFree(AST *a)
{
  if(a == NULL)
    return;

  ASTFree(a->right);
  ASTFree(a->down);

  delete a;
}

/**
 * Returns the total number of interpreter errors
 */
int error_count()
{
  return zzSyntaxErrCount + zzLexErrCount;
}

/**
 * Main function
 */
int main()
{
#ifdef NO_INTERACTIVE
  root = NULL;
  ANTLR(compres(&root), stdin);
  ASTPrint(root);
  eval(root);
#else
  // Interactive mode
  // Read input line by line using memory like a real interpreter
  int errors;
  string s;

  while(getline(cin, s))
  {
    errors = error_count();
    root = NULL;

    ANTLRs(compres(&root), (char*)s.c_str());

    if(errors - error_count() == 0)
    {
      ASTPrint(root);

      try
      {
        eval(root);
      }
      catch(string exception)
      {
        cout << "error: " << exception << endl;
      }
    }

    ASTFree(root);
    collect_garbage();
  }
#endif
}
>>

#lexclass START
#token EQUAL "="
#token BRACKO "\["
#token BRACKC "\]"
#token KEYO "\{"
#token KEYC "\}"
#token PARO "\("
#token PARC "\)"
#token COMMA ","
#token COLON ":"
#token MINUS "MINUS"
#token AND "AND"
#token MULT "\*"
#token STDEV "DESVIACIO"
#token UNITS "UNITATS"
#token PRODS "PRODUCTES"
#token PRINT "MOSTRAR"
#token NUM "[0-9]+"
#token ID "[a-zA-Z][a-zA-Z0-9]*"
#token SPACE "[\ \n]" << zzskip();>>
#token COMMENT "//~[\n]*" << zzskip();>>

compres
  : (instruction)* <<#0=createASTlist(_sibling);>>
  ;

instruction
  : assign
  | show_info
  ;

assign
  : ID EQUAL^ expr
  ;

show_info
  : ( STDEV^
    | UNITS^
    | PRODS^
    | PRINT^) expr
  ;

expr
  : mult_expr ((MINUS^|AND^) mult_expr)*
  ;

mult_expr
  : NUM (MULT^ mult_expr)
  | atom
  ;

atom
  : product_list
  | ID
  | PARO! expr PARC!
  ;

product_list
  : BRACKO^ items BRACKC!
  | k:KEYO^ <<#k->type=BRACKO;>> items_new KEYC!
  ;

items
  : { item (COMMA! item)* }
  ;

item
  : PARO! NUM^ COMMA! ID PARC!
  ;

items_new
  : { item_new (COMMA! item_new)* }
  ;

item_new
  : ID COLON! NUM^
  ;
