#ifndef tokens_h
#define tokens_h
/* tokens.h -- List of labelled tokens and stuff
 *
 * Generated from: example1.g
 *
 * Terence Parr, Will Cohen, and Hank Dietz: 1989-2001
 * Purdue University Electrical Engineering
 * ANTLR Version 1.33MR33
 */
#define zzEOF_TOKEN 1
#define WRITE 2
#define ID 3
#define ASIG 4
#define NUM 5
#define PLUS 6
#define MINUS 7
#define MULT 8
#define DIV 9
#define PARO 10
#define PARC 11
#define SPACE 12

#ifdef __USE_PROTOS
void program(AST**_root);
#else
extern void program();
#endif

#ifdef __USE_PROTOS
void instruction(AST**_root);
#else
extern void instruction();
#endif

#ifdef __USE_PROTOS
void expr(AST**_root);
#else
extern void expr();
#endif

#ifdef __USE_PROTOS
void multExpr(AST**_root);
#else
extern void multExpr();
#endif

#ifdef __USE_PROTOS
void divExpr(AST**_root);
#else
extern void divExpr();
#endif

#ifdef __USE_PROTOS
void parExpr(AST**_root);
#else
extern void parExpr();
#endif

#endif
extern SetWordType zzerr1[];
extern SetWordType zzerr2[];
extern SetWordType zzerr3[];
extern SetWordType setwd1[];
