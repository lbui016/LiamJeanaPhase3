%{
#include "CodeNode.h"
#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <vector>
#include <string>
#include "bison.tab.h"

#define YY_NO_UNPUT
/*extern int yylex();
extern int yyparse();*/
extern FILE* yyin;

void yyerror(const char *msg);
%}

%union{
  struct CodeNode *code_node; 
  char* ident;
  int num;
}

%error-verbose
/*%locations*/

%start program
%token FUNCTION
%token BEGIN_PARAMS
%token END_PARAMS
%token BEGIN_LOCALS
%token END_LOCALS
%token BEGIN_BODY
%token END_BODY
%token INTEGER
%token ARRAY
%token OF
%token IF
%token THEN
%token ENDIF
%token ELSE
%token WHILE
%token DO
%token BEGIN_LOOP
%token ENDLOOP
%token CONTINUE
%token BREAK
%token READ
%token WRITE
%token NOT
%token TRUE
%token FALSE
%token RETURN

%left SUB
%left ADD
%left MULT
%left DIV
%left MOD

%left EQ /*unsure*/
%left NEQ
%left LT
%left GT
%left LTE
%left GTE

%token <ident> IDENT
%token <num> NUMBER

%token SEMICOLON
%token COLON
%token COMMA
%token L_PAREN
%token R_PAREN
%token L_SQUARE_BRACKET
%token R_SQUARE_BRACKET
%token ASSIGN /*unsure*/

%type <code_nodes> statements
%type <code_nodes> statement
%type <code_node> functions
%type <code_node> function
%type <code_node> declarations
%type <code_node> declaration

%type <op_val> ident

%start program

%%
  /* write your rules here */

program: functions {
	CodeNode *node = $1;
	printf("%s\n", node->code.c_str());
};

functions: function functions{
	CodeNode *code_node1 = $1;
	CodeNode *code_node2 = $2;

	CodeNode *node = new CodeNode;
	node->code = code_node1->code + code_node2->code;
	$$ = node; }
	| %empty { 
	//printf("functions -> epsilon\n") 
	CodeNode *node = new CodeNode;
	$$ = node; //empty 
};

function: FUNCTION ident SEMICOLON BEGIN_PARAMS declarations END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY {
	//{printf("function -> FUNCTION IDENT SEMICOLON BEGIN_PARAMS declarations END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY \n");}
	CodeNode *node = new CodeNode;
	std::string func_name = $2;
	node->code = "";
	node->code += std::string("func ") + func_name + std::string("\n");
	// declare the declarations
	CodeNode *params = $5;
	node->code += params->code;

	// declare local variables
	CodeNode *locals = $8;
	node->code += locals->code;
	
	// add the statements
	CodeNode *statements = $11;
	node->code += statements->code;

	node->code += std::string("endfunc\n");

	$$ = node; 
};

declarations: declaration SEMICOLON declarations {
	//printf("declarations -> declaration SEMICOLON declarations\n");
	CodeNode *code_node1 = $1;
	CodeNode *code_node2 = $3;

	CodeNode *node = new CodeNode;
	node->code = code_node1->code + code_node2->code;
	$$ = node; }	
	| %empty {
	//printf("declarations -> epsilon\n");
	CodeNode *node = new CodeNode;
	$$ = node;  //empty
};

declaration: ident COLON INTEGER {
	//printf("declaration -> identifiers COLON INTEGER \n");
	printf("declaration: %s\n", $1);
	std::string id = $1;
	CodeNode *node = new CodeNode;
	//printf(". %s\n", id);
	node->code = std::string(". ") + id + std::string("\n");
	$$ = node;
	//node->name 
	}
	| ident COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER {
	//printf("Declaration -> identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER \n")
	//NOT DONE? -JEANA THINKS
};

ident: IDENT {
	//printf("ident -> IDENT %s\n", $1);
	$$ = $1; 
};

statement: var ASSIGN expression {
	//printf("statement -> var ASSIGN expression\n");
	CodeNode *node = new CodeNode;
	std::string id = $1;
	node->code = "";	//hard coded: expression.name
	node->code = std::string("= ") + id + std::string(", 150\n");
	$$ = node;
	}
  | IF bool_expr THEN statements elseStatement ENDIF {printf("statement -> IF bool_expr THEN statements elseStatement ENDIF \n");}
  | WHILE bool_expr BEGIN_LOOP statements ENDLOOP {printf("statement -> WHILE bool_expr BEGIN_LOOP statements ENDLOOP \n");}
  | DO BEGIN_LOOP statements ENDLOOP WHILE bool_expr {printf("statement -> DO BEGIN_LOOP statements ENDLOOP WHILE bool_expr \n");}
  | READ var {printf("statement -> READ vars \n")}
  | WRITE var {
	//printf("statement -> WRITE vars \n");
	CodeNode *node = newCodeNode;
	std::string var = "n"; //var.name
	node->code = ".> n\n";
	$$ = node;}
  | CONTINUE {printf("statement -> CONTINUE \n");}
  | BREAK {printf("statement -> BREAK \n");}
  | RETURN expression {printf("statement -> RETURN expression \n");}
;

statements: statement SEMICOLON statements {
  	CodeNode *code_node1 = $1;
	CodeNode *code_node2 = $3;
	
	CodeNode *node = new CodeNode;
	node->code = code_node1->code + code_node2->code;
	$$ = node;}
	| %empty {
	Code *node = new CodeNode;
	%% = node;
};

elseStatement: %empty {printf("elseStatement -> epsilon \n");} 
  | ELSE statements {printf("elseStatement -> ELSE statements \n");}
;

bool_expr: bool_expr bool_expr {printf("bool_expr -> bool_expr bool_expr \n");}
  | expression comp expression {printf("bool_expr -> expression comp expression \n");}
  | NOT {printf("bool_expr -> NOT \n");}
  | %empty {printf("bool_expr -> epsilon \n");}
;

comp: EQ {printf("comp -> EQ \n");}
  | NEQ {printf("comp -> NEQ \n");}
  | LT {printf("comp -> LT \n");}
  | GT {printf("comp -> GT \n");}
  | LTE {printf("comp -> LTE \n");}
  | GTE {printf("comp -> GTE \n");}
;

expression: multExpr {printf("expression -> multExpr \n");}
  | multExpr ADD expression {printf("expression -> multExpr ADD expression \n");}
  | multExpr SUB expression {printf("expression -> multExpr SUB expression \n");} 
;

multExpr: term  {printf("multExpr -> term \n");}
  | term MULT multExpr {printf("multExpr -> term MULT multExpr \n");}
  | term DIV multExpr {printf("multExpr -> term DIV multExpr \n");}
  | term MOD multExpr {printf("multExpr -> term MOD multExpr \n");}
;

term: var {printf("term -> var \n");}
  | NUMBER {printf("term -> NUMBER %d\n", $1);}
  | L_PAREN expression R_PAREN {printf("term -> L_PAREN expression R_PAREN \n");}
  | ident L_PAREN expression R_PAREN {printf("term -> ident L_PAREN expression R_PAREN \n");}
  | ident L_PAREN expression COMMA R_PAREN {printf("term -> ident L_PAREN expression COMMA R_PAREN \n");}
  | ident L_PAREN R_PAREN {printf("term -> ident L_PAREN R_PAREN");} 
;

var: IDENT {printf("var -> IDENT \n");}
  | IDENT L_SQUARE_BRACKET expression R_SQUARE_BRACKET {printf("var -> IDENT L_SQUARE_BRACKET expression R_SQUARE_BRACKET \n");}
;
%%

/*int main(int argc, char **argv) {
        yyin = stdin;
        do {
                yyparse();
        } while(!feof(yyin));
   return 0;
}*/

int main(int argc, char  **argv) {
        yyparse();
        return 0;
}

void yyerror(const char *msg) {
  extern int lineCount;
  extern char* yytext;
  printf("Error: on line %d: %s\n", lineCount, msg);
  exit(1);
}
