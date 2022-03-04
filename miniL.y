%{
#include "CodeNode.h"
#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <vector>
#include <string>
#include <string.h>
#include "y.tab.h"

#define YY_NO_UNPUT
extern int yylex();
extern int yyparse();
extern FILE* yyin;

int count_names = 0;
int count_loop = 0;
int count_if = 0;

enum Type { Integer, Array };
struct Symbol {
  std::string name;
  Type type;
};
struct Function {
  std::string name;
  std::vector<Symbol> declarations;
};

std::vector <Function> symbol_table;


Function *get_function() {
  int last = symbol_table.size()-1;
  return &symbol_table[last];
}

bool find(std::string &value) {
  Function *f = get_function();
  for(int i=0; i < f->declarations.size(); i++) {
    Symbol *s = &f->declarations[i];
    if (s->name == value) {
      return true;
    }
  }
  return false;
}

void add_function_to_symbol_table(std::string &value) {
  Function f; 
  f.name = value; 
  symbol_table.push_back(f);
}

void add_variable_to_symbol_table(std::string &value, Type t) {
  Symbol s;
  s.name = value;
  s.type = t;
  Function *f = get_function();
  f->declarations.push_back(s);
}

void print_symbol_table(void) {
  printf("symbol table:\n");
  printf("--------------------\n");
  for(int i=0; i<symbol_table.size(); i++) {
    printf("function: %s\n", symbol_table[i].name.c_str());
    for(int j=0; j<symbol_table[i].declarations.size(); j++) {
      printf("  locals: %s\n", symbol_table[i].declarations[j].name.c_str());
    }
  }
  printf("--------------------\n");
}

void yyerror(const char *msg);
%}


%union{
  char *ident; //op_val
  struct CodeNode *code_node; 
  int num; //int_val
}


%error-verbose
/*%locations*/

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

%token SUB
%token ADD
%token MULT
%token DIV
%token MOD

%token EQ /*unsure*/
%token NEQ
%token LT
%token GT
%token LTE
%token GTE

%token <ident> IDENT
%token <ident> NUMBER

%token SEMICOLON
%token COLON
%token COMMA
%left L_PAREN R_PAREN
%token L_PAREN
%token R_PAREN
%token L_SQUARE_BRACKET
%token R_SQUARE_BRACKET
%token ASSIGN /*unsure*/

%type <ident> ident
%type <code_node> functions
%type <code_node> function
%type <code_node> declarations
%type <code_node> declaration
%type <code_node> statements
%type <code_node> statement
%type <code_node> expression
%type <code_node> multExpr
%type <code_node> term
%type <code_node> var
%type <code_node> paramDeclarations
%type <code_node> localDeclarations
%type <code_node> bool_expr
%type <code_node> comp
%type <code_node> elseStatement

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

grammar: FUNCTION ident {
	std::string func_name = $2;
	add_function_to_symbol_table(func_name);
	
};

function: grammar  SEMICOLON BEGIN_PARAMS paramDeclarations END_PARAMS BEGIN_LOCALS localDeclarations END_LOCALS BEGIN_BODY statements END_BODY {
	//{printf("function -> FUNCTION IDENT SEMICOLON BEGIN_PARAMS declarations END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY \n");}
	CodeNode *node = new CodeNode;
	//midrule, add function to symbol table
	//add_function_to_symbol_table(func_name);
	Function *func_name = get_function(); 
	node->code = "";
	node->code += std::string("func ") + func_name->name + std::string("\n");
	// declare the params declarations
	CodeNode *declarations = $4;
	node->code += declarations->code;

	// declare local variables
	CodeNode *locals = $7;
	node->code += locals->code;
	
	// add the statements
	CodeNode *statements = $10;
	node->code += statements->code;

	node->code += std::string("endfunc\n") + std::string("\n");

	$$ = node; 
};

paramDeclarations: declarations {
	CodeNode *node = new CodeNode;
	Function *f = get_function();
	node->code = $1->code;
	for(int i = 0; i < f->declarations.size(); i++) {
		node->code += std::string("= ") + f->declarations[i].name + std::string(", ") + std::string("$") + std::to_string(i) + std::string("\n"); 
	}
	$$ = node;
};

localDeclarations: declarations {
	$$ = $1;
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
	$$ = node; 
};


declaration: ident COLON INTEGER {
	//printf("declaration -> identifiers COLON INTEGER \n");
	//printf("declaration: %s\n", $1);
	//std::string id = $1;
	//std::string id = $1;
	CodeNode *node = new CodeNode;
	std::string id = $1;
	Type t = Integer;
	add_variable_to_symbol_table(id, t);
	node->code = std::string(". ") + id + std::string("\n");
	$$ = node;
	//printf(". %s\n", id);
	//node->code = std::string(". ") + id + std::string("\n");
	//$$ = node;
	//node->name
	//std::string value = $1;
	//Type t = Integer;
	//add_variable_to_symbol_table(id, t); 
	}
	| ident COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER {
	//std::string id = $1;
	//CodeNode *node = new CodeNode;
	//node->code = std::string(". ") + id + std::string("\n");
	//$$ = node;
	//printf("Declaration -> identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER \n")
	//NOT DONE? -JEANA THINKS
	CodeNode *node = new CodeNode;
	std::string id = $1;
	node->code += std::string(".[] ") + id + std::string(", ") + $5 + std::string("\n");
	$$ = node; 
};

ident: IDENT {
	//printf("ident -> IDENT %s\n", $1);
	$$ = $1; 
};

statement: ident ASSIGN expression {
	//printf("statement -> var ASSIGN expression\n");
	CodeNode *node = new CodeNode;
	std::string id = $1;
	CodeNode *expression = $3;
	node->code = "";
	//node->name = id->name;
	//node->name += expression->name;
	node->code += expression->code;
	node->code += std::string("= ") + id + std::string(", ") + expression->name + std::string("\n");
	//printf("%p\n", $1);
	//printf("%s \n", node->code.c_str());
	$$ = node;
	}
  | ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET ASSIGN expression {
	CodeNode *node = new CodeNode;
	std::string id = $1;
	CodeNode *expression1 = $3;
	CodeNode *expression2 = $6;
	std::string temp = "_temp" + std::to_string(count_names);
	node->name = temp;
	//node->code = "";
	node->code += expression1->code + expression2->code;
	node->code += std::string("[]= ") + id + std::string(", ") + expression1->name + std::string(", ") + expression2->name + std::string("\n");
	//printf(node->code.c_str());
	$$ = node;
	//count_names++;
	}
  | IF bool_expr THEN statements elseStatement ENDIF {//printf("statement -> IF bool_expr THEN statements elseStatement ENDIF \n");
	CodeNode *node = new CodeNode;
	std::string temp = "_temp" + std::to_string(count_names - 1);
	CodeNode *bexpression = $2;
	CodeNode *statements = $4;
	CodeNode *elseStatement = $5;
	node->code += bexpression->code;
	node->code += "?:= if_true" + std::to_string(count_if) + ", " + temp + "\n"; 
//+ ":= else" + std::to_string(count_if) + "\n";
	node->code += statements->code + ":= endif" + std::to_string(count_if) + "\n";
	node->code += ": if_true" + std::to_string(count_if)  + "\n"; 
//node->name += statements->name + "\n";
	//node->code += statements->code + ":= endif" + std::to_string(count_if) + "\n";
	node->code += elseStatement->code;
	//node->code += statements->code;
	node->name = bexpression->name + statements->name;
	

	//node->code += ": if_true" + std::to_string(count_if)  + "\n"; 	
	//node->code += elseStatement->code;
	$$ = node;
	}
  | WHILE bool_expr BEGIN_LOOP statements ENDLOOP {
	//beginning of the loop 
	//int count_loop = 0;
	//std::stack<int> loop_stack;
	//push loop_stack

	//printf(": beginloop%d\n", count_loop);
	CodeNode *node = new CodeNode;
	node->code = ": beginloop" + std::to_string(count_loop)	+ "\n";
        std::string temp = "_temp" + std::to_string(count_names - 2);
        CodeNode *bexpression = $2;
        node->code += bexpression->code;
        node->name = bexpression->name;
        node->code += "?:= loopbody" + std::to_string(count_loop) + ", " + temp + "\n" + ":= endloop" + std::to_string(count_loop) + "\n" + ": loopbody" + std::to_string(count_loop) + "\n";
	//CodeNode *node = new CodeNode;
        CodeNode *statements = $4;
        node->name += statements->name;
        node->code += statements->code;
        
	node->code += ":= beginloop" + std::to_string(count_loop) + "\n" + ": endloop" + std::to_string(count_loop) + "\n";
	$$ = node;  

	count_loop++;
    }
  | DO BEGIN_LOOP statements ENDLOOP WHILE bool_expr {//printf("statement -> DO BEGIN_LOOP statements ENDLOOP WHILE bool_expr \n");
	//CodeNode *node = new CodeNode;
	//$$ = node;
	CodeNode *node = new CodeNode;
	$$ = node;
	}
  | READ var {//printf("statement -> READ vars \n")
	//CodeNode *node = new CodeNode;
	//CodeNode *var = $2;
	//node->code = std::string(".< ") + var->name + std::string("\n");
	//$$ = node;
	CodeNode *node = new CodeNode;
	$$ = node;
	}
  | WRITE var {
	//printf("statement -> WRITE vars \n");
	CodeNode *node = new CodeNode;
	CodeNode *var = $2;
	std::string id = var->name;
	//node->code = "";
	node->name = id;
	node->code += var->code;
	node->code += std::string(".> ") + id + std::string("\n");
	//printf(node->code.c_str());
	$$ = node;
	}
  | CONTINUE {
	//printf("statement -> CONTINUE \n");
	CodeNode *node = new CodeNode;
	$$ = node;
	}
  | BREAK {
	//printf("statement -> BREAK \n");
	CodeNode *node = new CodeNode;
	node->name += ":= endloop" + std::to_string(count_loop);
	printf("FROM BREAK\n");
	printf(node->name.c_str());
	printf("\n\n");
	$$ = node;
	}
  | RETURN expression {
	//printf("statement -> RETURN expression \n");
	CodeNode *node = new CodeNode;
	CodeNode *expression = $2;
	node->code += expression->code;
	std::string temp = "_temp" + std::to_string(count_names - 1);
        node->name = expression->name;
	//printf(node->name.c_str());
	node->code += std::string("ret ") + temp + std::string("\n");
	//printf(node->code.c_str());
	$$ = node;
	}
;

statements: statement SEMICOLON statements {
  	//printf("stat semi state\n");
	CodeNode *code_node1 = $1;
	CodeNode *code_node2 = $3;
	
	CodeNode *node = new CodeNode;
	node->code = code_node1->code + code_node2->code;
	node->name = code_node1->name + code_node2->name;
	printf(node->name.c_str());
	printf("\n\n");
	$$ = node;
	}
	| %empty {
	//printf("empty statement\n");
	CodeNode *node = new CodeNode;
	$$ = node;
};

elseStatement: %empty {
	//printf("elseStatement -> epsilon \n");
	CodeNode *node = new CodeNode;
        $$ = node;
	} 
  | ELSE statements {
	//printf("elseStatement -> ELSE statements \n");
	CodeNode *node = new CodeNode;
        CodeNode *statements = $2;
	//node->code += statements->code;
	node->code += ": else" + std::to_string(count_if) + "\n";
	//node->code += ": if_true" + std::to_string(count_if)  + "\n";
	node->code += statements->code;
	node->name = statements->name;
	node->code += ": endif" + std::to_string(count_if) + "\n";
	$$ = node;
	}
;

bool_expr: bool_expr bool_expr {
	//printf("bool_expr -> bool_expr bool_expr \n");
	CodeNode *node = new CodeNode;
        $$ = node;
	}
  | expression comp expression {
	//printf("bool_expr -> expression comp expression \n");
	/*CodeNode *node = new CodeNode;
	CodeNode *src1 = $1;
	CodeNode *src2 = $3;
	CodeNode *comparison  = $2;
	std::string comp = comparision->name;
	node->code = comp +*/
	//printf("exp comp exp\n");
	CodeNode *node = new CodeNode;
        std::string temp = "_temp" + std::to_string(count_names);
	//std::string comparison = $2;
	//printf(comparison.c_str());
	CodeNode *src1 = $1;
	CodeNode *src2 = $3;
	CodeNode *comparison = $2;
	node->name = temp;
	node->code += src1->code + src2->code;
	node->code += ". " + temp + "\n" + comparison->name + " " + temp + ", " + src1->name + ", " + src2->name + "\n"; 
	//CodeNode *comparision = $2;
	
	

	$$ = node;
	count_names++;
	}
  | NOT {
	//printf("bool_expr -> NOT \n");
	CodeNode *node = new CodeNode;
        $$ = node;
	}
  | %empty {
	//printf("bool_expr -> epsilon \n");
	CodeNode *node = new CodeNode;
        $$ = node;
	}
;

comp: EQ {
	//printf("comp -> EQ \n");
	CodeNode *node = new CodeNode;
        node->name = "==";
	$$ = node;
	}
  | NEQ {
	//printf("comp -> NEQ \n");
	CodeNode *node = new CodeNode;
        node->name = "!=";
	$$ = node;
	}
  | LT {
	//printf("comp -> LT \n");
	//CodeNode *node = new CodeNode;
        //std::string temp = "_temp" + std::to_string(count_names);
	//node->name = temp;
	CodeNode *node = new CodeNode;
	node->name = "<"; 
	$$ = node;
	}
  | GT {
	//printf("comp -> GT \n");
	CodeNode *node = new CodeNode;
	node->name = ">";
        $$ = node;
	}
  | LTE {
	//printf("comp -> LTE \n");
	CodeNode *node = new CodeNode;
	node->name = "<=";
        $$ = node;
	}
  | GTE {
	//printf("comp -> GTE \n");
	CodeNode *node = new CodeNode;
        node->name = ">=";
	$$ = node;
	}
;

expression: multExpr {
	//printf("expression -> multExpr \n");
	CodeNode *node = new CodeNode;
	CodeNode *multExpr = $1;
	std::string id = multExpr->name;
	//node->code = "";
	node->code += multExpr->code;
	//node->code += id;
	node->name = id;
	//printf("%p\n", $1);
	$$ = node;
	}
  | multExpr ADD expression {
	//printf("expression -> multExpr ADD expression \n");
	CodeNode * node= new CodeNode;
	CodeNode *multExpr = $1;
	CodeNode *expression = $3;
	//node->code = "";
	std::string temp = "_temp" + std::to_string(count_names);
	node->name = temp; //was = temp
	//printf(node->name.c_str());
	node->code += multExpr->code + expression->code;
	node->code += std::string(". ") + temp + std::string("\n") + std::string("+ ") + temp + std::string(", ") + multExpr->name + std::string(", ") + expression->name + std::string("\n");
	//printf(node->code.c_str());
	$$ = node;
	count_names++;
	}
  | multExpr SUB expression {
	//printf("expression -> multExpr SUB expression \n");
	CodeNode *node = new CodeNode;
	CodeNode *multExpr = $1;
        CodeNode *expression = $3;
        //node->code = "";
        std::string temp = "_temp" + std::to_string(count_names);
        node->name = temp;
	node->code += multExpr->code + expression->code;
        node->code += std::string(". ") + temp + std::string("\n") + std::string("- ") + temp + std::string(", ") + multExpr->name + std::string(", ") + expression->name + std::string("\n");	
	$$ = node;
	count_names++;
	} 
;

multExpr: term  {
	//printf("multExpr -> term \n");
	CodeNode *node = new CodeNode;
	CodeNode *term = $1;
	std::string id = term->name;
	node->code += term->code;
	node->name = id;
	//printf(node->code.c_str());
	//printf("%p\n", $1);
	$$ = node;
	}
  | term MULT multExpr {
	//printf("multExpr -> term MULT multExpr \n");
	CodeNode *node = new CodeNode;
	CodeNode *term = $1;
        CodeNode *multExpr = $3;
        //node->code = "";
        std::string temp = "_temp" + std::to_string(count_names);
        node->name = temp;
	node->code += term->code + multExpr->code;
        node->code += std::string(". ") + temp + std::string("\n") + std::string("* ") + temp + std::string(", ") + term->name + std::string(", ") + multExpr->name + std::string("\n");
	//printf(node->code.c_str());
	$$ = node;
	count_names++;
	}
  | term DIV multExpr {
	//printf("multExpr -> term DIV multExpr \n");
	CodeNode *node = new CodeNode;
	CodeNode *term = $1;
        CodeNode *multExpr = $3;
        //node->code = "";
        std::string temp = "_temp" + std::to_string(count_names);
        node->name = temp;
	node->code += term->code + multExpr->code;
        node->code += std::string(". ") + temp + std::string("\n") + std::string("/ ") + temp + std::string(", ") + term->name + std::string(", ") + multExpr->name + std::string("\n");
        $$ = node;
	count_names++;
	}
  | term MOD multExpr {
	//printf("multExpr -> term MOD multExpr \n");
	CodeNode *node = new CodeNode;
	CodeNode *term = $1;
        CodeNode *multExpr = $3;
        //node->code = "";
        std::string temp = "_temp" + std::to_string(count_names);
        node->name = temp;
	node->code += term->code + multExpr->code;
        node->code += std::string(". ") + temp + std::string("\n") + std::string("% ") + temp + std::string(", ") + term->name + std::string(", ") + multExpr->name + std::string("\n");
        $$ = node;
	count_names++;
	}
;

term: var {
	//printf("term -> var \n");
	CodeNode *node = new CodeNode;
	CodeNode *var = $1;
	std::string id = var->name;
	//node->code = "";
	node->code += var->code;
	node->name = id;
	//printf(node->code.c_str());
	//printf("%p\n", $1);
	$$ = node;
	}
  | NUMBER {
	//printf("term -> NUMBER %d\n", $1);
	CodeNode *node = new CodeNode;
	node->name = $1;
	$$ = node;
	}
  | L_PAREN expression R_PAREN {
	//printf("term -> L_PAREN expression R_PAREN \n");
	CodeNode *node = new CodeNode;
	CodeNode *expression = $2;
	node->name = expression->name;
	node->code += expression->code;
	//printf(node->code.c_str());
	$$ = node;
	}
  | ident L_PAREN expression R_PAREN {
	//printf("term -> ident L_PAREN expression R_PAREN \n");
	CodeNode *node = new CodeNode;
	std::string id = $1;
	CodeNode *expression = $3;
	node->name = id;
	node->code += expression->code;
	$$ = node;
	}
  | ident L_PAREN expression COMMA expression R_PAREN {
	//printf("term -> ident L_PAREN expression COMMA R_PAREN \n");
	CodeNode *node = new CodeNode;
	std::string id = $1;
	CodeNode *expression1 = $3;
	CodeNode *expression2 = $5;
	std::string temp = "_temp" + std::to_string(count_names);
	node->name = temp; //was id
	node->code += expression1->code; /* + expression2->code;*/
	node->code += std::string("param ") + expression1->name + std::string("\n");
	node->code += expression2->code; 
	node->code += std::string("param ") + expression2->name + std::string("\n") + std::string(". ") + temp + std::string("\n") + std::string("call ") + id + std::string(", ") + temp + std::string("\n");	
	//printf(node->code.c_str());
	$$ = node;
	count_names++;
	}
  | ident L_PAREN R_PAREN {
	//printf("term -> ident L_PAREN R_PAREN");
	CodeNode *node = new CodeNode;
	$$ = node;
	} 
;

var: ident {//printf("var -> IDENT \n");
	std::string name = $1;
	CodeNode *node = new CodeNode;
	node->code = "";
	node->name = name;
	//printf("%p\n", $1);
	$$ = node;
}
  | ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET {
	//printf("var -> IDENT L_SQUARE_BRACKET expression R_SQUARE_BRACKET \n");
	//write a[0] -> []= _temp1, a, 0
	CodeNode *node = new CodeNode;
        std::string id = $1;
	CodeNode *expression = $3;
	//node->code = "";
	std::string temp = "_temp" + std::to_string(count_names);
	node->name += temp;
	//printf(node->name.c_str());
	node->code += expression->code;
	node->code += std::string(". ") + temp + std::string("\n") + std::string("=[] ") + temp + std::string(", ") + id + std::string(", ") + expression->name + std::string("\n");
	//printf(node->code.c_str());
	$$ = node;
	count_names++;
	}
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
	print_symbol_table();
        return 0;
}

void yyerror(const char *msg) {
  extern int lineCount;
  extern char* yytext;
  printf("Error: on line %d: %s\n", lineCount, msg);
  exit(1);
}
