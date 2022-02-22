%{
	#include <stdio.h>
        #include <math.h>
	#include <string>
        #include "y.tab.h"
        int lineCount = 1;
        int columnCount = 0;
%}

   /* some common rules */
DIGIT   [0-9]


%%
   /* specific lexer rules in regex */
"function"      {return FUNCTION; columnCount += 8;}
"beginparams"   {return BEGIN_PARAMS; columnCount += 11;}
"endparams"      {return END_PARAMS; columnCount += 9;}
"beginlocals"      {return BEGIN_LOCALS; columnCount += 11;}
"endlocals"      {return END_LOCALS; columnCount += 9;}
"beginbody"      {return BEGIN_BODY; columnCount += 9;}
"endbody"      {return END_BODY; columnCount += 7;}
"integer"      {return INTEGER; columnCount += 7;}
"array"      {return ARRAY; columnCount += 5;}
"of"      {return OF; columnCount += 2;}
"if"      {return IF; columnCount += 2;}
"then"      {return THEN; columnCount += 4;}
"endif"      {return ENDIF; columnCount += 5;}
"else"      {return ELSE; columnCount += 4;}
"while"      {return WHILE; columnCount += 5;}
"do"      {return DO; columnCount += 2;}
"beginloop"      {return BEGIN_LOOP; columnCount += 9;}
"endloop"      {return ENDLOOP; columnCount += 7;}
"continue"      {return CONTINUE; columnCount += 8;}
"break"      {return BREAK; columnCount += 5;}
"read"      {return READ; columnCount += 4;}
"write"      {return WRITE; columnCount += 5;}
"not"      {return NOT; columnCount += 3;}
"true"      {return TRUE; columnCount += 4;}
"false"      {return FALSE; columnCount += 5;}
"return"     {return RETURN; columnCount += 6;}

"-"      {return SUB; columnCount += 1;}
"+"      {return ADD; columnCount += 1;}
"*"      {return MULT;  columnCount += 1;}
"/"      {return DIV;  columnCount += 1;}
"%"      {return MOD;  columnCount += 1;}

"=="     {return EQ; columnCount += 2;}
"<>"     {return NEQ; columnCount += 2;}
"<"      {return LT; columnCount += 1;}
">"      {return GT; columnCount += 1;}
"<="     {return LTE; columnCount += 2;}
">="     {return GTE; columnCount += 2;}
"##".+   {printf("");}

[a-zA-z][a-zA-Z0-9]+?_  {printf("Error at line %d, column %d: identifier \"%s\" cannot end with underscore\n", lineCount, columnCount, yytext);}
[a-zA-Z][a-zA-Z0-9_]?+        {char *ptr = new char[yyleng]; strcpy(ptr, yytext); yylval.ident=ptr; return IDENT; columnCount += yyleng;}
{DIGIT}+        {columnCount += yyleng; yylval.num = atoi(yytext); return NUMBER;}

";"      {return SEMICOLON;  columnCount += 1;}
":"      {return COLON;  columnCount += 1;}
","      {return COMMA;  columnCount += 1;}
"("      {return L_PAREN;  columnCount += 1;}
")"      {return R_PAREN;  columnCount += 1;}
"["      {return L_SQUARE_BRACKET;  columnCount += 1;}
"]"      {return R_SQUARE_BRACKET;  columnCount += 1;}
":="     {return ASSIGN;  columnCount += 2;}

[0-9][a-zA-Z0-9]?+      {printf("Error at line %d, column %d: identifier \"%s\" must begin with a letter\n", lineCount, columnCount, yytext);}

\n      {lineCount++; columnCount = 0;}
[\t\r]  {columnCount += 8;}
" "     {columnCount += 1;}

.       //{printf("Error at line %d, column %d: unrecognized symbol \"%s\"\n", lineCount, columnCount, yytext);}

%%
        /* C functions used in lexer */
/*int main(int argc, char ** argv)
{
   yylex();
}*/
