
%{
#include "symtab.h"
#include "token.h"
%}

%%

\/\/.*          ;
[[:space:]]     ;

GO              return GO;
TURN            return TURN; 
VAR             return VAR;
JUMP            return JUMP;
FOR             return FOR;
STEP            return STEP;
TO              return TO;
DO              return DO;
BEGIN           return COPEN;
END             return CCLOSE;

sin             return SIN;
cos             return COS;
sqrt            return SQRT;

;               return SEMICOLON;
\+              return PLUS;
-               return MINUS;
\*              return TIMES;
\/              return DIV;
\(              return OPEN;
\)              return CLOSE;
=               return ASSIGN;

[a-z][a-zA-Z0-9]*          { yylval.n = lookup(yytext); 
                                if (yylval.n==NULL)
								yylval.n = insert(yytext);
								return ID;
							  }
    
[0-9]+\.[0-9]+  yylval.d = atof(yytext);  return FLOAT;
[0-9]+          yylval.i = atoi(yytext);  return NUMBER;

WHILE           return WHILE;
IF              return IF;
THEN            return THEN;
ELSE            return ELSE;
\{              return LEFTBRAKETS;
\}              return RIGHTBRAKETS;
,               return COMMA;
PROCEDURE       return PROCEDURE;
\>              return BIGGER;
\<              return SMALLER;
\>=             return BIGGEROREQUAL;
\<=             return SMALLEROREQUAL;
==              return EQUAL;


.               fprintf(stderr,"Unexpected character %c (%2X)\n",yytext[0],yytext[0]);


%%
