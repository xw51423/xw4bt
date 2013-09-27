
/* Tokens.  */
#define GO 258
#define TURN 259
#define VAR 260
#define JUMP 261
#define FOR 262
#define STEP 263
#define TO 264
#define DO 265
#define COPEN 266
#define CCLOSE 267
#define SIN 268
#define COS 269
#define SQRT 270
#define FLOAT 271
#define ID 272
#define NUMBER 273
#define SEMICOLON 274
#define PLUS 275
#define MINUS 276
#define TIMES 277
#define DIV 278
#define OPEN 279
#define CLOSE 280
#define ASSIGN 281

#define IF 282
#define THEN 283
#define ELSE 284
#define PROCEDURE 285
#define LEFTBRAKETS 286
#define RIGHTBRAKETS 287
#define COMMA 288
#define BIGGER 289
#define SMALLER 290
#define BIGGEROREQUAL 291
#define SMALLEROREQUAL 292
#define WHILE 293
#define EQUAL 294
#define NOTEQUAL 295
#define CALL 296
#define PARAM 297



typedef union YYSTYPE
{ int i; node *n; double d;}
        YYSTYPE;
YYSTYPE yylval;

