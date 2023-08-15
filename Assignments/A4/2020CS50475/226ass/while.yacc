open Tree
%%
%name While
%term DOUBLECOLON | COLON | SEMICOLON
	| COMMA | RCURL | LCURL | ASSIGN
	| LPAR | RPAR | TILDE | DOUBLEBAR
	| DOUBLEAND | LT | LEQ | EQ | GT
	| GEQ | NEQ | PLUS | MINUS | TIMES
	| DIV | MOD | EXCLAIM | PROGRAM | VAR | INT
	| BOOL| READ | WRITE | IF 
	| THEN | ELSE | ENDIF | WHILE | DO 
	| ENDWH | TT | FF | NUMBER of int | ID of string | EOF | ILLCH
	
%nonterm start of AST|progr of PROG| block of BLK| declarationseq of DEC list| declaration of DEC
	 | type2 of Typ| variablelist of string list
	 | commandseq of CMD list | command of CMD
	 | expression of EXP | s of string list | t of CMD list
	 
%pos int
%eop EOF
%noshift EOF
%nonassoc DOUBLECOLON COLON SEMICOLON COMMA RCURL LCURL ASSIGN LPAR RPAR  
%nonassoc PROGRAM VAR INT BOOL READ WRITE IF THEN ELSE ENDIF WHILE DO ENDWH 
%nonassoc TT FF
%nonassoc DOUBLEBAR
%nonassoc DOUBLEAND
%right EQ
%left LT 
%left LEQ
%left GT 
%left GEQ
%left NEQ
%left TILDE
%left EXCLAIM
%left PLUS MINUS 
%left TIMES DIV MOD 
%start start


%nodefault 
%verbose
%arg (fileName) : string
%%				
start: progr(AST(progr))

progr: PROGRAM ID DOUBLECOLON block (PROG(ID, block))		

block: declarationseq commandseq (BLK(declarationseq, commandseq))	

declarationseq: ([])|declaration declarationseq		(declaration::declarationseq)		

declaration: VAR variablelist COLON type2 SEMICOLON	(DEC("Var", variablelist,type2))	

type2: INT (Int)|BOOL (Bool)

variablelist: ID s(ID::s)									
s:  ([])|COMMA ID s	(ID::s)

commandseq: LCURL t RCURL (t)									
t:  ([])|command SEMICOLON t	  (command::t)	

command: ID ASSIGN expression (SET(ID, expression))|								
	 READ ID (INPUT(ID))|									
	 WRITE expression (OUTPUT(expression))|									
	 IF expression THEN commandseq							
	 ELSE commandseq									
	 ENDIF (ITE(expression, commandseq1, commandseq2))|										
	 WHILE expression DO commandseq							
	 ENDWH (WH(expression, commandseq))

expression:     expression PLUS expression (BIN(PLUS,expression1, expression2)) | 
                expression MINUS expression (BIN(MINUS,expression1, expression2)) | 
                expression TIMES expression (BIN(TIMES,expression1, expression2)) |
                expression DIV expression (BIN(DIV,expression1, expression2))  | 
                expression MOD expression (BIN(MOD,expression1, expression2))   | 
                expression DOUBLEAND expression (BIN(AND,expression1, expression2)) | 
                expression DOUBLEBAR expression (BIN(OR,expression1, expression2)) |
                expression LT expression (BIN(LT,expression1, expression2))   |
                expression LEQ expression (BIN(LEQ,expression1, expression2))   | 
                expression EQ expression (BIN(EQ,expression1, expression2)) |
                expression GT expression (BIN(GT,expression1, expression2))   | 
                expression GEQ expression (BIN(GEQ,expression1, expression2))   |
                expression NEQ expression (BIN(NEQ,expression1, expression2)) |
                TILDE expression (UN(TILDE,expression)) | 
                EXCLAIM expression(UN(NOT,expression))|
                LPAR expression RPAR (expression) |
                NUMBER (INT(NUMBER)) |
                ID (Variable(ID)) | 
                TT (Boolean("tt")) |
                FF (Boolean("ff")) 
