structure T = Tokens

  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token
  type lexresult = (svalue,pos) token
  type lexarg = string
  type arg = lexarg

  val lin = ref 1;
val col = ref 0;
val eolpos = ref 0;

val badCh : string * string * int * int -> unit = fn
  (fileName,bad,line,col) =>
    TextIO.output(TextIO.stdOut,fileName^"["
		  ^Int.toString line^"."^Int.toString col
		  ^"] Invalid character \""^bad^"\"\n");
val eof = fn fileName => Tokens.EOF(!lin,!col);


%%
%full
%header (functor WhileLexFun(structure Tokens: While_TOKENS));
%arg (fileName:string);
%s WHILE COMMENT;
alpha = [A-Za-z];
digit = [0-9];
alphanumeric = [A-Za-z0-9];
ws = [\ \t];
eol = ("\013\010"|"\010"|"\013");
%%
<INITIAL>{ws}* => (lin:=1; eolpos:=0;
		   YYBEGIN WHILE; continue ());
<WHILE>{ws}* => (continue ());
<WHILE>{eol} => (lin:=(!lin)+1;
 eolpos:=yypos+size yytext; continue ());

<WHILE>"::" => (col:=yypos-(!eolpos); Tokens.DOUBLECOLON(!lin,!col));
<WHILE>":" => (col:=yypos-(!eolpos); Tokens.COLON(!lin,!col));
<WHILE>";" => (col:=yypos-(!eolpos); Tokens.SEMICOLON(!lin,!col));
<WHILE>"," => (col:=yypos-(!eolpos); Tokens.COMMA(!lin,!col));
<WHILE>"{" => (col:=yypos-(!eolpos); Tokens.LCURL(!lin,!col));
<WHILE>"}" => (col:=yypos-(!eolpos); Tokens.RCURL(!lin,!col));
<WHILE>":=" => (col:=yypos-(!eolpos); Tokens.ASSIGN(!lin,!col));
<WHILE>"(" => (col:=yypos-(!eolpos); Tokens.LPAR(!lin,!col));
<WHILE>")" => (col:=yypos-(!eolpos); Tokens.RPAR(!lin,!col));
<WHILE>"~" => (col:=yypos-(!eolpos); Tokens.TILDE(!lin,!col));
<WHILE>"||" => (col:=yypos-(!eolpos); Tokens.DOUBLEBAR(!lin,!col));
<WHILE>"&&" => (col:=yypos-(!eolpos); Tokens.DOUBLEAND(!lin,!col));
<WHILE>"!" => (col:=yypos-(!eolpos); Tokens.EXCLAIM(!lin,!col));
<WHILE>"<" => (col:=yypos-(!eolpos); Tokens.LT(!lin,!col));
<WHILE>"<=" => (col:=yypos-(!eolpos); Tokens.LEQ(!lin,!col));
<WHILE>"=" => (col:=yypos-(!eolpos); Tokens.EQ(!lin,!col));
<WHILE>">" => (col:=yypos-(!eolpos); Tokens.GT(!lin,!col));
<WHILE>">=" => (col:=yypos-(!eolpos); Tokens.GEQ(!lin,!col));
<WHILE>"<>" => (col:=yypos-(!eolpos); Tokens.NEQ(!lin,!col));
<WHILE>"+" => (col:=yypos-(!eolpos); Tokens.PLUS(!lin,!col));
<WHILE>"-" => (col:=yypos-(!eolpos); Tokens.MINUS(!lin,!col));
<WHILE>"*" => (col:=yypos-(!eolpos); Tokens.TIMES(!lin,!col));
<WHILE>"/" => (col:=yypos-(!eolpos); Tokens.DIV(!lin,!col));
<WHILE>"%" => (col:=yypos-(!eolpos); Tokens.MOD(!lin,!col));
<WHILE>{digit}+ =>  (col:=yypos-(!eolpos); Tokens.NUMBER(List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext), !lin,!col));
<WHILE>{alpha}{alphanumeric}* => (col:=yypos-(!eolpos);		if yytext="program" then
										Tokens.PROGRAM(!lin,!col)
									else if yytext="var" then
										Tokens.VAR(!lin,!col)
									else if yytext="int" then
										Tokens.INT(!lin,!col)
									else if yytext="bool" then
										Tokens.BOOL(!lin,!col)
									else if yytext="read" then
										Tokens.READ(!lin,!col)
									else if yytext="write" then
										Tokens.WRITE(!lin,!col)
									else if yytext="if" then
										Tokens.IF(!lin,!col)
									else if yytext="then" then
										Tokens.THEN(!lin,!col)
									else if yytext="else" then
										Tokens.ELSE(!lin,!col)
									else if yytext="endif" then
										Tokens.ENDIF(!lin,!col)
									else if yytext="while" then
										Tokens.WHILE(!lin,!col)
									else if yytext="do" then
										Tokens.DO(!lin,!col)
									else if yytext="endwh" then
										Tokens.ENDWH(!lin,!col)
									else if yytext="tt" then
										Tokens.TT(!lin,!col)
									else if yytext="ff" then
										Tokens.FF(!lin,!col)
									else
										Tokens.ID(yytext,!lin,!col));
<WHILE>. => (col:=yypos-(!eolpos);
	  badCh (fileName,yytext,!lin,!col);
	  Tokens.ILLCH(!lin,!col));
