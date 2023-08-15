functor WhileLexFun(structure Tokens: While_TOKENS)  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
COMMENT | WHILE | INITIAL
    structure UserDeclarations = 
      struct

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




      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
Vector.fromList []
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as (fileName:string)) () = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;
      (lin:=1; eolpos:=0;
		   YYBEGIN WHILE; continue ()))
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm; (continue ()))
fun yyAction2 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (lin:=(!lin)+1;
 eolpos:=yypos+size yytext; continue ())
      end
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); Tokens.DOUBLECOLON(!lin,!col)))
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); Tokens.COLON(!lin,!col)))
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); Tokens.SEMICOLON(!lin,!col)))
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); Tokens.COMMA(!lin,!col)))
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); Tokens.LCURL(!lin,!col)))
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); Tokens.RCURL(!lin,!col)))
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); Tokens.ASSIGN(!lin,!col)))
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); Tokens.LPAR(!lin,!col)))
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); Tokens.RPAR(!lin,!col)))
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); Tokens.TILDE(!lin,!col)))
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); Tokens.DOUBLEBAR(!lin,!col)))
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); Tokens.DOUBLEAND(!lin,!col)))
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); Tokens.EXCLAIM(!lin,!col)))
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); Tokens.LT(!lin,!col)))
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); Tokens.LEQ(!lin,!col)))
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); Tokens.EQ(!lin,!col)))
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); Tokens.GT(!lin,!col)))
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); Tokens.GEQ(!lin,!col)))
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); Tokens.NEQ(!lin,!col)))
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); Tokens.PLUS(!lin,!col)))
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); Tokens.MINUS(!lin,!col)))
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); Tokens.TIMES(!lin,!col)))
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); Tokens.DIV(!lin,!col)))
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); Tokens.MOD(!lin,!col)))
fun yyAction27 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (col:=yypos-(!eolpos); Tokens.NUMBER(List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext), !lin,!col))
      end
fun yyAction28 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (col:=yypos-(!eolpos);		if yytext="program" then
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
										Tokens.ID(yytext,!lin,!col))
      end
fun yyAction29 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (col:=yypos-(!eolpos);
	  badCh (fileName,yytext,!lin,!col);
	  Tokens.ILLCH(!lin,!col))
      end
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then if yyInput.eof(!(yystrm))
                  then UserDeclarations.eof(yyarg)
                  else yyAction0(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ2(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if yyInput.eof(!(yystrm))
                  then UserDeclarations.eof(yyarg)
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ2(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"|"
              then yyQ28(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ29(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ29(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ29(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ29(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ29(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ29(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ29(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ29(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction28(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ29(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ29(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ29(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
                  else yyAction28(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ29(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ30(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ32(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
            else if inp < #">"
              then if inp = #"="
                  then yyQ31(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                  else yyAction16(strm, yyNO_MATCH)
              else yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #";"
              then yyAction4(strm, yyNO_MATCH)
            else if inp < #";"
              then if inp = #":"
                  then yyQ33(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp = #"="
              then yyQ34(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ35(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"0"
              then yyAction27(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ35(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ35(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"0"
              then yyAction27(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ35(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"&"
              then yyQ36(strm', yyMATCH(strm, yyAction29, yyNO_MATCH))
              else yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ5(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction1(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ37(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyAction1(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ37(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction1(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ37(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyAction1(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ37(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"-"
              then yyQ15(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < #"-"
              then if inp = #"\""
                  then yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < #"\""
                  then if inp = #"\r"
                      then yyQ6(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                    else if inp < #"\r"
                      then if inp = #"\n"
                          then yyQ5(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                        else if inp < #"\n"
                          then if inp = #"\t"
                              then yyQ4(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                              else yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                          else yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                    else if inp = #" "
                      then yyQ4(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                    else if inp = #"!"
                      then yyQ7(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                      else yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp = #"("
                  then yyQ10(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < #"("
                  then if inp = #"&"
                      then yyQ9(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                    else if inp < #"&"
                      then if inp = #"%"
                          then yyQ8(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                          else yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                      else yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp = #"+"
                  then yyQ13(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < #"+"
                  then if inp = #")"
                      then yyQ11(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                      else yyQ12(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyQ14(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = #"?"
              then yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < #"?"
              then if inp = #";"
                  then yyQ19(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < #";"
                  then if inp = #"0"
                      then yyQ17(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                    else if inp < #"0"
                      then if inp = #"."
                          then yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                          else yyQ16(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                    else if inp = #":"
                      then yyQ18(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                      else yyQ17(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp = #"="
                  then yyQ21(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp = #"<"
                  then yyQ20(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyQ22(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = #"{"
              then yyQ24(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < #"{"
              then if inp = #"["
                  then yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < #"["
                  then if inp <= #"@"
                      then yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                      else yyQ23(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp <= #"`"
                  then yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyQ23(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = #"~"
              then yyQ27(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < #"~"
              then if inp = #"|"
                  then yyQ25(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyQ26(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of COMMENT => yyQ0(!(yystrm), yyNO_MATCH)
    | WHILE => yyQ1(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ2(!(yystrm), yyNO_MATCH)
  (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    fun makeLexer' ins = mk (yyInput.mkStream ins)
    end

  end
