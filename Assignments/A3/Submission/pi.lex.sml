functor PiLexFun(structure Tokens: Pi_TOKENS)  = struct

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
PI | COMMENT | INITIAL
    structure UserDeclarations = 
      struct

structure T = Tokens

type pos = int
type svalue = t.svalue
type ('a,'b) token = ('a,'b) T.token
type lexresult = (svalue,pos) token
type lexarg = string
type arg = lexarg

val lin = ref 1;
val col = ref 0;
val eolpos = ref 0;

val badCh : string * string * int * int -> unit = fn
	(fileName, bad, line, col) =>
	TextIO.output(TextIO.stdOut,fileName^"["
	^Int.toString line^"."^Int.toString col
	^"] Invalid character \""^bad^"\"\n");
val eof = fn fileName => T.EOF (!lin,!col);

structure KeyWord :
sig val find : string ->
		(int * int -> (svalue,int) token) option
end =
struct
 val TableSize = 422
 val HashFactor = 5
 val hash = fn
     s => List.foldr (fn (c,v) => 
       (v*HashFactor+(ord c)) mod TableSize) 0 (explode s)
 val HashTable = Array.array(TableSize,nil) :
 		(string * (int * int -> (svalue, int) token))
 		list Array.array
 val add = fn
     (s,v) => let val i = hash s
     	      in Array.update(HashTable,i,(s,v)
     	         :: (Array.sub(HashTable, i)))
     	         end
 val find = fn
     s => let val i = hash s
     	       fun f ((key,v)::r) = if s=key then SOME v
     	       		     else f r
     	         | f nil = NONE
     	   in f (Array.sub(HashTable, i))
     	   end
 val _ = (List.app add [
       ("new",         T.NEW)
       ])
end;

open Keyword;



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
                   YYBEGIN PI; continue ()))
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm; (continue ()))
fun yyAction2 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (lin:=(!lin)+1;
		eolpos:=yypos+size yytext; continue ())
      end
fun yyAction3 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (case find yytext of
		    SOME v => (col:yypos -(!eolpos);
		    		v(!lin,!col))
		 | _ 	    => (col:=yypos-(!eolpos);
		 		T.IDE(yytext,!lin,!col)))
      end
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN COMMENT; continue ()))
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.EQUALS(!lin,!col)))
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.LPAR(!lin,!col)))
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.RPAR(!lin,!col)))
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.OUTPUT(!lin,!col)))
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.INPUT(!lin,!col)))
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.DVBAR(!lin,!col)))
fun yyAction11 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (col:=yypos-(!eolpos);
		  badCh (fileName,yytext,!lin,!col);
		  T.ILLCH(!lin,!col))
      end
fun yyAction12 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (lin:=(!lin)+1;eolpos:=yypos+size yytext;
                   YYBEGIN PI; continue ())
      end
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm; (continue ()))
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
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ19(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
              else yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyQ18(strm', lastMatch)
            else if inp < #"\v"
              then if inp = #"\n"
                  then yyQ19(strm', lastMatch)
                  else yyQ18(strm', lastMatch)
            else if inp = #"\r"
              then yyQ20(strm', lastMatch)
              else yyQ18(strm', lastMatch)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"|"
              then yyQ15(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
              else yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction3(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp <= #"@"
                  then yyAction3(strm, yyNO_MATCH)
                  else yyQ16(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp = #"a"
              then yyQ16(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp < #"a"
              then yyAction3(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ16(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction3(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp <= #"@"
                  then yyAction3(strm, yyNO_MATCH)
                  else yyQ16(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp = #"a"
              then yyQ16(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
            else if inp < #"a"
              then yyAction3(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ16(strm', yyMATCH(strm, yyAction3, yyNO_MATCH))
              else yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction8(strm, yyNO_MATCH)
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
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction1(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ17(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyAction1(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ17(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction1(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ17(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyAction1(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ17(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #")"
              then yyQ10(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < #")"
              then if inp = #" "
                  then yyQ4(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < #" "
                  then if inp = #"\v"
                      then yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                    else if inp < #"\v"
                      then if inp = #"\t"
                          then yyQ4(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                        else if inp = #"\n"
                          then yyQ5(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                          else yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                    else if inp = #"\r"
                      then yyQ6(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                      else yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp = #"%"
                  then yyQ8(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < #"%"
                  then if inp = #"!"
                      then yyQ7(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                      else yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp = #"("
                  then yyQ9(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = #"A"
              then yyQ13(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #">"
                  then yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < #">"
                  then if inp = #"="
                      then yyQ11(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                      else yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp = #"?"
                  then yyQ12(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = #"{"
              then yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < #"{"
              then if inp = #"["
                  then yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < #"["
                  then yyQ13(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp <= #"`"
                  then yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyQ13(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = #"|"
              then yyQ14(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
      (* end case *))
in
  (case (!(yyss))
   of PI => yyQ0(!(yystrm), yyNO_MATCH)
    | COMMENT => yyQ1(!(yystrm), yyNO_MATCH)
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
