(* compiler.sml *)
structure WhileLrVals = WhileLrValsFun(
    structure Token = LrParser.Token);

structure WhileLex = WhileLexFun(
    structure Tokens = WhileLrVals.Tokens);
    
structure WhileParser = JoinWithArg(
    structure ParserData = WhileLrVals.ParserData
    structure Lex=WhileLex
    structure LrParser=LrParser);
    
structure While :
sig val compile : string -> Tree.AST
end =
struct
exception WhileError;
fun compile (fileName) =
    let val inStream = TextIO.openIn fileName;
    	val grab : int -> string = fn
    	    n => if TextIO.endOfStream inStream
    	    	 then ""
    	    	 else TextIO.inputN (inStream,n);
    	val printError : string * int * int -> unit = fn
    	    (msg,line,col) =>
    	    print (fileName^"["^Int.toString line^":"
    	    	   ^Int.toString col^"] "^msg^"\n");
    	val _ = Control.Print.printDepth:=1000;
    	val (tree,rem) = WhileParser.parse
    		      (15,
    		      (WhileParser.makeLexer grab fileName),
    		      printError,
    		      fileName)
    	    handle WhileParser.ParseError => raise WhileError;
    	val _ = TextIO.closeIn inStream;
   in tree
   end
end;
