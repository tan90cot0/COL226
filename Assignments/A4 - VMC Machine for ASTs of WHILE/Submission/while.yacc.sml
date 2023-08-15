functor WhileLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : While_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
open Tree

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\006\000\000\000\
\\001\000\002\000\095\000\000\000\
\\001\000\002\000\096\000\004\000\025\000\000\000\
\\001\000\002\000\097\000\000\000\
\\001\000\002\000\023\000\000\000\
\\001\000\003\000\093\000\000\000\
\\001\000\003\000\094\000\000\000\
\\001\000\003\000\101\000\011\000\059\000\012\000\058\000\013\000\057\000\
\\014\000\056\000\015\000\055\000\016\000\054\000\017\000\053\000\
\\018\000\052\000\019\000\051\000\020\000\050\000\021\000\049\000\
\\022\000\048\000\023\000\047\000\000\000\
\\001\000\003\000\102\000\000\000\
\\001\000\003\000\103\000\011\000\059\000\012\000\058\000\013\000\057\000\
\\014\000\056\000\015\000\055\000\016\000\054\000\017\000\053\000\
\\018\000\052\000\019\000\051\000\020\000\050\000\021\000\049\000\
\\022\000\048\000\023\000\047\000\000\000\
\\001\000\003\000\104\000\000\000\
\\001\000\003\000\105\000\000\000\
\\001\000\003\000\106\000\009\000\106\000\011\000\106\000\012\000\106\000\
\\013\000\106\000\014\000\106\000\015\000\106\000\016\000\106\000\
\\017\000\106\000\018\000\106\000\019\000\106\000\020\000\106\000\
\\021\000\049\000\022\000\048\000\023\000\047\000\032\000\106\000\
\\036\000\106\000\000\000\
\\001\000\003\000\107\000\009\000\107\000\011\000\107\000\012\000\107\000\
\\013\000\107\000\014\000\107\000\015\000\107\000\016\000\107\000\
\\017\000\107\000\018\000\107\000\019\000\107\000\020\000\107\000\
\\021\000\049\000\022\000\048\000\023\000\047\000\032\000\107\000\
\\036\000\107\000\000\000\
\\001\000\003\000\108\000\009\000\108\000\011\000\108\000\012\000\108\000\
\\013\000\108\000\014\000\108\000\015\000\108\000\016\000\108\000\
\\017\000\108\000\018\000\108\000\019\000\108\000\020\000\108\000\
\\021\000\108\000\022\000\108\000\023\000\108\000\032\000\108\000\
\\036\000\108\000\000\000\
\\001\000\003\000\109\000\009\000\109\000\011\000\109\000\012\000\109\000\
\\013\000\109\000\014\000\109\000\015\000\109\000\016\000\109\000\
\\017\000\109\000\018\000\109\000\019\000\109\000\020\000\109\000\
\\021\000\109\000\022\000\109\000\023\000\109\000\032\000\109\000\
\\036\000\109\000\000\000\
\\001\000\003\000\110\000\009\000\110\000\011\000\110\000\012\000\110\000\
\\013\000\110\000\014\000\110\000\015\000\110\000\016\000\110\000\
\\017\000\110\000\018\000\110\000\019\000\110\000\020\000\110\000\
\\021\000\110\000\022\000\110\000\023\000\110\000\032\000\110\000\
\\036\000\110\000\000\000\
\\001\000\003\000\111\000\009\000\111\000\011\000\111\000\013\000\057\000\
\\014\000\056\000\015\000\055\000\016\000\054\000\017\000\053\000\
\\018\000\052\000\019\000\051\000\020\000\050\000\021\000\049\000\
\\022\000\048\000\023\000\047\000\032\000\111\000\036\000\111\000\000\000\
\\001\000\003\000\112\000\009\000\112\000\012\000\058\000\013\000\057\000\
\\014\000\056\000\015\000\055\000\016\000\054\000\017\000\053\000\
\\018\000\052\000\019\000\051\000\020\000\050\000\021\000\049\000\
\\022\000\048\000\023\000\047\000\032\000\112\000\036\000\112\000\000\000\
\\001\000\003\000\113\000\009\000\113\000\011\000\113\000\012\000\113\000\
\\013\000\113\000\014\000\056\000\015\000\113\000\016\000\054\000\
\\017\000\053\000\018\000\052\000\019\000\051\000\020\000\050\000\
\\021\000\049\000\022\000\048\000\023\000\047\000\032\000\113\000\
\\036\000\113\000\000\000\
\\001\000\003\000\114\000\009\000\114\000\011\000\114\000\012\000\114\000\
\\013\000\114\000\014\000\114\000\015\000\114\000\016\000\054\000\
\\017\000\053\000\018\000\052\000\019\000\051\000\020\000\050\000\
\\021\000\049\000\022\000\048\000\023\000\047\000\032\000\114\000\
\\036\000\114\000\000\000\
\\001\000\003\000\115\000\009\000\115\000\011\000\115\000\012\000\115\000\
\\013\000\057\000\014\000\056\000\015\000\055\000\016\000\054\000\
\\017\000\053\000\018\000\052\000\019\000\051\000\020\000\050\000\
\\021\000\049\000\022\000\048\000\023\000\047\000\032\000\115\000\
\\036\000\115\000\000\000\
\\001\000\003\000\116\000\009\000\116\000\011\000\116\000\012\000\116\000\
\\013\000\116\000\014\000\116\000\015\000\116\000\016\000\116\000\
\\017\000\053\000\018\000\052\000\019\000\051\000\020\000\050\000\
\\021\000\049\000\022\000\048\000\023\000\047\000\032\000\116\000\
\\036\000\116\000\000\000\
\\001\000\003\000\117\000\009\000\117\000\011\000\117\000\012\000\117\000\
\\013\000\117\000\014\000\117\000\015\000\117\000\016\000\117\000\
\\017\000\117\000\018\000\052\000\019\000\051\000\020\000\050\000\
\\021\000\049\000\022\000\048\000\023\000\047\000\032\000\117\000\
\\036\000\117\000\000\000\
\\001\000\003\000\118\000\009\000\118\000\011\000\118\000\012\000\118\000\
\\013\000\118\000\014\000\118\000\015\000\118\000\016\000\118\000\
\\017\000\118\000\018\000\118\000\019\000\051\000\020\000\050\000\
\\021\000\049\000\022\000\048\000\023\000\047\000\032\000\118\000\
\\036\000\118\000\000\000\
\\001\000\003\000\119\000\009\000\119\000\011\000\119\000\012\000\119\000\
\\013\000\119\000\014\000\119\000\015\000\119\000\016\000\119\000\
\\017\000\119\000\018\000\119\000\019\000\051\000\020\000\050\000\
\\021\000\049\000\022\000\048\000\023\000\047\000\032\000\119\000\
\\036\000\119\000\000\000\
\\001\000\003\000\120\000\009\000\120\000\011\000\120\000\012\000\120\000\
\\013\000\120\000\014\000\120\000\015\000\120\000\016\000\120\000\
\\017\000\120\000\018\000\120\000\019\000\051\000\020\000\050\000\
\\021\000\049\000\022\000\048\000\023\000\047\000\032\000\120\000\
\\036\000\120\000\000\000\
\\001\000\003\000\121\000\009\000\121\000\011\000\121\000\012\000\121\000\
\\013\000\121\000\014\000\121\000\015\000\121\000\016\000\121\000\
\\017\000\121\000\018\000\121\000\019\000\121\000\020\000\121\000\
\\021\000\121\000\022\000\121\000\023\000\121\000\032\000\121\000\
\\036\000\121\000\000\000\
\\001\000\003\000\122\000\009\000\122\000\011\000\122\000\012\000\122\000\
\\013\000\122\000\014\000\122\000\015\000\122\000\016\000\122\000\
\\017\000\122\000\018\000\122\000\019\000\122\000\020\000\122\000\
\\021\000\122\000\022\000\122\000\023\000\122\000\032\000\122\000\
\\036\000\122\000\000\000\
\\001\000\003\000\123\000\009\000\123\000\011\000\123\000\012\000\123\000\
\\013\000\123\000\014\000\123\000\015\000\123\000\016\000\123\000\
\\017\000\123\000\018\000\123\000\019\000\123\000\020\000\123\000\
\\021\000\123\000\022\000\123\000\023\000\123\000\032\000\123\000\
\\036\000\123\000\000\000\
\\001\000\003\000\124\000\009\000\124\000\011\000\124\000\012\000\124\000\
\\013\000\124\000\014\000\124\000\015\000\124\000\016\000\124\000\
\\017\000\124\000\018\000\124\000\019\000\124\000\020\000\124\000\
\\021\000\124\000\022\000\124\000\023\000\124\000\032\000\124\000\
\\036\000\124\000\000\000\
\\001\000\003\000\125\000\009\000\125\000\011\000\125\000\012\000\125\000\
\\013\000\125\000\014\000\125\000\015\000\125\000\016\000\125\000\
\\017\000\125\000\018\000\125\000\019\000\125\000\020\000\125\000\
\\021\000\125\000\022\000\125\000\023\000\125\000\032\000\125\000\
\\036\000\125\000\000\000\
\\001\000\003\000\027\000\000\000\
\\001\000\003\000\064\000\000\000\
\\001\000\005\000\099\000\029\000\022\000\030\000\021\000\031\000\020\000\
\\035\000\019\000\041\000\018\000\000\000\
\\001\000\005\000\100\000\000\000\
\\001\000\005\000\026\000\000\000\
\\001\000\006\000\090\000\026\000\010\000\000\000\
\\001\000\006\000\091\000\000\000\
\\001\000\006\000\092\000\026\000\092\000\000\000\
\\001\000\006\000\013\000\000\000\
\\001\000\007\000\028\000\000\000\
\\001\000\008\000\036\000\010\000\035\000\024\000\034\000\038\000\033\000\
\\039\000\032\000\040\000\031\000\041\000\030\000\000\000\
\\001\000\009\000\080\000\011\000\059\000\012\000\058\000\013\000\057\000\
\\014\000\056\000\015\000\055\000\016\000\054\000\017\000\053\000\
\\018\000\052\000\019\000\051\000\020\000\050\000\021\000\049\000\
\\022\000\048\000\023\000\047\000\000\000\
\\001\000\011\000\059\000\012\000\058\000\013\000\057\000\014\000\056\000\
\\015\000\055\000\016\000\054\000\017\000\053\000\018\000\052\000\
\\019\000\051\000\020\000\050\000\021\000\049\000\022\000\048\000\
\\023\000\047\000\032\000\063\000\000\000\
\\001\000\011\000\059\000\012\000\058\000\013\000\057\000\014\000\056\000\
\\015\000\055\000\016\000\054\000\017\000\053\000\018\000\052\000\
\\019\000\051\000\020\000\050\000\021\000\049\000\022\000\048\000\
\\023\000\047\000\036\000\046\000\000\000\
\\001\000\025\000\004\000\000\000\
\\001\000\027\000\042\000\028\000\041\000\000\000\
\\001\000\033\000\098\000\034\000\098\000\037\000\098\000\042\000\098\000\000\000\
\\001\000\033\000\083\000\000\000\
\\001\000\034\000\085\000\000\000\
\\001\000\037\000\082\000\000\000\
\\001\000\041\000\005\000\000\000\
\\001\000\041\000\015\000\000\000\
\\001\000\041\000\039\000\000\000\
\\001\000\041\000\043\000\000\000\
\\001\000\042\000\000\000\000\000\
\\001\000\042\000\087\000\000\000\
\\001\000\042\000\088\000\000\000\
\\001\000\042\000\089\000\000\000\
\"
val actionRowNumbers =
"\046\000\057\000\052\000\000\000\
\\037\000\037\000\040\000\058\000\
\\053\000\038\000\059\000\034\000\
\\004\000\002\000\036\000\032\000\
\\041\000\042\000\042\000\042\000\
\\054\000\047\000\001\000\055\000\
\\048\000\034\000\042\000\045\000\
\\029\000\028\000\031\000\030\000\
\\042\000\042\000\042\000\044\000\
\\009\000\008\000\033\000\006\000\
\\005\000\002\000\035\000\007\000\
\\040\000\042\000\042\000\042\000\
\\042\000\042\000\042\000\042\000\
\\042\000\042\000\042\000\042\000\
\\042\000\042\000\026\000\025\000\
\\043\000\040\000\039\000\003\000\
\\051\000\016\000\015\000\014\000\
\\013\000\012\000\024\000\023\000\
\\022\000\021\000\020\000\019\000\
\\017\000\018\000\027\000\049\000\
\\011\000\040\000\050\000\010\000\
\\056\000"
val gotoT =
"\
\\001\000\084\000\002\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\007\000\004\000\006\000\005\000\005\000\000\000\
\\004\000\009\000\005\000\005\000\000\000\
\\008\000\010\000\000\000\
\\000\000\
\\007\000\012\000\000\000\
\\000\000\
\\000\000\
\\009\000\015\000\012\000\014\000\000\000\
\\000\000\
\\011\000\022\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\027\000\000\000\
\\010\000\035\000\000\000\
\\010\000\036\000\000\000\
\\000\000\
\\006\000\038\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\015\000\012\000\042\000\000\000\
\\010\000\043\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\058\000\000\000\
\\010\000\059\000\000\000\
\\010\000\060\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\063\000\000\000\
\\000\000\
\\000\000\
\\008\000\064\000\000\000\
\\010\000\065\000\000\000\
\\010\000\066\000\000\000\
\\010\000\067\000\000\000\
\\010\000\068\000\000\000\
\\010\000\069\000\000\000\
\\010\000\070\000\000\000\
\\010\000\071\000\000\000\
\\010\000\072\000\000\000\
\\010\000\073\000\000\000\
\\010\000\074\000\000\000\
\\010\000\075\000\000\000\
\\010\000\076\000\000\000\
\\010\000\077\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\079\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\082\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 85
val numrules = 39
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = string
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | ID of unit ->  (string) | NUMBER of unit ->  (int)
 | t of unit ->  (CMD list) | s of unit ->  (string list)
 | expression of unit ->  (EXP) | command of unit ->  (CMD)
 | commandseq of unit ->  (CMD list)
 | variablelist of unit ->  (string list) | type2 of unit ->  (Typ)
 | declaration of unit ->  (DEC)
 | declarationseq of unit ->  (DEC list) | block of unit ->  (BLK)
 | progr of unit ->  (PROG) | start of unit ->  (AST)
end
type svalue = MlyValue.svalue
type result = AST
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 41) => true | _ => false
val showTerminal =
fn (T 0) => "DOUBLECOLON"
  | (T 1) => "COLON"
  | (T 2) => "SEMICOLON"
  | (T 3) => "COMMA"
  | (T 4) => "RCURL"
  | (T 5) => "LCURL"
  | (T 6) => "ASSIGN"
  | (T 7) => "LPAR"
  | (T 8) => "RPAR"
  | (T 9) => "TILDE"
  | (T 10) => "DOUBLEBAR"
  | (T 11) => "DOUBLEAND"
  | (T 12) => "LT"
  | (T 13) => "LEQ"
  | (T 14) => "EQ"
  | (T 15) => "GT"
  | (T 16) => "GEQ"
  | (T 17) => "NEQ"
  | (T 18) => "PLUS"
  | (T 19) => "MINUS"
  | (T 20) => "TIMES"
  | (T 21) => "DIV"
  | (T 22) => "MOD"
  | (T 23) => "EXCLAIM"
  | (T 24) => "PROGRAM"
  | (T 25) => "VAR"
  | (T 26) => "INT"
  | (T 27) => "BOOL"
  | (T 28) => "READ"
  | (T 29) => "WRITE"
  | (T 30) => "IF"
  | (T 31) => "THEN"
  | (T 32) => "ELSE"
  | (T 33) => "ENDIF"
  | (T 34) => "WHILE"
  | (T 35) => "DO"
  | (T 36) => "ENDWH"
  | (T 37) => "TT"
  | (T 38) => "FF"
  | (T 39) => "NUMBER"
  | (T 40) => "ID"
  | (T 41) => "EOF"
  | (T 42) => "ILLCH"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 42) $$ (T 41) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34)
 $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27)
 $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20)
 $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13)
 $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ 
(T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (fileName):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.progr progr1, progr1left, progr1right)) :: 
rest671)) => let val  result = MlyValue.start (fn _ => let val  (progr
 as progr1) = progr1 ()
 in (AST(progr))
end)
 in ( LrTable.NT 0, ( result, progr1left, progr1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.block block1, _, block1right)) :: _ :: ( _, 
( MlyValue.ID ID1, _, _)) :: ( _, ( _, PROGRAM1left, _)) :: rest671))
 => let val  result = MlyValue.progr (fn _ => let val  (ID as ID1) = 
ID1 ()
 val  (block as block1) = block1 ()
 in (PROG(ID, block))
end)
 in ( LrTable.NT 1, ( result, PROGRAM1left, block1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.commandseq commandseq1, _, commandseq1right)
) :: ( _, ( MlyValue.declarationseq declarationseq1, 
declarationseq1left, _)) :: rest671)) => let val  result = 
MlyValue.block (fn _ => let val  (declarationseq as declarationseq1) =
 declarationseq1 ()
 val  (commandseq as commandseq1) = commandseq1 ()
 in (BLK(declarationseq, commandseq))
end)
 in ( LrTable.NT 2, ( result, declarationseq1left, commandseq1right), 
rest671)
end
|  ( 3, ( rest671)) => let val  result = MlyValue.declarationseq (fn _
 => ([]))
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
|  ( 4, ( ( _, ( MlyValue.declarationseq declarationseq1, _, 
declarationseq1right)) :: ( _, ( MlyValue.declaration declaration1, 
declaration1left, _)) :: rest671)) => let val  result = 
MlyValue.declarationseq (fn _ => let val  (declaration as declaration1
) = declaration1 ()
 val  (declarationseq as declarationseq1) = declarationseq1 ()
 in (declaration::declarationseq)
end)
 in ( LrTable.NT 3, ( result, declaration1left, declarationseq1right),
 rest671)
end
|  ( 5, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.type2 
type21, _, _)) :: _ :: ( _, ( MlyValue.variablelist variablelist1, _,
 _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = 
MlyValue.declaration (fn _ => let val  (variablelist as variablelist1)
 = variablelist1 ()
 val  (type2 as type21) = type21 ()
 in (DEC("Var", variablelist,type2))
end)
 in ( LrTable.NT 4, ( result, VAR1left, SEMICOLON1right), rest671)
end
|  ( 6, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  
result = MlyValue.type2 (fn _ => (Int))
 in ( LrTable.NT 5, ( result, INT1left, INT1right), rest671)
end
|  ( 7, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  
result = MlyValue.type2 (fn _ => (Bool))
 in ( LrTable.NT 5, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.s s1, _, s1right)) :: ( _, ( MlyValue.ID ID1
, ID1left, _)) :: rest671)) => let val  result = MlyValue.variablelist
 (fn _ => let val  (ID as ID1) = ID1 ()
 val  (s as s1) = s1 ()
 in (ID::s)
end)
 in ( LrTable.NT 6, ( result, ID1left, s1right), rest671)
end
|  ( 9, ( rest671)) => let val  result = MlyValue.s (fn _ => ([]))
 in ( LrTable.NT 10, ( result, defaultPos, defaultPos), rest671)
end
|  ( 10, ( ( _, ( MlyValue.s s1, _, s1right)) :: ( _, ( MlyValue.ID 
ID1, _, _)) :: ( _, ( _, COMMA1left, _)) :: rest671)) => let val  
result = MlyValue.s (fn _ => let val  (ID as ID1) = ID1 ()
 val  (s as s1) = s1 ()
 in (ID::s)
end)
 in ( LrTable.NT 10, ( result, COMMA1left, s1right), rest671)
end
|  ( 11, ( ( _, ( _, _, RCURL1right)) :: ( _, ( MlyValue.t t1, _, _))
 :: ( _, ( _, LCURL1left, _)) :: rest671)) => let val  result = 
MlyValue.commandseq (fn _ => let val  (t as t1) = t1 ()
 in (t)
end)
 in ( LrTable.NT 7, ( result, LCURL1left, RCURL1right), rest671)
end
|  ( 12, ( rest671)) => let val  result = MlyValue.t (fn _ => ([]))
 in ( LrTable.NT 11, ( result, defaultPos, defaultPos), rest671)
end
|  ( 13, ( ( _, ( MlyValue.t t1, _, t1right)) :: _ :: ( _, ( 
MlyValue.command command1, command1left, _)) :: rest671)) => let val  
result = MlyValue.t (fn _ => let val  (command as command1) = command1
 ()
 val  (t as t1) = t1 ()
 in (command::t)
end)
 in ( LrTable.NT 11, ( result, command1left, t1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.expression expression1, _, expression1right
)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let
 val  result = MlyValue.command (fn _ => let val  (ID as ID1) = ID1 ()
 val  (expression as expression1) = expression1 ()
 in (SET(ID, expression))
end)
 in ( LrTable.NT 8, ( result, ID1left, expression1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: ( _, ( _, 
READ1left, _)) :: rest671)) => let val  result = MlyValue.command (fn
 _ => let val  (ID as ID1) = ID1 ()
 in (INPUT(ID))
end)
 in ( LrTable.NT 8, ( result, READ1left, ID1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.expression expression1, _, expression1right
)) :: ( _, ( _, WRITE1left, _)) :: rest671)) => let val  result = 
MlyValue.command (fn _ => let val  (expression as expression1) = 
expression1 ()
 in (OUTPUT(expression))
end)
 in ( LrTable.NT 8, ( result, WRITE1left, expression1right), rest671)

end
|  ( 17, ( ( _, ( _, _, ENDIF1right)) :: ( _, ( MlyValue.commandseq 
commandseq2, _, _)) :: _ :: ( _, ( MlyValue.commandseq commandseq1, _,
 _)) :: _ :: ( _, ( MlyValue.expression expression1, _, _)) :: ( _, (
 _, IF1left, _)) :: rest671)) => let val  result = MlyValue.command
 (fn _ => let val  (expression as expression1) = expression1 ()
 val  commandseq1 = commandseq1 ()
 val  commandseq2 = commandseq2 ()
 in (ITE(expression, commandseq1, commandseq2))
end)
 in ( LrTable.NT 8, ( result, IF1left, ENDIF1right), rest671)
end
|  ( 18, ( ( _, ( _, _, ENDWH1right)) :: ( _, ( MlyValue.commandseq 
commandseq1, _, _)) :: _ :: ( _, ( MlyValue.expression expression1, _,
 _)) :: ( _, ( _, WHILE1left, _)) :: rest671)) => let val  result = 
MlyValue.command (fn _ => let val  (expression as expression1) = 
expression1 ()
 val  (commandseq as commandseq1) = commandseq1 ()
 in (WH(expression, commandseq))
end)
 in ( LrTable.NT 8, ( result, WHILE1left, ENDWH1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (BIN(PLUS,expression1, expression2))
end)
 in ( LrTable.NT 9, ( result, expression1left, expression2right), 
rest671)
end
|  ( 20, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (BIN(MINUS,expression1, expression2))
end)
 in ( LrTable.NT 9, ( result, expression1left, expression2right), 
rest671)
end
|  ( 21, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (BIN(TIMES,expression1, expression2))
end)
 in ( LrTable.NT 9, ( result, expression1left, expression2right), 
rest671)
end
|  ( 22, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (BIN(DIV,expression1, expression2))
end)
 in ( LrTable.NT 9, ( result, expression1left, expression2right), 
rest671)
end
|  ( 23, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (BIN(MOD,expression1, expression2))
end)
 in ( LrTable.NT 9, ( result, expression1left, expression2right), 
rest671)
end
|  ( 24, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (BIN(AND,expression1, expression2))
end)
 in ( LrTable.NT 9, ( result, expression1left, expression2right), 
rest671)
end
|  ( 25, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (BIN(OR,expression1, expression2))
end)
 in ( LrTable.NT 9, ( result, expression1left, expression2right), 
rest671)
end
|  ( 26, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (BIN(LT,expression1, expression2))
end)
 in ( LrTable.NT 9, ( result, expression1left, expression2right), 
rest671)
end
|  ( 27, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (BIN(LEQ,expression1, expression2))
end)
 in ( LrTable.NT 9, ( result, expression1left, expression2right), 
rest671)
end
|  ( 28, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (BIN(EQ,expression1, expression2))
end)
 in ( LrTable.NT 9, ( result, expression1left, expression2right), 
rest671)
end
|  ( 29, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (BIN(GT,expression1, expression2))
end)
 in ( LrTable.NT 9, ( result, expression1left, expression2right), 
rest671)
end
|  ( 30, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (BIN(GEQ,expression1, expression2))
end)
 in ( LrTable.NT 9, ( result, expression1left, expression2right), 
rest671)
end
|  ( 31, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (BIN(NEQ,expression1, expression2))
end)
 in ( LrTable.NT 9, ( result, expression1left, expression2right), 
rest671)
end
|  ( 32, ( ( _, ( MlyValue.expression expression1, _, expression1right
)) :: ( _, ( _, TILDE1left, _)) :: rest671)) => let val  result = 
MlyValue.expression (fn _ => let val  (expression as expression1) = 
expression1 ()
 in (UN(TILDE,expression))
end)
 in ( LrTable.NT 9, ( result, TILDE1left, expression1right), rest671)

end
|  ( 33, ( ( _, ( MlyValue.expression expression1, _, expression1right
)) :: ( _, ( _, EXCLAIM1left, _)) :: rest671)) => let val  result = 
MlyValue.expression (fn _ => let val  (expression as expression1) = 
expression1 ()
 in (UN(NOT,expression))
end)
 in ( LrTable.NT 9, ( result, EXCLAIM1left, expression1right), rest671
)
end
|  ( 34, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.expression 
expression1, _, _)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let
 val  result = MlyValue.expression (fn _ => let val  (expression as 
expression1) = expression1 ()
 in (expression)
end)
 in ( LrTable.NT 9, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.NUMBER NUMBER1, NUMBER1left, NUMBER1right))
 :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  (NUMBER as NUMBER1) = NUMBER1 ()
 in (INT(NUMBER))
end)
 in ( LrTable.NT 9, ( result, NUMBER1left, NUMBER1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.expression (fn _ => let val  (ID as ID1) =
 ID1 ()
 in (Variable(ID))
end)
 in ( LrTable.NT 9, ( result, ID1left, ID1right), rest671)
end
|  ( 37, ( ( _, ( _, TT1left, TT1right)) :: rest671)) => let val  
result = MlyValue.expression (fn _ => (Boolean("tt")))
 in ( LrTable.NT 9, ( result, TT1left, TT1right), rest671)
end
|  ( 38, ( ( _, ( _, FF1left, FF1right)) :: rest671)) => let val  
result = MlyValue.expression (fn _ => (Boolean("ff")))
 in ( LrTable.NT 9, ( result, FF1left, FF1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : While_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun DOUBLECOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun RCURL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun LCURL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun TILDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun DOUBLEBAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun DOUBLEAND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun LEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun GEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun EXCLAIM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun PROGRAM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun WRITE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDWH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun TT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun FF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun NUMBER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.NUMBER (fn () => i),p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun ILLCH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
end
end
