structure Tree =
struct 
datatype     AST = AST of PROG
and          PROG = PROG of string * BLK
and          BLK = BLK of DEC list * CMD list
and          DEC = DEC of string * string list * Typ
and          Var = Var of string
and          CMD = SET of string * EXP| INPUT of string | OUTPUT of EXP | ITE of EXP * CMD list * CMD list | WH of EXP * CMD list
and          EXP =  UN of unop*EXP | BIN of binop*EXP*EXP  |INT of int | Variable of string |Boolean of string
and          binop = PLUS|MINUS|TIMES|DIV|MOD|AND|OR|LT|LEQ|EQ|GT|GEQ|NEQ
and          unop = TILDE|NOT
and 	     Typ = Int | Bool
end

signature STACK =
    sig
        type 'a Stack
        exception EmptyStack
        exception Second
        exception Error of string
        val create:'a Stack
        val push : 'a * 'a Stack -> 'a Stack
        val pop : 'a Stack -> 'a Stack
        val top : 'a Stack -> 'a
        val empty: 'a Stack -> bool
        val poptop : 'a Stack -> ('a * 'a Stack) option
        val nth : 'a Stack * int -> 'a
        val drop : 'a Stack * int -> 'a Stack
        val depth : 'a Stack -> int
        val app : ('a -> unit) -> 'a Stack -> unit
        val map : ('a -> 'b) -> 'a Stack -> 'b Stack
        val mapPartial : ('a -> 'b option) -> 'a Stack -> 'b Stack
        val find : ('a -> bool) -> 'a Stack -> 'a option
        val filter : ('a -> bool) -> 'a Stack -> 'a Stack
        val foldr : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
        val foldl : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
        val exists : ('a -> bool) -> 'a Stack -> bool
        val all : ('a -> bool) -> 'a Stack -> bool
        val list2stack : 'a list -> 'a Stack  (* Convert a list into a Stack *)
        val stack2list: 'a Stack -> 'a list (* Convert a Stack into a list *)
        val toString: ('a -> string) -> 'a Stack -> string
        val to_String: int list -> string
        val to_String2: string Stack -> string
        val to_String3: string Stack -> string
end

structure FunStack :> STACK =
    struct
    type 'a Stack = 'a list
    exception EmptyStack
    exception Second
    exception Error of string
    val create : 'a Stack = []
    fun push(x,l) = x::l
    fun pop(head::tail) = tail
        |pop(nil) = raise EmptyStack
    fun top(head::tail) = head
        |top(nil) = raise Second
    fun empty(nil) = true 
        |empty(head::tail) = false
    fun poptop(l) = List.getItem(l)
    fun nth(l,n) = List.nth(l,n)
    fun drop(l,n) = List.drop(l,n)
    fun depth(l) = List.length(l)
    fun app (f:'a -> unit) l = List.app f l
    fun map(f:'a -> 'b) l = List.map f l
    fun mapPartial(f:'a -> 'b option) l = List.mapPartial f l
    fun find(a:'a -> bool) b = List.find a b
    fun filter(a:'a -> bool) b = List.filter a b
    fun foldr(f:'a * 'b -> 'b) g h = List.foldr f g h
    fun foldl(f:'a * 'b -> 'b) g h = List.foldl f g h
    fun exists(a:'a -> bool) b = List.exists a b
    fun all(a:'a -> bool) b = List.all a b
    fun list2stack l = l
    fun stack2list l = l
    fun a2s p = Int.toString(p)
    fun toString(a2s:'a -> string) l = "ok"
    fun to_String(head::tail) =  (Int.toString(head))^", "^(to_String(tail))
        |to_String(nil) = ""
    fun to_String2(head::tail) =  (head)^" | "^(to_String2(tail))
        |to_String2(nil) = "" 
    fun to_String3(head::tail) =  (head)^" "^(to_String3(tail))
        |to_String3(nil) = "" 
end

datatype     TreeNode =Leaf of string | Node of string * TreeNode list
exception Wrong_type
exception Div0

(*val cmdseq = Node("CMDS", [Node("SET SEQ", [Leaf("A VAR"), Node("PLUS", [Leaf("5 INT") , Leaf("6 INT")])]),
Node("SET SEQ", [Leaf("B VAR"), Node("AND", [Leaf("tt BOOL") , Leaf("ff BOOL")])]),
Node("SET SEQ", [Leaf("C VAR"), Node("TIMES", [Leaf("11 INT") , Leaf("2 INT")])]),
Node("WH SEQ", [Node("NEQ BEXP", [Leaf("A VAR"), Leaf("C VAR")]), Node("CMDS", [Node("ITE SEQ", [Node("NEQ BEXP", [Leaf("A VAR"), Leaf("2 INT")]), Node("CMDS", [Node("SET SEQ", [Leaf("A VAR"), Node("PLUS", [Leaf("A VAR") , Leaf("1 INT")])])]), Node("CMDS", [Node("SET SEQ", [Leaf("A VAR"), Node("PLUS", [Leaf("A VAR") , Leaf("2 INT")])])])])])])])*)

val cmd_array = Array.array(1000,"")
val counter = 0

val _ = Control.Print.stringDepth:=1000;
val _ = Control.Print.printDepth:=1000;

fun postfix(cmd) = let
            fun post ast = case ast of Leaf ast => ast |  Node ast=> helper(ast) 
            and helper(str,head::tail) = (post head) ^" "^ helper(str, tail)
                 |helper(str, nil) = str
            val pp = post cmd
            in
                pp
            end

fun translate(str) = Array.sub (cmd_array, valOf(Int.fromString(substring(str, 6, size(str) - 6)))) handle Option => "hello"

fun printArray(m, index, maxlen) = if maxlen> index then Int.toString(Array.sub(m,index))^" | "^printArray(m, index+1, maxlen) else ""

fun addToStack(seq, stk) =  let
                                fun search st = if substring(st,size(st)-1,1) = "$" orelse substring(st,size(st)-1,1) = " " then 0 else 1+search(substring(st,0,size(st)-1))
                                and break(str) = (substring(str,size(str)-search(str),search(str)), substring(str, 0, size(str)-search(str)-1))
                                and add(seq, stk) = if(String.size(#2(break(seq)))>0) then addToStack(#2(break(seq)), FunStack.push(#1(break(seq)), stk)) else FunStack.push(#1(break(seq)), stk)
                                val c_stack = add(seq, stk)                 
                            in 
                                c_stack
                            end

fun count(stk) = if(FunStack.depth(stk)>0) then let val hd = FunStack.top(stk)
                                                    val stk = FunStack.pop(stk)
                                                in
                                                    if hd = "ITE" orelse hd = "WH" then 1 + count(stk)
                                                    else
                                                        count(stk)
                                                end
                                                else 0

fun reverse_stack(stk, stk2) = let  val a = if FunStack.depth(stk) >0 then  FunStack.top(stk) else "NO";
                                    val stk = if FunStack.depth(stk) >0 then FunStack.pop(stk) else stk;
                                    val stk2 = if FunStack.depth(stk) >0 then FunStack.push(a, stk2) else stk2
                                in if FunStack.depth(stk) >0 then reverse_stack(stk, stk2) else FunStack.push(a, stk2)
                                end

fun find_in_stack(stk, x) = if FunStack.depth(stk) >0 then (if FunStack.top(stk) = x then true else find_in_stack(FunStack.pop(stk), x)) else false

fun get_commandseq(stk, stk2) = (*first element of stk is seq*)
                            let val topmost = if FunStack.depth(stk) >0 then FunStack.top(stk) else "NO";
                                val old = FunStack.pop(stk);
                                val old = if(topmost = "BEXP" orelse topmost = "CMDS") then FunStack.push("#"^topmost, old) else old;
                                val new = FunStack.push(topmost, stk2);
                            in  
                                if(topmost = "BEXP" orelse topmost = "CMDS") then (stk2,old) else get_commandseq(old, new)
                            end
(*handle nested while and ite by running get_commandseq again when encountered with while and ite*)

(*find the innermost while and ite and replace them with command sequences*)


fun go_to(stk, stk2) = let val topmost = if FunStack.depth(stk) >0 then FunStack.top(stk) else "NO";
                                val old = FunStack.pop(stk);
                                val cond = topmost = "ITE" andalso find_in_stack(old, "ITE") = false
                                val new =  if(cond = true) then FunStack.push("#" ^ topmost, stk2) else  FunStack.push(topmost, stk2);
                            in  
                                if(cond = true) then (new, old) else go_to(old, new)
                            end

fun go_to2(stk, stk2) = let val topmost = if FunStack.depth(stk) >0 then FunStack.top(stk) else "NO";
                                val old = FunStack.pop(stk);
                                val cond = topmost = "WH" andalso find_in_stack(old, "WH") = false
                                val new =  if(cond = true) then FunStack.push("#" ^ topmost, stk2) else  FunStack.push(topmost, stk2);
                            in  
                                if(cond = true) then (new, old) else go_to2(old, new)
                            end

fun go_to3(stk, stk2) = let val topmost = if FunStack.depth(stk) >0 then FunStack.top(stk) else "NO";
                                val old = FunStack.pop(stk);
                                val cond = (topmost = "WH" orelse topmost = "ITE") andalso find_in_stack(old, "WH") = false andalso find_in_stack(old, "ITE") = false
                                val new =  if(cond = true) then FunStack.push("#" ^ topmost, stk2) else  FunStack.push(topmost, stk2);
                            in  
                                if(cond = true) then (new, old, topmost) else go_to3(old, new)
                            end

fun update(old, new, counter) = let
                        val old = if FunStack.depth(old) >0 then FunStack.pop(old) else old;
                        val new_stack2 = FunStack.create;
                        val pair_new = get_commandseq(old, FunStack.push("CMDS",new_stack2));
                        val old = #2(pair_new);;
                        val element = "$" ^ FunStack.to_String3(#1(pair_new));

                        val _ = Array.update(cmd_array, counter,element)
                        val new = FunStack.push("clause" ^Int.toString(counter), new);
                        val counter = counter + 1

                        in 
                        (old, new, counter)
                        end

fun edit_stack_while(stk, stk2, counter) = let val wh1 = go_to2(stk, stk2);                          
                                val up = update(#2(wh1), #1(wh1), counter)
                                val new = reverse_stack(#1(up), #2(up))
                            in
                                (new, #3(up))
                            end 
        
fun edit_stack_ite(stk, stk2, counter) = let val ite1 = go_to(stk, stk2);
                                val old = #2(ite1)
                                val new = #1(ite1)

                                val old = if FunStack.depth(old) >0 then FunStack.pop(old) else old;
                                val new_stack2 = FunStack.create;
                                val pair_new = get_commandseq(old, FunStack.push("CMDS",new_stack2));
                                val old = #2(pair_new);;
                                val element = "$" ^ FunStack.to_String3(#1(pair_new));

                                val _ = Array.update(cmd_array, counter,element)
                                val new = FunStack.push("clause" ^Int.toString(counter), new);
                                val counter = counter + 1
                 
                                (*else clause*)
                                
                                val old = if FunStack.depth(old) >0 then FunStack.pop(old) else old;
                                val new_stack2 = FunStack.create;
                                val pair_new = get_commandseq(old, FunStack.push("CMDS",new_stack2));
                                val old = #2(pair_new);;
                                val element = "$" ^ FunStack.to_String3(#1(pair_new));

                                val _ = Array.update(cmd_array, counter,element)
                                val new = FunStack.push("clause" ^Int.toString(counter), new);
                                val counter = counter + 1 

                                (*if clause*)
                                val new = reverse_stack(old, new)
                            in
                                (new, counter)
                            end 

fun edit_stack(stk, stk2, count, counter) =  if count = 0 then reverse_stack(stk, FunStack.create) else 
                                    (if #3(go_to3(stk, FunStack.create)) = "WH" then 
                                    let 
                                        val pair = edit_stack_while(stk, stk2, counter)
                                        val part1 = reverse_stack(#1(pair), FunStack.create)
                                        val part2 = #2(pair)
                                    in 
                                        edit_stack(part1, stk2, count-1, part2)
                                    end
                                    else 
                                    let 
                                        val pair = edit_stack_ite(stk, stk2, counter)
                                        val part1 = reverse_stack(#1(pair), FunStack.create)
                                        val part2 = #2(pair)
                                    in 
                                        edit_stack(part1, stk2, count-1, part2)
                                    end )
                                    
fun init_params(post ,x, m) =  let val v = FunStack.create;
                                val c = FunStack.create;
                                val c = addToStack(post, c)
                            in 
                                (x,v,m,c)
                            end

signature vmc = 
    sig
    val rules : (int * 'a FunStack.Stack * 'a array * 'a FunStack.Stack)  -> (int * 'a FunStack.Stack * 'a array * 'a FunStack.Stack) 
end

structure vmc = 
    struct
    val ht : (string, int) HashTable.hash_table = HashTable.mkTable(HashString.hashString, op=)(17, Domain)
    fun rules(x,v,m,c) = let 
                            val c_top = if FunStack.depth(c) >0 then FunStack.top(c) else "NO"
                            val c = if FunStack.depth(c) >0 then FunStack.pop(c) else c
                            val c_top2 = if FunStack.depth(c) >0 then FunStack.top(c) else "NO"
                            val check1 = String.compare(c_top2, "INT") = EQUAL
                            val check2 = String.compare(c_top2, "BOOL") = EQUAL
                            val check3 = String.compare(c_top2, "VAR") = EQUAL
                            val check4 = String.compare(c_top, "PLUS") = EQUAL
                            val check5 = String.compare(c_top, "MINUS") = EQUAL
                            val check6 = String.compare(c_top, "TIMES") = EQUAL
                            val check7 = String.compare(c_top, "DIV") = EQUAL
                            val check8 = String.compare(c_top, "MOD") = EQUAL
                            val check9 = String.compare(c_top, "TILDE") = EQUAL
                            val check10 = String.compare(c_top, "NOT") = EQUAL
                            val check11 = String.compare(c_top, "NEQ") = EQUAL
                            val check12 = String.compare(c_top, "EQ") = EQUAL
                            val check13 = String.compare(c_top, "LT") = EQUAL
                            val check14 = String.compare(c_top, "GT") = EQUAL
                            val check15 = String.compare(c_top, "LEQ") = EQUAL
                            val check16 = String.compare(c_top, "GEQ") = EQUAL
                            val check17 = String.compare(c_top, "AND") = EQUAL
                            val check18 = String.compare(c_top, "OR") = EQUAL
                            val check19 = String.compare(c_top, "SET") = EQUAL
                            val check20 = String.compare(c_top, "BEXP") = EQUAL orelse String.compare(c_top, "#BEXP") = EQUAL
                            val check21 = String.compare(c_top, "ITE") = EQUAL orelse String.compare(c_top, "#ITE") = EQUAL
                            val check22 = String.compare(c_top, "WH") = EQUAL
                            val check23 = String.compare(c_top, "INPUT") = EQUAL
                            val check24 = String.compare(c_top, "OUTPUT") = EQUAL
                            val ch = HashTable.inDomain ht c_top;

                            val c = if check1 = true then (*integers*)
                                        let val _ = if ch = false then HashTable.insert ht (c_top, x) else ();
                                            val e = valOf(Int.fromString(c_top))
                                            handle Option => 0;                                         
                                            val _ = if ch = false then Array.update(m, x,e) else (); (*storing value in memory*)
                                        in FunStack.pop(c)    
                                        end

                                    else if check2 = true then (*booleans*)
                                        let val _ = if ch = false then HashTable.insert ht (c_top, x) else ();
                                            val e = if String.compare(c_top, "tt") = EQUAL then 1 else 0    (*type checking perform here*)                                        
                                            val _ = if ch = false then Array.update(m, x,e) else (); (*storing value in memory*)
                                        in FunStack.pop(c)    
                                        end

                                    else if check3 = true then (*variables*)
                                        let val _ = if ch = false then HashTable.insert ht (c_top, x) else ();
                                            val _ = if ch = false then Array.update(m, x,0) else (); (*storing value in memory*)
                                        in FunStack.pop(c) 
                                        end

                                    else if check11 = true then      (*neq*)
                                        let val v_top = FunStack.nth(v,0);
                                            val v_top2 = FunStack.nth(v,1);
                                            val l1 = HashTable.lookup ht (v_top);
                                            val l2 = HashTable.lookup ht (v_top2);
                                            val a1 = Array.sub(m,l1);
                                            val a2 = Array.sub(m,l2);
                                            val a3 = if a1 = a2 then 0 else 1;
                                            val next1 = if FunStack.depth(c) >0 then FunStack.top(c) else "NO";
                                            val c= if FunStack.depth(c) >0 then FunStack.pop(c) else c;
                                            val next2 = if FunStack.depth(c) >0 then  FunStack.top(c) else "NO";
                                            val c= if FunStack.depth(c) >0 then FunStack.pop(c) else c;
                                            val next3 = if FunStack.depth(c) >0 then  FunStack.top(c) else "NO";
                                            val c= if FunStack.depth(c) >0 then FunStack.pop(c) else c;

                                            val c = if a3 = 1 andalso next3 = "#WH" then FunStack.push("NEQ", c) else c;
                                            val c = FunStack.push(next3, c);
                                            val c = FunStack.push(next2, c);
                                            val c = FunStack.push(next1, c);
                                            
                                        in c
                                        end
                                    
                                    else if check12 = true then      (*eq*)
                                        let val v_top = FunStack.nth(v,0);
                                            val v_top2 = FunStack.nth(v,1);
                                            val l1 = HashTable.lookup ht (v_top);
                                            val l2 = HashTable.lookup ht (v_top2);
                                            val a1 = Array.sub(m,l1);
                                            val a2 = Array.sub(m,l2);
                                            val a3 = if a1 = a2 then 1 else 0;
                                            val next1 = if FunStack.depth(c) >0 then FunStack.top(c) else "NO";
                                            val c= if FunStack.depth(c) >0 then FunStack.pop(c) else c;
                                            val next2 = if FunStack.depth(c) >0 then  FunStack.top(c) else "NO";
                                            val c= if FunStack.depth(c) >0 then FunStack.pop(c) else c;
                                            val next3 = if FunStack.depth(c) >0 then  FunStack.top(c) else "NO";
                                            val c= if FunStack.depth(c) >0 then FunStack.pop(c) else c;

                                            val c = if a3 = 1 andalso next3 = "#WH" then FunStack.push("NEQ", c) else c;
                                            val c = FunStack.push(next3, c);
                                            val c = FunStack.push(next2, c);
                                            val c = FunStack.push(next1, c);
                                            
                                        in c
                                        end
                                    
                                    else if check13 = true then      (*lt*)
                                        let val v_top = FunStack.nth(v,0);
                                            val v_top2 = FunStack.nth(v,1);
                                            val l1 = HashTable.lookup ht (v_top);
                                            val l2 = HashTable.lookup ht (v_top2);
                                            val a1 = Array.sub(m,l1);
                                            val a2 = Array.sub(m,l2);
                                            val a3 = if a1 > a2 then 1 else 0;
                                            val next1 = if FunStack.depth(c) >0 then FunStack.top(c) else "NO";
                                            val c= if FunStack.depth(c) >0 then FunStack.pop(c) else c;
                                            val next2 = if FunStack.depth(c) >0 then  FunStack.top(c) else "NO";
                                            val c= if FunStack.depth(c) >0 then FunStack.pop(c) else c;
                                            val next3 = if FunStack.depth(c) >0 then  FunStack.top(c) else "NO";
                                            val c= if FunStack.depth(c) >0 then FunStack.pop(c) else c;

                                            val c = if a3 = 1 andalso next3 = "#WH" then FunStack.push("NEQ", c) else c;
                                            val c = FunStack.push(next3, c);
                                            val c = FunStack.push(next2, c);
                                            val c = FunStack.push(next1, c);
                                            
                                        in c
                                        end

                                    else if check14 = true then      (*gt*)
                                        let val v_top = FunStack.nth(v,0);
                                            val v_top2 = FunStack.nth(v,1);
                                            val l1 = HashTable.lookup ht (v_top);
                                            val l2 = HashTable.lookup ht (v_top2);
                                            val a1 = Array.sub(m,l1);
                                            val a2 = Array.sub(m,l2);
                                            val a3 = if a1 < a2 then 1 else 0;
                                            val next1 = if FunStack.depth(c) >0 then FunStack.top(c) else "NO";
                                            val c= if FunStack.depth(c) >0 then FunStack.pop(c) else c;
                                            val next2 = if FunStack.depth(c) >0 then  FunStack.top(c) else "NO";
                                            val c= if FunStack.depth(c) >0 then FunStack.pop(c) else c;
                                            val next3 = if FunStack.depth(c) >0 then  FunStack.top(c) else "NO";
                                            val c= if FunStack.depth(c) >0 then FunStack.pop(c) else c;

                                            val c = if a3 = 1 andalso next3 = "#WH" then FunStack.push("NEQ", c) else c;
                                            val c = FunStack.push(next3, c);
                                            val c = FunStack.push(next2, c);
                                            val c = FunStack.push(next1, c);
                                            
                                        in c
                                        end

                                    else if check15 = true then      (*leq*)
                                        let val v_top = FunStack.nth(v,0);
                                            val v_top2 = FunStack.nth(v,1);
                                            val l1 = HashTable.lookup ht (v_top);
                                            val l2 = HashTable.lookup ht (v_top2);
                                            val a1 = Array.sub(m,l1);
                                            val a2 = Array.sub(m,l2);
                                            val a3 = if a1 >= a2 then 1 else 0;
                                            val next1 = if FunStack.depth(c) >0 then FunStack.top(c) else "NO";
                                            val c= if FunStack.depth(c) >0 then FunStack.pop(c) else c;
                                            val next2 = if FunStack.depth(c) >0 then  FunStack.top(c) else "NO";
                                            val c= if FunStack.depth(c) >0 then FunStack.pop(c) else c;
                                            val next3 = if FunStack.depth(c) >0 then  FunStack.top(c) else "NO";
                                            val c= if FunStack.depth(c) >0 then FunStack.pop(c) else c;

                                            val c = if a3 = 1 andalso next3 = "#WH" then FunStack.push("NEQ", c) else c;
                                            val c = FunStack.push(next3, c);
                                            val c = FunStack.push(next2, c);
                                            val c = FunStack.push(next1, c);
                                            
                                        in c
                                        end

                                    else if check16 = true then      (*geq*)
                                        let val v_top = FunStack.nth(v,0);
                                            val v_top2 = FunStack.nth(v,1);
                                            val l1 = HashTable.lookup ht (v_top);
                                            val l2 = HashTable.lookup ht (v_top2);
                                            val a1 = Array.sub(m,l1);
                                            val a2 = Array.sub(m,l2);
                                            val a3 = if a1 <= a2 then 1 else 0;
                                            val next1 = if FunStack.depth(c) >0 then FunStack.top(c) else "NO";
                                            val c= if FunStack.depth(c) >0 then FunStack.pop(c) else c;
                                            val next2 = if FunStack.depth(c) >0 then  FunStack.top(c) else "NO";
                                            val c= if FunStack.depth(c) >0 then FunStack.pop(c) else c;
                                            val next3 = if FunStack.depth(c) >0 then  FunStack.top(c) else "NO";
                                            val c= if FunStack.depth(c) >0 then FunStack.pop(c) else c;

                                            val c = if a3 = 1 andalso next3 = "#WH" then FunStack.push("NEQ", c) else c;
                                            val c = FunStack.push(next3, c);
                                            val c = FunStack.push(next2, c);
                                            val c = FunStack.push(next1, c);
                                            
                                        in c
                                        end
                                    
                                    else if check20 = true then (*bexp*)
                                        let val lookahead1 = c_top2;
                                            val lookahead2 = FunStack.nth(c, 1);
                                            val lookahead3 = FunStack.nth(c, 2);
                                            val condn = if FunStack.depth(v) >0 then FunStack.top(v) else "NO";
                                            val _ = if lookahead2 = "#WH" andalso condn = "tt" then rules(x,FunStack.create,m,addToStack(translate(lookahead1), FunStack.create)) else (x,v,m,c)
                                            val cmd_seq = if FunStack.depth(c) >0 then FunStack.top(c) else "NO";
                                            val c= if lookahead2 = "#WH" andalso condn = "tt" andalso FunStack.depth(c) >0 then FunStack.pop(c) else c;
                                            val wh = if FunStack.depth(c) >0 then FunStack.top(c) else "NO";
                                            val c= if lookahead2 = "#WH" andalso condn = "tt" andalso FunStack.depth(c) >0 then FunStack.pop(c) else c;
                                            val cond = if FunStack.depth(c) >0 then FunStack.top(c) else "NO";
                                            val c= if lookahead2 = "#WH" andalso condn = "tt" andalso FunStack.depth(c) >0 then FunStack.pop(c) else c;

                                            val c = if lookahead2 = "#WH" andalso condn = "tt" then FunStack.push(wh, c) else c;
                                            val c = if lookahead2 = "#WH" andalso condn = "tt" then FunStack.push(cmd_seq, c) else c;
                                            val c = if lookahead2 = "#WH" andalso condn = "tt" then FunStack.push("#BEXP", c) else c;
                                            val c = if lookahead2 = "#WH" andalso condn = "tt" then FunStack.push(cond, c) else c;

                                            val _ = if lookahead3 = "#ITE" andalso condn = "tt" then rules(x,FunStack.create,m,addToStack(translate(lookahead1), FunStack.create)) else (x,v,m,c)
                                            val _ = if lookahead3 = "#ITE" andalso condn = "ff" then rules(x,FunStack.create,m,addToStack(translate(lookahead2), FunStack.create)) else (x,v,m,c)

                                            val c= if lookahead3 = "#ITE" andalso FunStack.depth(c) >0 then FunStack.pop(c) else c;
                                            val c= if lookahead3 = "#ITE" andalso FunStack.depth(c) >0 then FunStack.pop(c) else c;
                                            val c= if lookahead3 = "#ITE" andalso FunStack.depth(c) >0 then FunStack.pop(c) else c;
                                            val c= if lookahead3 = "#ITE" andalso FunStack.depth(c) >0 then FunStack.pop(c) else c;
                                        in
                                            c
                                        end
                                    
                                    else
                                        c
                            val v = if check1 = true then 
                                        FunStack.push(c_top, v) 

                                    else if check2 = true then 
                                        FunStack.push(c_top, v)

                                    else if check3 = true then 
                                        FunStack.push(c_top, v) 

                                    else if check4 = true then      (*plus*)
                                        let val v_top = if FunStack.depth(v) >0 then FunStack.top(v) else "NO";
                                            val v = if FunStack.depth(v) >0 then FunStack.pop(v) else v;
                                            val v_top2 = if FunStack.depth(v) >0 then FunStack.top(v) else "NO";
                                            val v = if FunStack.depth(v) >0 then FunStack.pop(v) else v;
                                            val l1 = HashTable.lookup ht (v_top);
                                            val l2 = HashTable.lookup ht (v_top2);
                                            val a1 = Array.sub(m,l1);
                                            val a2 = Array.sub(m,l2);
                                            val a3 = a1+a2;
                                            val a4 = Array.update(m, x, a3);
                                            val _ = HashTable.insert ht (Int.toString(a3), x);
                                        in FunStack.push(Int.toString(a3), v) 
                                        end
                                    
                                    else if check5 = true then      (*minus*)
                                        let val v_top = if FunStack.depth(v) >0 then FunStack.top(v) else "NO";
                                            val v = if FunStack.depth(v) >0 then FunStack.pop(v) else v;
                                            val v_top2 = if FunStack.depth(v) >0 then FunStack.top(v) else "NO";
                                            val v = if FunStack.depth(v) >0 then FunStack.pop(v) else v;
                                            val l1 = HashTable.lookup ht (v_top);
                                            val l2 = HashTable.lookup ht (v_top2);
                                            val a1 = Array.sub(m,l1);
                                            val a2 = Array.sub(m,l2);
                                            val a3 = a1-a2;
                                            val a4 = Array.update(m, x, a3);
                                            val _ = HashTable.insert ht (Int.toString(a3), x);
                                        in FunStack.push(Int.toString(a3), v) 
                                        end

                                    else if check6 = true then      (*times*)
                                        let val v_top = if FunStack.depth(v) >0 then FunStack.top(v) else "NO";
                                            val v = if FunStack.depth(v) >0 then FunStack.pop(v) else v;
                                            val v_top2 = if FunStack.depth(v) >0 then FunStack.top(v) else "NO";
                                            val v = if FunStack.depth(v) >0 then FunStack.pop(v) else v;
                                            val l1 = HashTable.lookup ht (v_top);
                                            val l2 = HashTable.lookup ht (v_top2);
                                            val a1 = Array.sub(m,l1);
                                            val a2 = Array.sub(m,l2);
                                            val a3 = a1*a2;
                                            val a4 = Array.update(m, x, a3);
                                            val _ = HashTable.insert ht (Int.toString(a3), x);
                                        in FunStack.push(Int.toString(a3), v) 
                                        end

                                    else if check7 = true then      (*div*)
                                        let val v_top = if FunStack.depth(v) >0 then FunStack.top(v) else "NO";
                                            val v = if FunStack.depth(v) >0 then FunStack.pop(v) else v;
                                            val v_top2 = if FunStack.depth(v) >0 then FunStack.top(v) else "NO";
                                            val v = if FunStack.depth(v) >0 then FunStack.pop(v) else v;
                                            val l1 = HashTable.lookup ht (v_top);
                                            val l2 = HashTable.lookup ht (v_top2);
                                            val a1 = Array.sub(m,l1);
                                            val a2 = Array.sub(m,l2);
                                            val a3 = if a2 = 0 then raise Div0 else a1 div a2;
                                            val a4 = Array.update(m, x, a3);
                                            val _ = HashTable.insert ht (Int.toString(a3), x);
                                        in FunStack.push(Int.toString(a3), v) 
                                        end

                                    else if check8 = true then      (*mod*)
                                        let val v_top = if FunStack.depth(v) >0 then FunStack.top(v) else "NO";
                                            val v = if FunStack.depth(v) >0 then FunStack.pop(v) else v;
                                            val v_top2 = if FunStack.depth(v) >0 then FunStack.top(v) else "NO";
                                            val v = if FunStack.depth(v) >0 then FunStack.pop(v) else v;
                                            val l1 = HashTable.lookup ht (v_top);
                                            val l2 = HashTable.lookup ht (v_top2);
                                            val a1 = Array.sub(m,l1);
                                            val a2 = Array.sub(m,l2);
                                            val a3 = a1 mod a2;
                                            val a4 = Array.update(m, x, a3);
                                            val _ = HashTable.insert ht (Int.toString(a3), x);
                                        in FunStack.push(Int.toString(a3), v) 
                                        end

                                    else if check9 = true then      (*tilde*)
                                        let val v_top = if FunStack.depth(v) >0 then FunStack.top(v) else "NO";
                                            val v = if FunStack.depth(v) >0 then FunStack.pop(v) else v;
                                            val l1 = HashTable.lookup ht (v_top);
                                            val a1 = Array.sub(m,l1);
                                            val a3 = 0-a1;
                                            val a4 = Array.update(m, x, a3);
                                            val _ = HashTable.insert ht (Int.toString(a3), x);
                                        in FunStack.push(Int.toString(a3), v) 
                                        end

                                    else if check10 = true then      (*not*)
                                        let val v_top = FunStack.top(v);
                                            val v = FunStack.pop(v);
                                            val l1 = HashTable.lookup ht (v_top);
                                            val a1 = Array.sub(m,l1);
                                            val a3 = abs(0-a1); 
                                            val a4 = Array.update(m, x, a3);
                                            val _ = HashTable.insert ht (Int.toString(a3), x);
                                        in FunStack.push(Int.toString(a3), v) 
                                        end

                                    else if check11 = true then      (*neq*)
                                        let val v_top = FunStack.nth(v,0);
                                            val v_top2 = FunStack.nth(v,1);
                                            val l1 = HashTable.lookup ht (v_top);
                                            val l2 = HashTable.lookup ht (v_top2);
                                            val a1 = Array.sub(m,l1);
                                            val a2 = Array.sub(m,l2);
                                            val a3 = if a1 = a2 then 0 else 1;
                                            val a4 = Array.update(m, x, a3);
                                            val a5 = if a3 = 1 then "tt" else "ff";
                                            val while_check = FunStack.nth(c, 2)
                                            val v = if a3 = 1 andalso while_check = "#WH" then v else FunStack.pop(v); (*only in case of while*)
                                            val v = if a3 = 1 andalso while_check = "#WH" then v else FunStack.pop(v);
                                            val _ = HashTable.insert ht (a5, x);
                                        in FunStack.push(a5, v) 
                                        end

                                    else if check12 = true then      (*eq*)
                                        let val v_top = FunStack.nth(v,0);
                                            val v_top2 = FunStack.nth(v,1);
                                            val l1 = HashTable.lookup ht (v_top);
                                            val l2 = HashTable.lookup ht (v_top2);
                                            val a1 = Array.sub(m,l1);
                                            val a2 = Array.sub(m,l2);
                                            val a3 = if a1 = a2 then 1 else 0;
                                            val a4 = Array.update(m, x, a3);
                                            val a5 = if a3 = 1 then "tt" else "ff";
                                            val while_check = FunStack.nth(c, 2)
                                            val v = if a3 = 1 andalso while_check = "#WH" then v else FunStack.pop(v); (*only in case of while*)
                                            val v = if a3 = 1 andalso while_check = "#WH" then v else FunStack.pop(v);
                                            val _ = HashTable.insert ht (a5, x);
                                        in FunStack.push(a5, v) 
                                        end
(*change others to x*)
                                    else if check13 = true then      (*lt*)
                                        let val v_top = FunStack.nth(v,0);
                                            val v_top2 = FunStack.nth(v,1);
                                            val l1 = HashTable.lookup ht (v_top);
                                            val l2 = HashTable.lookup ht (v_top2);
                                            val a1 = Array.sub(m,l1);
                                            val a2 = Array.sub(m,l2);
                                            val a3 = if a1 > a2 then 1 else 0;
                                            val a4 = Array.update(m, x, a3);
                                            val a5 = if a3 = 1 then "tt" else "ff";
                                            val while_check = FunStack.nth(c, 2)
                                            val v = if a3 = 1 andalso while_check = "#WH" then v else FunStack.pop(v); (*only in case of while*)
                                            val v = if a3 = 1 andalso while_check = "#WH" then v else FunStack.pop(v);
                                            val _ = HashTable.insert ht (a5, x);
                                        in FunStack.push(a5, v) 
                                        end

                                    else if check14 = true then      (*gt*)
                                        let val v_top = FunStack.nth(v,0);
                                            val v_top2 = FunStack.nth(v,1);
                                            val l1 = HashTable.lookup ht (v_top);
                                            val l2 = HashTable.lookup ht (v_top2);
                                            val a1 = Array.sub(m,l1);
                                            val a2 = Array.sub(m,l2);
                                            val a3 = if a1 < a2 then 1 else 0;
                                            val a4 = Array.update(m, x, a3);
                                            val a5 = if a3 = 1 then "tt" else "ff";
                                            val while_check = FunStack.nth(c, 2)
                                            val v = if a3 = 1 andalso while_check = "#WH" then v else FunStack.pop(v); (*only in case of while*)
                                            val v = if a3 = 1 andalso while_check = "#WH" then v else FunStack.pop(v);
                                            val _ = HashTable.insert ht (a5, x);
                                        in FunStack.push(a5, v) 
                                        end

                                    else if check15 = true then      (*leq*)
                                        let val v_top = FunStack.nth(v,0);
                                            val v_top2 = FunStack.nth(v,1);
                                            val l1 = HashTable.lookup ht (v_top);
                                            val l2 = HashTable.lookup ht (v_top2);
                                            val a1 = Array.sub(m,l1);
                                            val a2 = Array.sub(m,l2);
                                            val a3 = if a1 >= a2 then 1 else 0;
                                            val a4 = Array.update(m, x, a3);
                                            val a5 = if a3 = 1 then "tt" else "ff";
                                            val while_check = FunStack.nth(c, 2)
                                            val v = if a3 = 1 andalso while_check = "#WH" then v else FunStack.pop(v); (*only in case of while*)
                                            val v = if a3 = 1 andalso while_check = "#WH" then v else FunStack.pop(v);
                                            val _ = HashTable.insert ht (a5, x);
                                        in FunStack.push(a5, v) 
                                        end

                                    else if check16 = true then      (*geq*)
                                        let val v_top = FunStack.nth(v,0);
                                            val v_top2 = FunStack.nth(v,1);
                                            val l1 = HashTable.lookup ht (v_top);
                                            val l2 = HashTable.lookup ht (v_top2);
                                            val a1 = Array.sub(m,l1);
                                            val a2 = Array.sub(m,l2);
                                            val a3 = if a1 <= a2 then 1 else 0;
                                            val a4 = Array.update(m, x, a3);
                                            val a5 = if a3 = 1 then "tt" else "ff";
                                            val while_check = FunStack.nth(c, 2)
                                            val v = if a3 = 1 andalso while_check = "#WH" then v else FunStack.pop(v); (*only in case of while*)
                                            val v = if a3 = 1 andalso while_check = "#WH" then v else FunStack.pop(v);
                                            val _ = HashTable.insert ht (a5, x);
                                        in FunStack.push(a5, v) 
                                        end

                                    else if check17 = true then      (*and*)
                                        let val v_top = FunStack.top(v);
                                            val v = FunStack.pop(v);
                                            val v_top2 = FunStack.top(v);
                                            val v = FunStack.pop(v);
                                            val l1 = HashTable.lookup ht (v_top);
                                            val l2 = HashTable.lookup ht (v_top2);
                                            val a1 = if Array.sub(m,l1) = 1 then true else if Array.sub(m,l1) = 0 then false else raise Wrong_type;
                                            val a2 = if Array.sub(m,l2) = 1 then true else if Array.sub(m,l2) = 0 then false else raise Wrong_type;
                                            val a3 = a1 andalso a2;
                                            val a4 = if a3 then 1 else 0;
                                            val a5 = Array.update(m, x, a4);
                                            val _ = HashTable.insert ht (Int.toString(a4), x);
                                        in FunStack.push(Int.toString(a4), v) 
                                        end

                                    else if check18 = true then      (*or*)
                                        let val v_top = FunStack.top(v);
                                            val v = FunStack.pop(v);
                                            val v_top2 = FunStack.top(v);
                                            val v = FunStack.pop(v);
                                            val l1 = HashTable.lookup ht (v_top);
                                            val l2 = HashTable.lookup ht (v_top2);
                                            val a1 = if Array.sub(m,l1) = 1 then true else if Array.sub(m,l1) = 0 then false else raise Wrong_type;
                                            val a2 = if Array.sub(m,l2) = 1 then true else if Array.sub(m,l2) = 0 then false else raise Wrong_type;
                                            val a3 = a1 orelse a2;
                                            val a4 = if a3 then 1 else 0;
                                            val a5 = Array.update(m, x, a4);
                                            val _ = HashTable.insert ht (Int.toString(a4), x);
                                        in FunStack.push(Int.toString(a4), v) 
                                        end                                      

                                    else if check19 = true then      (*set*)
                                        let val v_top = FunStack.top(v);
                                            val v = FunStack.pop(v);
                                            val v_top2 = FunStack.top(v);
                                            val v = FunStack.pop(v);
                                            val l1 = HashTable.lookup ht (v_top);
                                            val l2 = HashTable.lookup ht (v_top2);
                                            val a1 = Array.sub(m,l1);
                                            val a2 = Array.update(m, l2, a1)
                                        in v 
                                        end
                                    
                                    else if check20 = true then      (*bexp*)
                                        let 
                                            val v = if FunStack.depth(v) >0 then FunStack.pop(v) else v;
                                        in v 
                                        end
                                 
                               
                                    else v
                            val exit = if String.compare(c_top2, "NO") = EQUAL orelse String.compare(c_top, "NO") = EQUAL orelse String.compare(c_top, "CMDS") = EQUAL then
                                       1 
                                        else 0
                        in 
                            if exit = 1 then (x+1,v,m,c) else rules(x+1,v,m,c)
                        end
end

fun execute(cmd_seq) = let   val post = postfix(cmd_seq);
                            val stk =  reverse_stack(addToStack("$" ^ post, FunStack.create), FunStack.create)
                            val cnt = count(stk)
                            val c = edit_stack(stk, FunStack.create,cnt, 0)
                            val okie3 = FunStack.to_String2(c)
                            val letsc = vmc.rules (0,FunStack.create,Array.array(500,0),c) handle Wrong_type => (0,FunStack.create, Array.array(100,0), FunStack.create)
                                                                                                    |Div0 =>(0,FunStack.create, Array.array(100,0), FunStack.create)
                            val c = FunStack.to_String2(#4(letsc))
                            val v = FunStack.to_String2(#2(letsc))
                            val m = #3(letsc)
                            val x = #1(letsc)
                            val m = printArray(m, 0, x)
                        in
                            (v, m, c)
                        end


(*To do:
1. to string tuple
4. type error handling
5. readme*)