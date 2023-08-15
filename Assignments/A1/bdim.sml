exception Div0
exception NegInd
exception Nonbool
exception Outofbounds

(*IO program*)
fun read (filename:string) =
    let val f = TextIO.getInstream(TextIO.openIn filename)
	fun loop (accum: string list, f) =
	    case (TextIO.StreamIO.inputLine f) of 
	        SOME(chunk, f') => loop (chunk::accum, f')
	      | NONE => (TextIO.StreamIO.closeIn f; accum)
    in  Vector.fromList(rev(loop ([], f)))
    end

(*searches a string st for a given character c*)
fun search (c, st) = if substring(st,0,1) = c then 0 else 1+search(c,substring(st,1,size(st)-1));

(*breaks a string and returns the number and the remaining string*)
fun break(delimiter, str, start) = let  val pos = search(delimiter, str)
                                    in (substring(str,start,pos), substring(str, pos+1, size(str)-pos-1))
                                    end
(*takes as input one line of the bdim file and returns a list of tuples of the numbers*)                                   
fun parse str = let
                    val pair1 = break(",", str, 1)
                    val pair2 = break(",", #2(pair1), 0)
                    val pair3 = break(",", #2(pair2), 0)
                    val pair4 = break(")", #2(pair3), 0)
                in 
                    (#1(pair1), #1(pair2), #1(pair3), #1(pair4))
                end
(*converts each element of the tuples to an int, from string*)
fun int_tuple(a,b,c,d) =    let val e = valOf(Int.fromString(a))
                                handle Option => 0
                                val f = valOf(Int.fromString(b))
                                handle Option => 0
                                val g = valOf(Int.fromString(c))
                                handle Option => 0
                                val h = valOf(Int.fromString(d))
                                handle Option => 0
                            in
                                (e,f,g,h)
                            end
(*Decides which line of the code vector should run*)   
fun code_line(line, path) = let val x = Vector.map int_tuple (Vector.map parse (read(path)))
                                val len = Vector.length(x)
                            in
                            if line >= len then raise Outofbounds else Vector.sub(x, line)
                            end
(*Has all the 17 opcodes and how the program should react in each*)
fun run (tup, mem, line, path) =      if #4(tup) <0 then raise NegInd 
                                    else if #1(tup) = 0 then () 
                                    else if #1(tup) = 1 then    let val inp = print("input: ")
                                                                    val v = valOf(Int.fromString(valOf(TextIO.inputLine TextIO.stdIn)))
                                                                    handle Option => 0
                                                                    val res = Array.update(mem, #4(tup), v)
                                                                in run(code_line(line+1, path), mem, line+1, path)
                                                                end
                                    else if #1(tup) = 2 then    let val a = Array.update(mem, #4(tup), Array.sub(mem, #2(tup)))
                                                                in
                                                                run(code_line(line+1, path), mem, line+1, path)
                                                                end
                                    else if #1(tup) = 3 then    let val x = if Array.sub(mem, #2(tup)) = 0 then 1 else 0
                                                                    val a = Array.update(mem, #4(tup), x)
                                                                in
                                                                run(code_line(line+1, path), mem, line+1, path)
                                                                end
                                    else if #1(tup) = 4 then    let val x = if Array.sub(mem, #2(tup)) = 0 then false else if Array.sub(mem, #2(tup)) = 1 then true else raise Nonbool 
                                                                    val y = if Array.sub(mem, #3(tup)) = 0 then false else if Array.sub(mem, #3(tup)) = 1 then true else raise Nonbool
                                                                    val z = x orelse y
                                                                    val b = if z = true then 1 else 0
                                                                    val a = Array.update(mem, #4(tup), b)
                                                                in
                                                                run(code_line(line+1, path), mem, line+1, path)
                                                                end
                                    else if #1(tup) = 5 then    let val x = if Array.sub(mem, #2(tup)) = 0 then false else if Array.sub(mem, #2(tup)) = 1 then true else raise Nonbool
                                                                    val y = if Array.sub(mem, #3(tup)) = 0 then false else if Array.sub(mem, #3(tup)) = 1 then true else raise Nonbool
                                                                    val z = x andalso y
                                                                    val b = if z = true then 1 else 0
                                                                    val a = Array.update(mem, #4(tup), b)
                                                                in
                                                                run(code_line(line+1, path), mem, line+1, path)
                                                                end
                                    else if #1(tup) = 6 then    let val a = Array.update(mem, #4(tup), Array.sub(mem, #2(tup)) + Array.sub(mem, #3(tup)))
                                                                in
                                                                run(code_line(line+1, path), mem, line+1, path)
                                                                end
                                    else if #1(tup) = 7 then    let val a = Array.update(mem, #4(tup), Array.sub(mem, #2(tup)) - Array.sub(mem, #3(tup)))
                                                                in
                                                                run(code_line(line+1, path), mem, line+1, path)
                                                                end
                                    else if #1(tup) = 8 then    let val a = Array.update(mem, #4(tup), Array.sub(mem, #2(tup)) * Array.sub(mem, #3(tup)))
                                                                in
                                                                run(code_line(line+1, path), mem, line+1, path)
                                                                end
                                    else if #1(tup) = 9 then    let val a = (if Array.sub(mem, #3(tup)) = 0 then raise Div0 else Array.update(mem, #4(tup), Array.sub(mem, #2(tup)) div Array.sub(mem, #3(tup))))
                                                                in
                                                                run(code_line(line+1, path), mem, line+1, path)
                                                                end
                                    else if #1(tup) = 10 then   let val a = Array.update(mem, #4(tup), Array.sub(mem, #2(tup)) mod Array.sub(mem, #3(tup)))
                                                                in
                                                                run(code_line(line+1, path), mem, line+1, path)
                                                                end
                                    else if #1(tup) = 11 then   let val a = Array.update(mem, #4(tup), if Array.sub(mem, #2(tup)) = Array.sub(mem, #3(tup)) then 1 else 0)
                                                                in
                                                                run(code_line(line+1, path), mem, line+1, path)
                                                                end
                                    else if #1(tup) = 12 then   let val a = Array.update(mem, #4(tup), if Array.sub(mem, #2(tup)) > Array.sub(mem, #3(tup)) then 1 else 0)
                                                                in
                                                                run(code_line(line+1, path), mem, line+1, path)
                                                                end
                                    else if #1(tup) = 13 then   if Array.sub(mem, #2(tup)) = 0 then run(code_line(line+1, path), mem, line+1, path) else run(code_line(#4(tup), path), mem, #4(tup), path)
                                    else if #1(tup) = 14 then   run(code_line(#4(tup), path), mem, #4(tup), path)
                                    else if #1(tup) = 15 then   let val p = print(Int.toString(Array.sub(mem, #2(tup)))^"\n")
                                                                in
                                                                run(code_line(line+1, path), mem, line+1, path)
                                                                end
                                    else if #1(tup) = 16 then   let 
                                                                    val res = Array.update(mem, #4(tup), #2(tup))

                                                                in run(code_line(line+1, path), mem, line+1, path)
                                                                end
                                    else print("invalid")  
(*the function which is going to be called in the terminal*)         
(*4 errors have been handled*)                           
fun interpret path = run(code_line(0, path), Array.array(100,0), 0, path) handle  Div0 => print("You are trying to divide by 0.\n") 
                                                                                | NegInd => print("You have entered a negative index.\n")
                                                                                | Nonbool=> print("You are performing boolean operations on non-boolean values.\n")
                                                                                | Outofbounds => print("Index out of bounds.\n")