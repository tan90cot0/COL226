signature STACK =
    sig
        type 'a Stack
        exception EmptyStack
        exception Error of string
        val create: unit -> 'a Stack
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
end

structure FunStack :> STACK =
    struct
    type 'a Stack = 'a list
    exception EmptyStack
    exception Error of string
    fun create() = []
    fun push(x,l) = x::l
    fun pop(head::tail) = tail
        |pop(nil) = raise EmptyStack
    fun top(head::tail) = head
        |top(nil) = raise EmptyStack
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
    fun toString(a2s:'a -> string) l = "hihi"
end
