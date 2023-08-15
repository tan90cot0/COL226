val cmdseq = Node("CMDS", [Node("SET SEQ", [Leaf("A VAR"), Node("PLUS", [Leaf("5 INT") , Leaf("6 INT")])]),
Node("SET SEQ", [Leaf("B VAR"), Node("AND", [Leaf("tt BOOL") , Leaf("ff BOOL")])]),
Node("SET SEQ", [Leaf("C VAR"), Node("TIMES", [Leaf("11 INT") , Leaf("2 INT")])]),
Node("WH SEQ", [Node("NEQ BEXP", [Leaf("A VAR"), Leaf("C VAR")]), Node("CMDS", [Node("ITE SEQ", [Node("NEQ BEXP", [Leaf("A VAR"), Leaf("2 INT")]), Node("CMDS", [Node("SET SEQ", [Leaf("A VAR"), Node("PLUS", [Leaf("A VAR") , Leaf("1 INT")])])]), Node("CMDS", [Node("SET SEQ", [Leaf("A VAR"), Node("PLUS", [Leaf("A VAR") , Leaf("2 INT")])])])])])])])


(*
val cmdseq = Node("CMDS", [Node("SET SEQ", [Leaf("A VAR"), Node("PLUS", [Leaf("5 INT") , Leaf("6 INT")])]),
Node("SET SEQ", [Leaf("B VAR"), Node("AND", [Leaf("tt BOOL") , Leaf("ff BOOL")])]),
Node("SET SEQ", [Leaf("C VAR"), Node("TIMES", [Leaf("11 INT") , Leaf("2 INT")])]),
Node("ITE SEQ", [Node("NEQ BEXP", [Leaf("A VAR"), Leaf("C VAR")]), 
                Node("CMDS", [Node("ITE SEQ", [Node("NEQ BEXP", 
                [Leaf("A VAR"), Leaf("C VAR")]), 
                Node("CMDS", [Node("SET SEQ", [Leaf("A VAR"), Node("PLUS", [Leaf("A VAR") , Leaf("6 INT")])])]), 
                Node("CMDS", [Node("SET SEQ", [Leaf("A VAR"), Node("PLUS", [Leaf("A VAR") , Leaf("2 INT")])])])])]), 
                Node("CMDS", [Node("SET SEQ", [Leaf("A VAR"), Node("PLUS", [Leaf("A VAR") , Leaf("3 INT")])])])])])



val cmdseq = Node("CMDS", [Node("SET SEQ", [Leaf("A VAR"), Node("PLUS", [Leaf("5 INT") , Leaf("6 INT")])]),
Node("SET SEQ", [Leaf("C VAR"), Node("TIMES", [Leaf("11 INT") , Leaf("2 INT")])]),
Node("WH SEQ", [Node("NEQ BEXP", [Leaf("A VAR"), Leaf("C VAR")]),Node("CMDS", [Node("SET SEQ", [Leaf("A VAR"), Node("PLUS", [Leaf("A VAR") , Leaf("1 INT")])])]) ])])*)