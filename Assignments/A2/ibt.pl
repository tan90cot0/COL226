ibt(empty).
ibt(node(N, L, R)):- integer(N), ibt(L), ibt(R).

size(empty, 0).										%base case
size(node(_,L,R), N):-  							size(L,N1), 
													size(R,N2),
													N is 1+N1+N2.							%recursive size formula

height(empty, 0).									%base case
height(node(_,L,R), N):- 							height(L,N1), 
				 									height(R,N2), 
				 									N is 1 + max(N1,N2).					%recursive height formula

preorder(empty, []).								%base case
preorder(node(N,L,R), [N|List3]):- 					preorder(L, List1), 
													preorder(R, List2), 	
													append(List1, List2, List3).			%recursive preorder formula N,L,R

inorder(empty, []).									%base case
inorder(node(N,L,R), List):-  						inorder(L, List1), 
													inorder(R, List2), 
													append(List1,[N], List3),
													append(List3, List2, List).				%recursive inorder formula L,N,R

postorder(empty, []).								%base case
postorder(node(N,L,R), List):- 						postorder(L, List1), 
													postorder(R, List2), 
													append(List1, List2, List3),
													append(List3, [N], List).				%recursive postorder formula L,R,N

trPreorder(empty, []).
trPreorder(BT, List):- 								tr_prehelper(BT, List, []).				%calling a tail recursive function


tr_prehelper(empty, [], _).
tr_prehelper(node(N,L,R), [N|List], Stack):- 		push(R,Stack,Stack2),					%pushing the next nodes onto a stack
													push(L,Stack2,[Head|Tail]),
													tr_prehelper(Head, List, Tail).


push(empty, Stack, Stack).
push(BT, Stack1, Stack2):- 							append([BT], Stack1, Stack2).			%push node to a stack
							 
trInorder(empty, []).
trInorder(BT, List):- 								tr_inhelper(BT, List, []).

tr_inhelper(empty, List, [Head|Tail]):- 			tr_inhelper(Head, List, Tail).			%calling a tail recursive function
tr_inhelper(node(N,empty,empty), [N], []).
tr_inhelper(node(N,empty,empty), [N|L], [Hd|Tl]):- 	tr_inhelper(Hd, L, Tl).
tr_inhelper(node(N,L,R), List, Stack):- 			push(R,Stack,Stack2),
					 								push(node(N, empty, empty),Stack2,Stack3),	%creating a false node to match data type
					 								tr_inhelper(L, List, Stack3).

trPostorder(empty, []).
trPostorder(BT, List2):- 							tr_posthelper(BT, List, []), reverse(List, List2).	%calling a tail recursive function

tr_posthelper(empty, [], _).
tr_posthelper(node(N,L,R), [N|List], Stack):- 		push(L,Stack,Stack2),
													push(R,Stack2,[Head|Tail]),
													tr_posthelper(Head, List, Tail).

eulerTour(empty, []).								%base case
eulerTour(node(N,L,R), List):- 						eulerTour(L, List1), 
													eulerTour(R, List2), 
													append(List1, [N], List1N),
													append(List2, [N], List2N),
													append(List1N, List2N, List3),				%recursive formula for eulertour - NLNRN
													append([N], List3, List).

partition(N, [N|Tail], [], Tail).					%base case
partition(N, [Head|Tail], [Head|HL], HalfList2):- 	partition(N, Tail, HL, HalfList2).			%Divides a list about N
														
half_list([Head|Tail], HL, HL2):- 					reverse(Tail, [_|Tl]),
													reverse(Tl, List),
													partition(Head, List, HL, HL2).				%Removes the first and last elements of a list
									
ettopre([], []).									%base case
ettopre([Head|Tail], Pre):- 						half_list([Head|Tail], H1, H2),				%converts eulertour list to preorder
													ettopre(H1, Pre1),
													ettopre(H2, Pre2),
													append([Head], Pre1, Int1),
													append(Int1, Pre2, Pre).

ettoin([], []).										%base case
ettoin([Head|Tail], Pre):- 							half_list([Head|Tail], H1, H2),				%converts eulertour list to inorder
													ettoin(H1, Pre1),
													ettoin(H2, Pre2),
													append(Pre1,[Head], Int1),
													append(Int1, Pre2, Pre).

ettopost([], []).									%base case
ettopost([Head|Tail], Pre):- 						half_list([Head|Tail], H1, H2),				%converts eulertour list to postorder
													ettopost(H1, Pre1),
													ettopost(H2, Pre2),
													append(Pre1, Pre2, Int1),
													append(Int1, [Head], Pre).							

preET(empty, []).									%base case
preET(BT, Pre):- 									eulerTour(BT, ET), ettopre(ET, Pre).

inET(empty, []).									%base case
inET(BT, In):- 										eulerTour(BT, ET), ettoin(ET, In).

postET(empty, []).									%base case
postET(BT, Post):- 									eulerTour(BT, ET), ettopost(ET, Post).

toString(empty, "()").								%base case
toString(node(N,L,R), S):- 							toString(L,S1),
													toString(R,S2),
													string_concat("(", N, Str1),				%step by step printing a string
													string_concat(Str1, ", ", Str2),
													string_concat(Str2, S1, Str3),
													string_concat(Str3, ", ", Str4),
													string_concat(Str4, S2, Str5),
													string_concat(Str5, ")", S).

isBalanced(empty).									%base case								
isBalanced(node(_,L,R)):- 							isBalanced(L),								%if both subtrees and current node is balanced then tree is balanced
													isBalanced(R),
													height(L, H1),
													height(R, H2),
													abs(H1-H2)<2.

min_tree(node(N,empty,_), N).						%base case
min_tree(node(_,L,_), Min):- 						min_tree(L,Min).							%finds the minimum element of a tree

max_tree(node(N,_,empty), N).						%base case
max_tree(node(_,_,R), Max):- 						max_tree(R,Max).							%finds the maximum element of a tree

isBST(empty).										%base case
isBST(node(_, empty, empty)).						%another base case
isBST(node(N, L, empty)):-   						max_tree(L, N1),							%checking the BST property 
													isBST(L),
													N>N1.
isBST(node(N, empty, R)):-							min_tree(R, N2),
													isBST(R),
													N2>N.
isBST(node(N, L, R)):-   							max_tree(L, N1),
			      									min_tree(R, N2),							%if node and subtrees satisfy then tree satisfies
													isBST(L),
													isBST(R),
													N2>N,
					 								N>N1.

makeBST([], empty).
makeBST(L, node(Median, BST1, BST2)):- 				median(L, Median, Smaller, Greater),
													makeBST(Smaller, BST1),						%recursive call to make the 2 subtrees of the median
													makeBST(Greater, BST2).

median(List, Median, Smaller, Greater):- 			sort(List, L2),								%sorts the list
													length(L2, N),
													N>0,
													Index is (N-1)//2,							%finding the median index
													nth0(Index, L2, Median),
													append(Smaller, [Median], L),				%using backtracking to find Smaller and Greater
													append(L, Greater, L2).

lookup(_, empty):- false.							%base case
lookup(N, BST):- 									inorder(BST, List), member(N, List).		%if it is a member of the inorder traversal, it is present in the BST

insert(N, empty, node(N, empty, empty)).			%base case
insert(N, node(N1, L1, R), node(N1, L2, R)):- 		N1>N, 
													insert(N, L1, L2).							%insert in left subtree
insert(N, node(N1, L, R1), node(N1, L, R2)):- 		N>N1, 
													insert(N, R1, R2).							%insert in right subtree

insert(N, node(N, _, _), node(N, _, _)).														%in case a duplicate value is entered

delete(N, node(N, L, empty), L). 					%base case
delete(N, node(N, empty, R), R).					%base case, considers leaf node as well
delete(N, node(N, L, R), node(MaxL, L_new, R)):-    max_tree(L, MaxL),
													delete(MaxL, L, L_new).						%replace with inorder predecessor
delete(N, node(N1, L1, R), node(N1, L2, R)):- 		N1>N, 
													lookup(N,node(N1,L1,R)),
													delete(N, L1, L2).							%go to left subtree
delete(N, node(N1, L, R1), node(N1, L, R2)):- 		N>N1, 
													lookup(N,node(N1,L,R1)),
													delete(N, R1, R2).							%go to right subtree