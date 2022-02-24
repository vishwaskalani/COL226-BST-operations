ibt(empty).
ibt(node(N,L,R)):- integer(N), ibt(L), ibt(R).
/*N is an integer.
S is a string of printable characters.
L is a list of integers.
BT, LBT and RBT are all integer binary trees.
BST, BST1 and BST2 are all integer binary search trees.*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*This is a simple recursive implementation of size with the size of empty tree as 0
 otherwise the size of left subtree + size of right subtree +1 */
size(empty,0).
size(node(N,L,R),Z):-integer(N),
                    size(L,X),
                    size(R,Y),
                    Z is X+Y+1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*This is a simple recursive implementation of height with the height of empty tree as 0
 otherwise the max of  (height of left subtree ,height of right subtree) +1 */
height(empty,0).
height(node(N, L, R),Z):-integer(N),
                        height(L,X),
                        height(R,Y),
                        Z is max(X,Y)+1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*This is a simple recursive implementation of preorder with the preorder of empty tree as empty
 list otherwise it is the root node concatenated with preorder of left subtree 
 and then the right subtree*/
preorder(empty,[]).
preorder(node(N, L, R),[N|List]):-integer(N),
                                preorder(L,List1),
                                preorder(R,List2),
                                append(List1,List2,List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*This is a simple recursive implementation of inorder with the inorder of empty tree as empty
 list otherwise it is inorder of left subtree appended to  the root node and
 then the inorder of right subtree*/
inorder(empty,[]).
inorder(node(N,L,R),List):-integer(N),
                        inorder(L,List1),
                        inorder(R,List2),
                        append(List1,[N|List2],List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*This is a simple recursive implementation of postorder with the postorder of empty tree as empty
 list otherwise it is postorder of left subtree appended to the postorder of right subtree and 
 and then the root node*/
postorder(empty,[]).
postorder(node(N,L,R),List):-integer(N),
                        postorder(L,List1),
                        postorder(R,List2),
                        append(List1,List2,List3),
                        append(List3,[N],List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*The euler tree of an empty tree is empty list. In this euler tree, every leaf is added thrice 
to the euler tour list, since we are visiting on reaching it and then returning back after looking 
for left subtree and similarly returning to node after looking for right subtree. Therefore each node 
will be visited thrice in this euler tour which is recursively implemented*/
eulerTour(empty,[]).
eulerTour(node(N,L,R),List):-integer(N),
                            eulerTour(L,List1),
                            eulerTour(R,List2),
                            append([N|List1],[N|List2],List3),
                            append(List3,[N],List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*The tail recursive implementation of preorder uses list as a stack, to perform a traversal like 
DFS keeping an account of visited nodes. Corresponding to every stage of stack, there is a list which
maintains preorder. The preorder list depends upon the top of the stack. In the below implementation 
tailer is a helper predicate which begins with root node in stack and finds out the preorder list using 
backtracing by appropriately modifying the stack.*/
tailer([],[]).
tailer([empty|Tail],List):-tailer(Tail,List).
tailer([node(N,L,R)|Tail], [N|List]):-tailer([L,R|Tail],List).
trPreorder(empty,[]).
trPreorder(node(N,L,R),List1):-tailer([node(N,L,R)],List1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*The tail recursive implementation of inorder uses list as a stack, to perform a traversal like 
DFS keeping an account of visited nodes. Corresponding to every stage of stack, there is a list which
maintains inorder. The inorder list depends upon the top of the stack as well as the 2nd element 
of stack some times. In the below implementation tailer2 is a helper predicate which begins with root node in stack 
and finds out the preorder list using backtracing by appropriately modifying the stack.*/
tailer2([empty],[]).
tailer2([empty,node(N,_,R)|Tail],[N|List]):-tailer2([R|Tail],List).
tailer2([node(N,L,R)|Tail],List):-tailer2([L,node(N,L,R)|Tail],List).
trInorder(empty,[]).
trInorder(node(N,L,R),List1):-tailer2([node(N,L,R)],List1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*The tail recursive implementation of postorder uses list as a stack, to perform a traversal like 
DFS keeping an account of visited nodes. Corresponding to every stage of stack, there is a list which
maintains postorder. The postorder list depends upon the top of the stack. In the below implementation 
taile32 is a helper predicate which begins with root node in stack and finds out the postorder list using 
backtracing by appropriately modifying the stack.*/
tailer3([],[]).
tailer3([empty|Tail],List):-tailer3(Tail,List).
tailer3([node(N,L,R)|Tail],[N|List]):-tailer3([R,L|Tail],List).
trPostorder(empty,[]).
trPostorder(node(N,L,R),List1):-tailer3([node(N,L,R)],List2),reverse(List2,List1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*Alternate preET made for distict elements BT, with same euler tour as given above*/
/*
remove(_, [], []).
remove(X, [X|L1], L2):-remove(X,L1,L2).
remove(X, [E|L1], [E|L2]):-X\==E,remove(X,L1,L2).
work_on_list([],[]).
work_on_list([H|L],[H|List]):-remove(H,[H|L],L1),work_on_list(L1,List).
preET(empty,[]).
preET(node(N,L,R),List):-eulerTour(node(N,L,R),Liste),work_on_list(Liste,List).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*For finding Preorder derived from euler tree, we are labelling each node in the euler tree list,
each node is labelled uniquely,considering a full binary tree and continuous labels 0 ,1, 2, 3,..
starting from root node. The helper function work_on_list searches for all the same occurances of same
node and removes them using remove predicate. Therefore if we consider the first occurance of every node
in the euler Tour then we get the preorder. Finally the bracket remover removes the labels added to nodes*/
remove(_, [], []).
remove(X, [X|L1], L2):-remove(X,L1,L2).
remove(X, [E|L1], [E|L2]):-X\==E,remove(X,L1,L2).
work_on_list([],[]).
work_on_list([H|L],[H|List]):-remove(H,[H|L],L1),work_on_list(L1,List).
eulerTour_diff(empty,[],_,_).
eulerTour_diff(node(N,L,R),List,Count,I):-integer(N),
                            Count2 is Count+I,
                            Count3 is Count2+1,
                            J is 2*I,
                            K is 2*I+1,
                            eulerTour_diff(L,List1,Count2,J),
                            eulerTour_diff(R,List2,Count3,K),
                            append([[N,Count]|List1],[[N,Count]|List2],List3),
                            append(List3,[[N,Count]],List).
bracket_remover([],[]).
bracket_remover([[X,_]|Tail],[X|List]):-bracket_remover(Tail,List).
preET(empty,[]).
preET(node(N,L,R),List):-eulerTour_diff(node(N,L,R),Liste,0,1),work_on_list(Liste,Listf),bracket_remover(Listf,List).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*The process done here is similar to preET but just the work done is on reverse euler tour. The
first occurance of every node in reverse euler tour gives reversed post order*/

postET(empty,[]).
postET(node(N,L,R),List):-eulerTour_diff(node(N,L,R),Liste,0,1),
                        reverse(Liste,Listd),
                        work_on_list(Listd,Listc),
                        reverse(Listc,Listf),
                        bracket_remover(Listf,List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*For finding inorder using Euler tour, we are simply first removing preorder with labels derived from 
euler tour with labels, and removing post order with labels from back side of euler tour. Then what 
remains is the inorder and we can simply remove the labels to get inorder.*/
preET_diff(node(N,L,R),List):-eulerTour_diff(node(N,L,R),Liste,0,1),
                                work_on_list(Liste,List).
postET_diff(node(N,L,R),List):-eulerTour_diff(node(N,L,R),Liste,0,1),
                                reverse(Liste,Listd),
                                work_on_list(Listd,Listc),
                                reverse(Listc,List).
remover(List,[],List).
remover([H|T1],[H|T2],List):-remover(T1,T2,List).
remover([H|T1],[H2|T2],[H|List]):-H\==H2,remover(T1,[H2|T2],List).
inET(empty,[]).
inET(node(N,L,R),List):-eulerTour_diff(node(N,L,R),Lista,0,1),
                        preET_diff(node(N,L,R),Listb),
                        remover(Lista,Listb,Listc),
                        reverse(Listc,Listd),
                        postET_diff(node(N,L,R),Liste),
                        reverse(Liste,Listf),
                        remover(Listd,Listf,Listg),
                        reverse(Listg,Listh),
                        bracket_remover(Listh,List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*The below implementation of toString specifies the () conversion of empty tree as a rule
and relates a general tree with simple recursion to the right and left subtree. "number_string"
helps in converting the root node to string and "string_concat" helps in concatanating the toString 
version of left and right subtrees as well as brackets and comma at right places.*/

toString(empty,'()').
toString(node(N,L,R),String):-integer(N),
                                    number_string(N,S),
                                    toString(L,String1),
                                    toString(R,String2),
                                    string_concat('(',S,Stringa),
                                    string_concat(', ',String1,Stringb),
                                    string_concat(', ',String2,Stringc),
                                    string_concat(Stringa,Stringb,Stringd),
                                    string_concat(Stringd,Stringc,Stringe),
                                    string_concat(Stringe,')',String).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*The isBalanced predicate specifies empty tree clause as a rule and relates a general tree with
the balancing of right and left subtrees and ensures the absolute value of difference in heights 
of subtrees at root node is not greater than 1. abs(x) helps in evaluating absoulte value.*/

isBalanced(empty).
isBalanced(node(N,L,R)):-integer(N),
                        height(L,H1),
                        height(R,H2),
                        T is H1-H2,
                        abs(T,A),
                        2>A,
                        isBalanced(L),
                        isBalanced(R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*A binary tree is a bst if it as unique values and its inorder is thus an increasing sequence.
the helper predicate isincreasing is true if list in it is increasing otherwise it is false.
Further inorder of tree is evaluated and checked whether it is increasing or not.*/

isincreasing([]).
isincreasing([X]):-integer(X).
isincreasing([X,Y|Tail]):-integer(X),integer(Y),X<Y,isincreasing([Y|Tail]).
isBST(empty).
isBST(node(N,L,R)):-inorder(node(N,L,R),List),isincreasing(List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*makeBST predicate uses sub_list predicate which is for finding out first X elements of a list.
That is sub_list(X,List,List1) will be true if List1 is first X elements of List. Further indexer(X,List,Y)
is true if Y is at Xth index of the list. makeBST works after sorting the list (which is done using inbuilt sort
of prolog), and then dividing the list in two halves, first of n//2 elements and other of n-n//2-1 elements
and then recursively applying the same definition of makeBST.*/

sub_list(0,_,[]).
sub_list(X,[E|Tail],[E|List]):-Y is X-1,sub_list(Y,Tail,List).
indexer(0,[H|_],H).
indexer(X,[_|T],E):-X>0,Y is X-1,indexer(Y,T,E).
makeBST([],empty).
makeBST(Lists,node(N,L,R)):-sort(Lists,List),
                            length(List,X),
                            Y is X//2,
                            indexer(Y,List,N),
                            sub_list(Y,List,List1),
                            reverse(List,Lista),
                            Z is X-Y,
                            W is Z-1,
                            sub_list(W,Lista,List2),
                            makeBST(List1,L),makeBST(List2,R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*The first two lines of lookup predicate specifies base case that lookup in empty tree
is always false and looking up in a tree with value at root node is true. 
The next two lines ensure that if value is being searched at correct location by recursively 
sending it to the right subtree by comparison with root node.*/

lookup(_,empty):-fail.
lookup(N,node(N,_,_)).
lookup(X,node(N,L,_)):-X<N,lookup(X,L).
lookup(X,node(N,_,R)):-X>N,lookup(X,R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*The first line of insert predicate specifies base case of insertion in empty trees. 
The next two lines ensure that if value to be inserted is at right location by recursively 
sending it to the correct leaf by comparison with root node. Then insertion below a leaf
is ensured by base case*/

insert(X,empty,node(X,empty,empty)).
insert(X,node(N,L,R),node(N,Y,R)):-X<N,insert(X,L,Y).
insert(X,node(N,L,R),node(N,L,Y)):-X>N,insert(X,R,Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*The delete algo is based on the concept that if it is a leaf then remove it directly otherwise
place the inorder predecessor or successor appropriately at its place and delete it from its location.
So first we are recursively reaching that node and then if it has no right subtree then we are replacing
it with inorder predecessor otherwise replacing with inorder successor.*/
rightmost(node(N,_,empty),N).
rightmost(node(_,_,R),Y):-rightmost(R,Y).
leftmost(node(N,empty,_),N).
leftmost(node(_,L,_),Y):-leftmost(L,Y).
inorder_pred(node(_,L,_),Y):-rightmost(L,Y).
inorder_succ(node(_,_,R),Y):-leftmost(R,Y).
delete(N,node(N,empty,empty),empty).
delete(N,node(N,L,R),node(X,L,T1)):-inorder_succ(node(N,L,R),X),delete(X,R,T1).
delete(N,node(N,L,empty),node(X,T1,empty)):-inorder_pred(node(N,L,empty),X),delete(X,L,T1).
delete(Y,node(N,L,R),node(N,L,Rd)):-Y>N,delete(Y,R,Rd).
delete(Y,node(N,L,R),node(N,Rd,R)):-Y<N,delete(Y,L,Rd).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





















                        

