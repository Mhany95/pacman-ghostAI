
domains
	Path = symbol*
	State = state(Path,integer) %State path, FullCost
	SList = State*

%--------------------------------------------------working---------------------------------------
predicates

	nondeterm goal_cost(Path, integer)
	nondeterm coord(symbol,integer,integer).
	nondeterm h_Cost(symbol,integer).
	nondeterm pacman_Pos(symbol).
	nondeterm neighbour(symbol,symbol,integer).
	nondeterm full_cost(Path,integer).
	nondeterm findpath(symbol,symbol, Path) %(i,i)
	find_MinState(State,State,State,symbol) %(i,i,o,i)
	nondeterm filter_list(SList,SList,SList,symbol) %(i,i,o,i)
	nondeterm filter_list_aux(SList,State,SList,symbol) %(i,i,o,i)
	nondeterm reverse(Path,Path)
	nondeterm reverse_aux(Path,Path,Path)
	nondeterm aStar_Search(SList, SList, symbol, Path) %(i,i,o) (Paths,Goal_Node,return Final_Path)
	expand(State, SList) %(i,i,i,o)
	nondeterm getChildren(State, State) %(i,i,i,o)
	append(SList, SList, SList) %(i,i,o)
	sort(SList, SList) %(i,o) (queue, return sorted queue)
	nondeterm swap(SList, SList)
clauses
	%a relationships
        neighbour(a1,a2,1).
        neighbour(a1,b1,1).
        neighbour(a2,a1,1).
        neighbour(a2,a3,1).
        neighbour(a3,a2,1).
        neighbour(a3,a4,1).
        neighbour(a4,a3,1).
        neighbour(a4,b4,1).
        neighbour(a4,a5,1).
        neighbour(a5,a4,1).
        neighbour(a5,b5,1).
        neighbour(a5,a6,1).
        neighbour(a6,a5,1).
        neighbour(a6,b6,1).
        neighbour(a8,a9,1).
        neighbour(a9,a8,1).
        neighbour(a9,a10,1).
        neighbour(a9,b9,1).
        neighbour(a10,a9,1).
        neighbour(a10,a11,1).
        neighbour(a10,b10,1).
        neighbour(a11,a10,1).
        neighbour(a13,a14,1).
        neighbour(a13,b13,1).
        neighbour(a14,a13,1).
        neighbour(a14,a15,1).
        neighbour(a14,b14,1).
        neighbour(a15,a14,1).
        neighbour(a15,a16,1).
        neighbour(a15,b15,1).
        neighbour(a16,a15,1).
        neighbour(a16,a17,1).
        neighbour(a17,a16,1).
        neighbour(a17,a18,1).
        neighbour(a18,a17,1).
        neighbour(a18,b18,1).
        
        %b relationships
        neighbour(b1,a1,1).
        neighbour(b1,c1,1).
        neighbour(b4,a4,1).
        neighbour(b4,c4,1).
        neighbour(b4,b5,1).
        neighbour(b5,b4,1).
        neighbour(b5,a5,1).
        neighbour(b5,b6,1).
        neighbour(b6,a6,1).
        neighbour(b6,b5,1).
        neighbour(b6,c6,1).
        neighbour(b9,a9,1).
        neighbour(b9,c9,1).
        neighbour(b9,b10,1).
        neighbour(b10,a10,1).
        neighbour(b10,b9,1).
        neighbour(b10,c10,1).
        neighbour(b13,a13,1).
        neighbour(b13,b14,1).
        neighbour(b13,c13,1).
        neighbour(b14,a14,1).
        neighbour(b14,b13,1).
        neighbour(b14,b15,1).
        neighbour(b15,a15,1).
        neighbour(b15,b14,1).
        neighbour(b15,c15,1).
        neighbour(b18,a18,1).
        neighbour(b18,c18,1).
        
        %c relationships
        neighbour(c1,b1,1).
        neighbour(c1,d1,1).
        neighbour(c3,c4,1).
        neighbour(c4,c3,1).
        neighbour(c4,b4,1).
        neighbour(c4,d4,1).
        neighbour(c6,b6,1).
        neighbour(c6,c7,1).
        neighbour(c6,d6,1).
        neighbour(c7,c6,1).
        neighbour(c7,c8,1).
        neighbour(c8,c7,1).
        neighbour(c8,c9,1).
        neighbour(c9,b9,1).
        neighbour(c9,c8,1).
        neighbour(c9,c10,1).
        neighbour(c10,c9,1).
        neighbour(c10,b10,1).
        neighbour(c10,c11,1).
        neighbour(c11,c10,1).
        neighbour(c11,c12,1).
        neighbour(c12,c11,1).
        neighbour(c12,c13,1).
        neighbour(c13,c12,1).
        neighbour(c13,b13,1).
        neighbour(c13,d13,1).
        neighbour(c15,b15,1).
        neighbour(c15,c16,1).
	neighbour(c15,d15,1).
	neighbour(c16,c15,1).
	neighbour(c18,b18,1).
	neighbour(c18,d18,1).
	
	%d relationships
	neighbour(d1,c1,1).
	neighbour(d1,e1,1).
	neighbour(d4,c4,1).
	neighbour(d4,e4,1).
	neighbour(d6,c6,1).
	neighbour(d6,e6,1).
	neighbour(d13,c13,1).
	neighbour(d13,e13,1).
	neighbour(d15,c15,1).
	neighbour(d15,e15,1).
	neighbour(d18,c18,1).
	neighbour(d18,e18,1).
	
	%e relationships
	neighbour(e1,d1,1).
	neighbour(e1,e2,1).
	neighbour(e2,e1,1).
	neighbour(e2,e3,1).
	neighbour(e3,e2,1).
	neighbour(e3,e4,1).
	neighbour(e4,e3,1).
	neighbour(e4,e5,1).
	neighbour(e4,d4,1).
	neighbour(e5,e4,1).
	neighbour(e5,e6,1).
	neighbour(e6,e5,1).
	neighbour(e6,e7,1).
	neighbour(e6,d6,1).
	neighbour(e7,e6,1).
	neighbour(e7,e8,1).
	neighbour(e8,e7,1).
	neighbour(e8,e9,1).
	neighbour(e9,e8,1).
	neighbour(e9,e10,1).
	neighbour(e10,e9,1).
	neighbour(e10,e11,1).
	neighbour(e11,e10,1).
	neighbour(e11,e12,1).
	neighbour(e12,e11,1).
	neighbour(e12,e13,1).
	neighbour(e13,e12,1).
	neighbour(e13,e14,1).
	neighbour(e13,d13,1).
	neighbour(e14,e13,1).
	neighbour(e14,e15,1).
	neighbour(e15,e14,1).
	neighbour(e15,e16,1).
	neighbour(e15,d15,1).
	neighbour(e16,e15,1).
	neighbour(e16,e17,1).
	neighbour(e17,e16,1).
	neighbour(e17,e18,1).
	neighbour(e18,e17,1).
	neighbour(e18,d18,1).
	
	
	
	%row a coordinates
	coord(a1,0,0).
	coord(a2,0,1).
	coord(a3,0,2).
	coord(a4,0,3).
	coord(a5,0,4).
	coord(a6,0,5).
	coord(a8,0,7).
	coord(a9,0,8).
	coord(a10,0,9).
	coord(a11,0,10).
	coord(a13,0,12).
	coord(a14,0,13).
	coord(a15,0,14).
	coord(a16,0,15).
	coord(a17,0,16).
	coord(a18,0,17).
	
	%row b coordinates
	coord(b1,1,0).
	coord(b4,1,3).
	coord(b5,1,4).
	coord(b6,1,5).
	coord(b9,1,8).
	coord(b10,1,9).
	coord(b13,1,12).
	coord(b14,1,13).
	coord(b15,1,14).
	coord(b18,1,17).
	
	%row c coordinates
	coord(c1,2,0).
	coord(c3,2,2).
	coord(c4,2,3).
	coord(c6,2,5).
	coord(c7,2,6).
	coord(c8,2,7).
	coord(c9,2,8).
	coord(c10,2,9).
	coord(c11,2,10).
	coord(c12,2,11).
	coord(c13,2,12).
	coord(c15,2,14).
	coord(c16,2,15).
	coord(c18,2,17).
	
	%row d coordinates
	coord(d1,3,0).
	coord(d4,3,3).
	coord(d6,3,5).
	coord(d13,3,12).
	coord(d15,3,14).
	coord(d18,3,17).
	
	%row e coordinates
	coord(e1,4,0).
	coord(e2,4,1).
	coord(e3,4,2).
	coord(e4,4,3).
	coord(e5,4,4).
	coord(e6,4,5).
	coord(e7,4,6).
	coord(e8,4,7).
	coord(e9,4,8).
	coord(e10,4,9).
	coord(e11,4,10).
	coord(e12,4,11).
	coord(e13,4,12).
	coord(e14,4,13).
	coord(e15,4,14).
	coord(e16,4,15).
	coord(e17,4,16).
	coord(e18,4,17).
	
	
	%calculating h cost (euclidian)
	h_Cost(Point1,Distance):-pacman_Pos(Point2),coord(Point1,X1,Y1),coord(Point2,X2,Y2),NX=X2-X1,NY=Y2-Y1,NX2=NX*NX,NY2=NY*NY,Res=NX2+NY2,Distance=sqrt(Res).
	pacman_Pos(b1).
	
	%Stop condition when there are two nodes only in the path
	goal_cost([N1,N2], Cost):-
		neighbour(N2, N1 ,Cost).
	%Calculate the goal cost of the path		
	goal_cost([N1,N2|T], Result):-
		neighbour(N2, N1, Cost1),
		goal_cost([N2|T] , Cost2),
		Result = Cost1 + Cost2.	
		
	%Calculate the full cost of the path
	full_cost([N|T], Full_Cost):-
		L=[N|T],
			%write("-> Calculating fullcost of ",L,"\n"),		
		h_Cost(N, Heuristic_Cost), %This is the secret of Why we save path in reverse order? inseated of navigating to the end of list
			%write("Heuristic cost of (",N,") is (",Heuristic_Cost,")\n"),
		goal_cost([N|T], Goal_Cost),
			%write("Goal cost of ",L," is (",Goal_Cost,")\n\n"),
		Full_Cost = Heuristic_Cost + Goal_Cost.	
		
		findpath(StartNode, GoalNode, ShortestPath):-
		h_Cost(StartNode,SFullCost),
		aStar_Search([state([StartNode],SFullCost)], [], GoalNode, RShortestPath), %(i,i,o)
		reverse(RShortestPath,ShortestPath).
			%write("&&&&&&&&& The result &&&&&&&&&"),
			%write("\n--> Shortest Path is ", ShortestPath),
			%write("\n&&&&&&&&&&&&&&&&&&&&&&&&&&\n").
	
	%Stop condition of Best-First Search Algo.
	%aStar_Search([], _, _, []).
			%write("\nSearch --> Stop Condition (Goal not found)\n\n\n").
			
	%Stop condition of Best-First Search Algo.
	aStar_Search([OpenState|_], ClosedList, GoalNode, OpenPath):-
		OpenState = state(OpenPath,FullCost),
		OpenPath = [GoalNode|_],!.
			%write("\n\nSearch --> Stop Condition (Goal found)\n"),
			%write("with Full Cost= ",FullCost),nl,
			%write("ClosedList: ",ClosedList),nl.
			
	%Recursive rule of Best-First Search Algo.
	aStar_Search([OpenState|OpenStates], ClosedList, GoalNode, FinalPath):-
			%write("------------------------------------------------------------\n"),
			%write("Search --> RR(1) The State with smallest fullcost ",OpenState,"\n"),
			%write("\nExpanding (",OpenState,")\n"),
		expand(OpenState, ChildrenStates),
			%write("Search --> RR(2) After expanding (",OpenState,"),its children are ",ChildrenStates,"\n"),	

		filter_list(ChildrenStates, ClosedList, FilteredChidrenList,c),		
			%write("Search --> RR(2.1) After Filtering Children (",ChildrenStates,"), results ",FilteredChidrenList,"\n"),	
		filter_list(OpenStates, FilteredChidrenList, FilteredOpenList,o),
			%write("Search --> RR(2.2) After Filtering OpenList (",OpenStates,"),results ",FilteredOpenList,"\n"),

		sort(FilteredOpenList, SortedOpenList),
			%write("Search --> RR(3) OpenList after ascending sorting according to fullcost ",SortedOpenList,"\n"),
		append(ClosedList, [OpenState], NewClosedList),
			%write("Search --> RR(4) New ClosedList",NewClosedList,"\n\n\n"),
				
		aStar_Search(SortedOpenList, NewClosedList, GoalNode, FinalPath).

	%1)
	expand(OpenState, ChildrenStates):-
		findall(ChildState, getChildren(OpenState, ChildState), ChildrenStates).
	
	%A helper predicate for expanding
	getChildren(OpenState, ChildState):-
		OpenState = state([OpenNode|Path],_),
		neighbour(OpenNode, ChildNode, _),
		
		%write("Child: ",ChildNode),nl,
		
		Child_Path = [ChildNode,OpenNode|Path],
		full_cost(Child_Path,Child_FullCost),
		
		ChildState = state(Child_Path,Child_FullCost).


	find_MinState(State1,State2,MinState,_):-
		State1 = state(_,FCost1),
		State2 = state(_,FCost2),
		FCost1 < FCost2,!,
		MinState = State1.
	find_MinState(_,State2,State2,o):-!.
		
	filter_list_aux([],State,[State],o):-!.
	filter_list_aux([],_,[],c):-!.
	filter_list_aux([HState|T],State,[MinState|T],Type):-
		HState = state([X|_],_),
		State = state([X|_],_),
		find_MinState(HState,State,MinState,Type),!.
	filter_list_aux([HState|T],State,T,c):-   % This condition when there is closedNode X, which opened via another path with cost greater than its closed cost (i.e. Neglect X)
		HState = state([X|_],_),
		State = state([X|_],_),!.
	filter_list_aux([HState|T1],State,[HState|T2],Type):-
		filter_list_aux(T1,State,T2,Type).
	
	filter_list([],L,L,o):-!.
	filter_list([],_,[],c):-!.
	filter_list(L,[],L,_):-!.
	filter_list(List,[State|T],FilteredList,Type):-
		filter_list(List,T,FList1,Type),
		filter_list_aux(FList1,State,FilteredList,Type).		
	
	%2)	
	append([],L,L):-!.
	append([H|T1], L, [H|T2]):-
		append(T1,L,T2).		
	
	%3)
	%for sorting paths according to fullcost
	sort(Queue, SortedQueue):-
		swap(Queue, Queue2),!,
		sort(Queue2, SortedQueue).
	sort(Sorted, Sorted).
		
	%A helper predicate for sorting states
	swap([X,Y|T], [Y,X|T]):-
		X = state(_,CX),
		Y = state(_,CY),
		CX > CY.
		
	swap([X|T], [X|T2]):-
		swap(T, T2).
			reverse(L,RL):-
		reverse_aux(L,RL,[]).
	reverse_aux([],RL,RL):-!.
	reverse_aux([H|T],RL,Acc):-
		reverse_aux(T,RL,[H|Acc]).
goal
findpath(e10,c4, ShortestPath).