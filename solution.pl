:- use_module(library(random)). 
:- use_module(library(lists)).

% Define the dynamic variables as follows
:- dynamic start/2.
:- dynamic covid/2. % - covid(X, Y) - covids' coordinates
:- dynamic home/2. % - home(X, Y) - home's coordinates
:- dynamic mask/2. % - mask(X, Y) - mask's coordinates
:- dynamic doctor/2. % - doctor(X, Y) - doctor's coordinates
:- dynamic has_mask/1.% - has_mask(HM) - HM is either 1 or 0 whether the actor has mask on or not


% Map generation

size(9, 9). % Map size, default if 9x9
variant(1). % From how far the actor can percieve a covid
start(0, 0). % Starting position, default is (0, 0)

% Insert your input predicates between the lines if you do not want the map to generate randomly.
% Do not forget to comment the generation map code in the test function
% -----------------
%covid(1, 1).
%covid(2, 7).
%home(6, 3).
%mask(5, 4).
%doctor(6, 7).
%covid(5,7).
%covid(8,6).
%doctor(3,2).
%mask(8,4).
%home(8,8).
% -----------------

% Home generation function, that can spawn a home anywhere within the borders
generate_home :-
    size(XS, YS),
    retractall(home(_,_)),
    random(0, XS, X), random(0, YS, Y), assert(home(X, Y)).

% Covids generation function, that will repeat until home is not inside the covid zone and 
% covids are not spawned in the same cell
generate_covids :-
    size(XS, YS),
    retractall(covid(_,_)),
    random(0, XS, X1), random(0, YS, Y1), assert(covid(X1, Y1)),
    random(0, XS, X2), random(0, YS, Y2), assert(covid(X2, Y2)),
    home(XH, YH),
    (X1 =:= X2, Y1 =:= Y2 -> (generate_covids); true),
    (inside_covid_zone(XH, YH) -> (generate_covids); true).

% Mask generation function, that will repeat until mask is not inside the covid zone
generate_mask :-
    size(XS, YS),
    retractall(mask(_,_)),
    random(0, XS, X), random(0, YS, Y), assert(mask(X, Y)),
   	(inside_covid_zone(X, Y) -> (generate_mask); true).

% Doctor generation function, that will repeat until doctor is not inside the covid zone
% and is not in the same cell with the mask
generate_doctor :-
    size(XS, YS),
    retractall(doctor(_,_)),
    random(0, XS, X), random(0, YS, Y), assert(doctor(X, Y)),
    mask(XM, YM),
    (X =:= XM, Y =:= YM -> (generate_doctor); true),
    (inside_covid_zone(X, Y) -> (generate_doctor); true).

% Util predicate that will return true if provided coordinates are within the borders of the field
within_borders(X, Y) :- 
    size(SX, SY),
    SX_ is SX - 1, SY_ is SY - 1,
    between(0, SX_, X), between(0, SY_, Y).

% Util predicate that will return true if provided coordinates are within covid's infection zone
inside_covid_zone(X, Y) :-
    covid(XC, YC),
    XL is XC - 1, XR is XC + 1,
    YL is YC - 1, YR is YC + 1,
    between(XL, XR, X), between(YL, YR, Y).

% Returns every possible move from a current cell
next_move(XP, YP, XN, YN, HM) :-
    (((XN is XP + 1, YN is YP));
    ((XN is XP, YN is YP + 1));
    ((XN is XP - 1, YN is YP));
    ((XN is XP, YN is YP - 1));
    ((XN is XP + 1, YN is YP + 1));
    ((XN is XP + 1, YN is YP - 1));
    ((XN is XP - 1, YN is YP + 1));
    ((XN is XP - 1, YN is YP - 1))),
    (\+ inside_covid_zone(XN, YN); HM =:= 1), % Check that coords are not inside covid zone, ignore if has mask
    within_borders(XN, YN). % Check that coords are within borders 

% Util function to print the path in a pretty way
print_path([(X, Y)]) :-
    print((X, Y)), nl.
print_path([(X, Y) | Remainder]) :-
    print((X, Y)), write(" -> "),
    print_path(Remainder).

% Util function that merges two pathes into one
merge_path([], L, L).
merge_path([H | T], L, [H | M]):-
    merge_path(T, L, M).

% Returns the shortest path from an array of all pathes
shortest_path(Pathes, Path) :-
        maplist(mapping, Pathes, LEs),
        keysort(LEs, [_-Path|_]).

% Mapping function that is used for the purposes of keysort/2 above
mapping(Elem, L-Elem) :-
        length(Elem, L).

% Backtracking search %

:- dynamic bt_best_length/1. % - bt_best_length(Length) - stores the current found best length 
bt_best_length(inf). % Initialize the length to be equal to infinity, since there are no best path yet

% Start point of the Backtracking search
bt_search(Path, Length) :-
    start(XS, YS), home(XH, YH), retractall(has_mask(_)), assert(has_mask(0)), % Set initial conditions
    setof(Path, bt_exploit_path((XS, YS), (XH, YH), [(XS, YS)], Path, 1, 0), Pathes), % Get all found pathes
    shortest_path(Pathes, Path), % Determine the shortest path from the found pathes
    bt_best_length(Length). % Retrieve the best length from the dynamic variable

% Base case of a recursive backtracking function
% Called when the current cell is the goal cell
bt_exploit_path((XS, YS), (XS, YS), _, [(XS, YS)], Length, _) :-
    retractall(has_mask(_)), assert(has_mask(0)), % Withdraw the mask, if the actor has one
    bt_best_length(BLength), % Retrieve the current best length
    (BLength > Length -> (retractall(bt_best_length(_)), assert(bt_best_length(Length))); true). % Update best length if a better one is found

% Recursive backtracking function
% (XS, YS) - current cell; (XH, YH) - goal cell, Visited - list of visited cells;
% [(XS, YS) | Remainder ] - current path, Length - current length, HM - mask status (1 if has one, 0 otherwise) 
bt_exploit_path((XS, YS), (XH, YH), Visited, [(XS, YS) | Remainder], Length, HM) :-
    bt_best_length(BLength), % Retrieve current best length
    Length < BLength, % Stop backtracking if current length is more or equals to the best length
    
   	% The following piece of code is used to update the mask status
    % If current status is 0, then check if the actor is inside the mask or doctor cell,
    % and update the status accordingly
    retractall(has_mask(_)), assert(has_mask(HM)),
    (HM =:= 0 -> (doctor(XD, YD), mask(XM, YM),
                 	(((XD =:= XS, YD =:= YS); (XM =:= XS, YM =:= YS)) -> (
                    retractall(has_mask(_)), assert(has_mask(1))); true )
                 ); true
    ),
    has_mask(HM_),
    
    next_move(XS, YS, XN, YN, HM_), % Get all the possible moves from a current cell
    \+ member((XN, YN), Visited), % Make sure that the coming (new) cell is not visited yet
    NLength is Length + 1, % Increment the length counter
    bt_exploit_path((XN, YN), (XH, YH), [(XN, YN) | Visited], Remainder, NLength, HM_). % Recursive call

% A* search %

:- dynamic predecessor/2. % - predecessor((XP, YP), (X, Y)) - where (XP, YP) are coords of predecessor of the cell (X, Y)
:- dynamic op_list/5. % - op_list(X, Y, G, H, F) - where (X, Y) are coods of a cell, and G, H are the results of g_ and h_ functions correspondingly, and F is their sum
:- dynamic cl_list/5. % - cl_list(X, Y, G, H, F) - parameters are the same as for op_list/5

% op_list stands for "opened list" and is used for keeping record of cell that were visited but not expanded yet (its successors have not been explored)
% cl_list stands for "closed list" and is used for keeping record of cell that were visited and explanded

% h is a heuristic function that estimates the cost from a current cell to the goal
% g is a function that estimates the cost from the start cell to a current cell
% f is the sum of h and g

% Function to calculate the H value for a current cell
h_function(X, Y, H) :-
    home(XH, YH), H is abs(XH - X) + abs(YH - Y).

% Function to calculate the G function for a current cell
g_function(X, Y, G) :-
    % If there were a predecessor to a current cell, then its G value is the predecessor's G value + 1
    ((predecessor((XP, YP), (X, Y)), op_list(XP, YP, _, _, _)) -> op_list(XP, YP, GP, _, _), G is GP + 1);
    ((predecessor((XP, YP), (X, Y)), cl_list(XP, YP, _, _, _)) -> cl_list(XP, YP, GP, _, _), G is GP + 1);
    % Calculate the G from the starting cell otherwise
    (start(XS, YS), G is abs(XS - Y) + abs(YS - Y)).

% Start point of several A* searches to find the best path
a_star_best(Path, Length) :-
    start(XS, YS), home(XH, YH), mask(XM, YM), doctor(XD, YD),
    astar_search(PathSH), % Run A* from start to home 
    retractall(start(_, _)), assert(start(XM, YM)),
    (astar_search([ (_, _) | PathMH ]); true), % Run A* from mash to home, and discard the first entry
    retractall(start(_, _)), assert(start(XD, YD)),
    (astar_search([ (_, _) | PathDH ]); true), % Run A* from doctor to home, and discard the first entry
    
    retractall(start(_, _)), assert(start(XS, YS)),
    retractall(home(_, _)), assert(home(XM, YM)),
    (astar_search(PathSM); true), % Run A* from start to mask
    retractall(home(_, _)), assert(home(XD, YD)),
    (astar_search(PathSD); true), % Run A* from start to doctor
    
    retractall(home(_, _)), assert(home(XH, YH)), % Set start and home to the original values
    merge_path(PathSM, PathMH, PathTM), % Merge pathes from start to mask and from mask to home
    merge_path(PathSD, PathDH, PathTD), % Merge pathes from start to doctor and from doctor to home
    shortest_path([PathSH, PathTM, PathTD], Path), % Choose the best path from the two merged and from start to home
    length(Path, Length). % Determine the length of the best path

% Start point of a single A* search
astar_search(Path) :-
    start(XS, YS), home(XH, YH),
    retractall(predecessor(_, _)),
    retractall(has_mask(_)), assert(has_mask(0)), % Define the initial conditions
	g_function(XS, YS, GS), h_function(XS, YS, HS), FS is GS + HS,% Calculate the G, H and F for the starting cell
    retractall(op_list(_, _, _, _, _)), retractall(cl_list(_, _, _, _, _)),
    assert(op_list(XS, YS, GS, HS, FS)), % Update the opened list with the starting cell, and its G, H, and F values
    astar_find(_), % Start recursive A* search
    get_path((XS, YS), (XH, YH), Path). % Get the resulting path from the sequence of predecessors

% Util function that returns a path from a sequence of predecessors
get_path((X, Y), (X, Y), [(X, Y)]).
get_path((X, Y), (XH, YH), [(X, Y) | Path]):-
    predecessor((X, Y), (XS, YS)), % Retrieve the successor of the cell (X, Y)
    get_path((XS, YS), (XH, YH), Path). % Recursive call

% Start point of the recursive util function best_next
choose_next((X, Y)) :-
  findall(op_list(X_, Y_, _, _, F_), op_list(X_, Y_, _, _, F_), [ List | Remainder ]), % Instantiate the list of the values of the op_list
  best_next(List, Remainder, (X, Y)). % 

% Base case of the util function that finds the next best cell to expand 
best_next(op_list(X_, Y_, _, _, _), [], (X, Y)):- X is X_, Y is Y_.

% Recursive definition of the util function that finds the next best cell to expand
best_next(op_list(X, Y, _, _, F), [ op_list(X_, Y_, _, _, F_) | Remainder ], (XR, YR)) :-
  	F < F_ -> 
    (best_next(op_list(X, Y, _, _, F), Remainder, (XR, YR))); % If F < F_, continue with the current cell
    (best_next(op_list(X_, Y_, _, _, F_), Remainder, (XR, YR))). % If F >= F_, continue with the new candidate cell

% Base case of the recursive A* search
astar_find(_) :-
    % If goal pair (XH, YH) is in the closed list, then we reached home
    home(XH, YH), cl_list(XH, YH, _, _, _).

% Recursive A* search function
astar_find(Path) :-
    home(XH, YH), % Retrieve the goal pair (XH, YH)
    
    % Continue if opened list is not empty, and the goal pair is not in the closed list yet
    (op_list(_, _, _, _, _), not(cl_list(XH, YH, _, _, _)) ->
    	choose_next((XN, YN)), % Find the best (in terms of F) next cell to analyze from the opened list
        op_list(XN, YN, GN, HN, FN), % Retrieve its G, H, and F values
        assert(cl_list(XN, YN, GN, HN, FN)), retractall(op_list(XN, YN, _, _, _)), % Add the cell to the closed list, and retract from the opened list
    
    	% Similar to the backtracking part, the following code is used to check if mask or doctor is reached
    	% and update the mask status accordingly
        has_mask(HM),
        (HM =:= 0 -> (doctor(XD, YD), mask(XM, YM),
                 		(((XD =:= XN, YD =:= YN); (XM =:= XN, YM =:= YN)) -> (
                 		retractall(has_mask(_)), assert(has_mask(1))); true )
                	 ); true
    	),
        
    	(explore_neighbours(XN, YN, GN, HN, FN); true), % Explore the neigbourhood of the cell
    	astar_find(Path) % Recursive call
    ).

% This function is used to exloring the neighbourhood of some cell, i.e., determining its successors
explore_neighbours(X, Y, G, H, F):-
    has_mask(HM), % Retrieve the mask status
    next_move(X, Y, XN, YN, HM), % Determine the possible next moves from a current cell
    not(cl_list(XN, YN, _, _, _)), % Check that the new cell is not closed yet
    
    % If the new cell is not opened yet, then make it "open" :)
    ((not(op_list(XN, YN, _, _, _)) ->
    	assert(predecessor((X, Y), (XN, YN))), % Set (X, Y) as a predeseccor of the new cell
    	g_function(XN, YN, G_), h_function(XN, YN, H_), F_ is G_ + H_, % Calculate the G, H, and F values for the new cell
        assert(op_list(XN, YN, G_, H_, F_)) % Add the new cell to the opened list
    );
    
    % Update the new cell if it is opened already, but the G of a previous predeseccor is greater than the current's one (X, Y)
    (op_list(XN, YN, GN, HN, _), G_ is G + 1, (G_ < GN) ->
     	F_ is G_ + HN, % Determine the new F according to the new G
  	    assert(op_list(XN, YN, G_, HN, F_)), % Add the new cell with updated G and F values to the opened list
        retract(op_list(XN, YN, GN, _, _)), % Retract the new cell with outdated G value
        retract(predecessor((_, _), (XN, YN))), % Retract the previous predecessor
        assert(predecessor((X, Y), (XN, YN))) % Set the current cell as a new predecessor
    )),
    
    explore_neighbours(X, Y, G, H, F). % Recursive call
    
% Test function
test :-
    % Uncomment the four lines below if you want the map to generate randomly
    generate_home,
    generate_covids,
    generate_mask,
   	generate_doctor,
    bagof((X, Y), home(X, Y), Homes), % Get home's position
    bagof((X, Y), covid(X, Y), Covids), % Get covids' positions
    bagof((X, Y), mask(X, Y), Masks), % Get mask's position
    bagof((X, Y), doctor(X, Y), Doctors), % Get doctor's position
    write("Homes: "), print(Homes), nl, % Print home's position
    write("Covids: "), print(Covids), nl, % Print covids' positions
    write("Masks: "), print(Masks), nl, % Print mask's position
    write("Doctors: "), print(Doctors), nl, % Print doctor's position
    nl,
    
    % Run A* search to determine the Path, Length, and Execution time
    write("*** A* result ***"), nl,
    statistics(runtime, [AST | _]),
    ((a_star_best(APath, ALength),
      statistics(runtime, [AET | _]),
      AT is abs(AET - AST),
      write("Path: "), print_path(APath),
      write("Length: "), print(ALength), nl,
      write("Execution time: "), print(AT), write(" ms"), nl
    ); write("Path not found"), nl),
    nl,
   
    % Run Backtracking search to determine the Path, Length, and Execution time
    write("*** Backtracking result ***"), nl,
    statistics(runtime, [BST | _]),
    ((bt_search(BPath, BLength),
      statistics(runtime, [BET | _]),
      BT is abs(BET - BST),
      write("Path: "), print_path(BPath),
      write("Length: "), print(BLength), nl,
      write("Execution time: "), print(BT), write(" ms"), nl
    ); write("Path not found"), nl).
    
    halt(0).
