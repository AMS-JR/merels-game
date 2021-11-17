:- use_module([library(lists), io ]).

%%%%%%%%%%%%%%%
% Players      %
%%%%%%%%%%%%%%%
 player1('1').
 player2('2').

%%%%%%%%%%%%%%%
% Check if Player is represented %
%%%%%%%%%%%%%%%
is_player1(Player) :- player1(Player).
is_player2(Player) :- player2(Player).

%%%%%%%%%%%%%%%
%  Check if merel is a valid player           %
%%%%%%%%%%%%%%%
is_merel(Player) :- is_player1(Player).
is_merel(Player) :- is_player2(Player).
 
%%%%%%%%%%%%%%%
% Args are player representations and different            %
%%%%%%%%%%%%%%%
other_player(Arg1, Arg2) :- Arg1 \== Arg2,
                            is_player1(Arg1),
                            is_player2(Arg2).

other_player(Arg1, Arg2) :- Arg1 \== Arg2,
                            is_player1(Arg2),
                            is_player2(Arg1).

%%%%%%%%%%%%%%%
% Check if a pair exist in the game  %
%%%%%%%%%%%%%%%
pair([Point, Merel], Point, Merel) :- is_point(Point), %->AMS
                                      is_merel(Merel). %all pair does is pair Point in Arg2 and Merel in Arg3 into Arg1

%%%%%%%%%%%%%%%
% position of the merel on the board%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%I NEED RE-EVALUATE AGAIN%%%%%%%%%%%%%%%%%%
merel_on_board([Point, Merel], []) :- false.    %Fail if Board is empty     -> This can be removed since anything not defined is already false.

merel_on_board([Point, Merel], [[Point, Merel]|_Tail]).

merel_on_board([Point, Merel], [_Head|Tail]) :-
    merel_on_board([Point, Merel], Tail).
    
%merel_on_board([Point, Merel], Board) :- ground(Board), % Board is basically the position of the merel .Maybe not
%                                         is_point(Point),
%                                         is_merel(Merel). %->AMS
                                         
%merel_on_board([Point, Merel], Board) :- ground(Board), % Board is basically the position of the merel
%                                     pair([Point, Merel], Point, Merel).               %extension->AMS.
                                         
%%%%%%%%%%%%%%%%%%%%%
%  For any variable to be a point, it must be in the list[a...x]%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_point(Point) :- member(Point,[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x]).
%%%%%%%%%%%%%%%%
% a connected row of arguments %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% horizonal rows
row(a, b, c ).
row(d, e, f ).
row(g, h, i ).
row(j, k, l ).
row(m, n, o ).
row(p, q, r ).
row(s, t, u ).
row(v, w, x ).
%vertical rows
row(a, j, v ).
row(d, k, s ).
row(g, l, p ).
row(b, e, h ).
row(q, t, w ).
row(i, m, r ).
row(f, n, u ).
row(c, o, x ).

%%%%%%%%%%%%%%%%%%%
% check if two points on the board are connected%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
connected(a,b).
connected(a,j).
connected(b,a).
connected(b,c).
connected(b,e).
connected(c,b).
connected(c,o).
connected(d,e).
connected(d,k).
connected(e,b).
connected(e,d).
connected(e,f).
connected(e,h).
connected(f,e).
connected(f,n).
connected(g,h).
connected(g,l).
connected(h,e).
connected(h,g).
connected(h,i).
connected(i,h).
connected(i,m).
connected(j,a).
connected(j,k).
connected(j,v).
connected(k,d).
connected(k,j).
connected(k,l).
connected(k,s).
connected(l,g).
connected(l,k).
connected(l,p).
connected(m,i).
connected(m,n).
connected(m,r).
connected(n,f).
connected(n,m).
connected(n,o).
connected(n,u).
connected(o,c).
connected(o,n).
connected(o,x).
connected(p,l).
connected(p,q).
connected(q,p).
connected(q,r).
connected(q,t).
connected(r,m).
connected(r,q).
connected(s,k).
connected(s,t).
connected(t,s).
connected(t,u).
connected(t,w).
connected(u,n).
connected(u,t).
connected(v,j).
connected(v,w).
connected(w,t).
connected(w,v).
connected(w,x).
connected(x,o).
connected(x,w).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FACTS ABOUT ROW AND CONNECTED SO THAT I CAN REFACTOR @17.11.2021::06:07 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% each square has 8 points.
% Every other 3 alphabbets are connected with nextto alphabet and are in a horizonal row.
% Vertical rows are a little tricky
% There are 8 horizontal rows
% connected/2 points can be deduced from the rows. Always, the middle point is connected to the other 2 points.
% Also connected/2 should be implemeted like other_player/2 predicate, meaning Args order should be irrelivant.
% ACTUALLY A MORE SIMPLE APPROACH WILL BE TO LIST DOWN ALL TH ROWS AND THEN
% DEFINE A PREDICATE THAT WILL PARSE THOSE ROWS TO DETERMINE IF TWO POINTS ARE CONNECTED.

%%%%%%%%%%%%%%%%%%%%%
% iniatial state of the board%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
initial_board([]).   % This is where i define the state of the board. Whether it empty or its state at a certain point in the game.
%initial_board([[a, '1'],[b, '1'],[d, '1'],[u, '2'],[v, '2']]).                %board for and_the_winner_is/2 predicate 1
%initial_board([[o, '1'],[w, '1'],[n, '1'],[t, '1'],[j, '1'],[x, '2'],[u, '2'],[v, '2']]).       %board for and_the_winner_is/2 predicate 2

%%%%%%%%%%%%%%%
% emty board  %
%%%%%%%%%%%%%%%
board([]).  %TO BE IMPLEMENTED   .or not . could be intial_board([]).->AMS
%board([pair([X,Y],X,Y)|Pairs]) :- board(Pairs);

%%%%%%%%%%%%%%%%%%%%%%
% deciding the winner @16.11.2021::19:49%
%%%%%%%%%%%%%%%%%%%%%%
and_the_winner_is(Board, Winner) :-
                           is_opponent_reduced_to_two_merels_by(Board, Winner),
                           report_winner( Winner ).

%need to confirm if
and_the_winner_is(Board, Winner) :-        % block opponent so that he has no legal moves
                            is_merel(Winner),
                            other_player(Winner, OtherPlayer),
                            findall(Point, merel_on_board([Point, OtherPlayer], Board), OtherPlayerOldPoints ), %find all existing Points with OtherPlayer as Merel on the board
                            is_there_no_legal_move_for_old_points(OtherPlayerOldPoints, OtherPlayer, Board), % Check that OtherPlayer has no legal moves to make
                            report_winner(Winner).
                            
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Check if Player has won the game  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_opponent_reduced_to_two_merels_by(Board, Winner) :-
                is_merel(Winner),
                findall(Point, merel_on_board([Point, Winner], Board), Points ),
                length(Points, Length), Length > 2,
                other_player(Winner, SecondPlayer),
                findall(Point, merel_on_board([Point, SecondPlayer], Board),Secondpoints),
                length(Secondpoints, LengthofPlayer2), LengthofPlayer2 < 3.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Check if OtherPlayer has won the game @16.11.2021::7:14PM  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*is_opponent_reduced_to_two_merels_by(Board, Winner) :-
               is_player2(Looser).
               other_player(Looser, Winner),
               is_opponent_reduced_to_two_merels_by(Board, Winner).*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  checking if there is a legal move for each OldPoint%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_there_no_legal_move_for_old_points([], OtherPlayer, Board). %base case after all points in Points array have been tested and popped off
%is_there_a_legal_move_for_old_points([Point|Points], OtherPlayer, Board) :-
%                            findall(NewPoint, get_legal_move(OtherPlayer, Point, NewPoint, Board), Player2LegalMoves),
%                            length(Player2LegalMoves, Length), Length = 0, % varifies that there is no legal move for the SecondPlayer at a Point
%                            is_there_a_legal_move_for_old_points(Points, OtherPlayer, Board).
%%%%%%%%%%%%%%
% I need to account for only one Point [Point]. A new definition of is_there_no_legal_move_for_old_points([Point], OtherPlayer, Board)
% I actually do not need to. SINCE [Point] is same as [Point|[]]
%%%%%%%%%%%%%%%%
is_there_no_legal_move_for_old_points([Point|Points], OtherPlayer, Board) :-    %Given the points of the OtherPlayer pieces on the board,
                            findall(NewPoint, connected(Point, NewPoint), OtherPlayerLegalMoves), %recursivily find the connected points for each point
                            is_any_connected_point_empty( OtherPlayerLegalMoves, Board),   %check if any connected point is empty
                            is_there_no_legal_move_for_old_points(Points, OtherPlayer, Board).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  check if a connected point is empty
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_any_connected_point_empty([], Board). %base case. If there is no more connected point to test, then return true since this predicate is called by a recursive predicate
is_any_connected_point_empty([Point|Points], Board) :-
                                     pair( Pair, Point, _ ),      % a choice here for both players
                                     is_not_empty_merel_at_point(Pair, Board),
                                     is_any_connected_point_empty(Points, Board).
                                     
is_not_empty_merel_at_point([Point, Merel], Board) :- merel_on_board([Point, Merel], Board).      %needs to return true or false

%MAYBE
%Board = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A game for 2 human players %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%
% Start the game %
%%%%%%%%%%%%%%%%%%
play :- welcome,
        initial_board( Board ),
        display_board( Board ),
        is_player1( Player ),
        play( 6, Player, Board ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% play/3 possible predicates %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%All merels have been placed.
play(0, Player, Board) :-
         and_the_winner_is(Board, Winner), %board represents a winning state
         display_board( Board ),  %DISPLAY_BOARD
         report_winner( Winner ).%REPORT_WINNER
         %play(TODO, TODO, TODO).
%Not all merels have been placed.
/*code for section 3.6 @16.11.2021::19:49
play(MerelsInHand, Player, Board) :-
        \+ (MerelsInHand = 0),  %Fail, if MerelsInHand = 0 so that other play/3 predicates will execute
        get_legal_place( Player, Point, Board ),  %GET_LEGAL_PLACING( +Player, -Point, +Board)
        append([[Point, Player]], Board, CurrentBoard), %ADD_THE_NEW_PAIR_IN_THE_BOARD
        report_move( Player, Point ), %REPORT_PLACING
        is_there_a_mill(Point, Player, CurrentBoard, NewBoard ),%LOOK_FOR_NEW_MILLS IF_EXISTS -> REPORT_MILL GET_REMOVE_POINT
        display_board( NewBoard ),  %DISPLAY_BOARD
        other_player(Player, OtherPlayer),  %OTHER_PLAYER
        MerelsRemainingInHand is MerelsInHand-1,   %MERELS_REMAINING_IN_HAND
        play(MerelsRemainingInHand , OtherPlayer, NewBoard).

%All merels have been placed.
play(0, Player, Board) :-
        get_legal_move( Player, OldPoint, NewPoint, Board ),%GET_LEGAL_MOVE( Player, OldPoint, NewPoint, Board )
        subtract(Board, [[OldPoint, Player]], UpdatedBoard),%MOVE_MEREL_TO_NEWPOINT -> REMOVE_MEREL_FROM_OLDPOINT
        append(UpdatedBoard, [[NewPoint, Player]], CurrentBoard),   %MOVE_MEREL_TO_NEWPOINT -> ADD_MEREL_TO_NEWPOINT
        is_there_a_mill(NewPoint, Player, CurrentBoard, NewBoard ),%LOOK_FOR_NEW_MILLS, IF_EXISTS -> REPORT_MILL, %IF_EXISTS_MILL, GET_REMOVE_POINT
        display_board( NewBoard ),   %DISPLAY_BOARD
        other_player(Player, OtherPlayer),  %OTHER_PLAYER
        play(0, OtherPlayer, NewBoard).
*/
%%%%%%%%%%%%%%%%%%%%%%
% Look for a mill, report it and remove OTher players piece
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_there_a_mill(Point, Player, Board, NewBoard) :-
       findall(NewPoint,(pair(Pair,NewPoint, Player), member(Pair,Board)), CurrentPlayersPoints), %current players Points on the Board
       length(CurrentPlayersPoints, Length), Length > 2,                %If Total Pieces of Current Player on the Board is not more than 2, there cannot be a mill
       connected_row_with_member_Point(Point, CurrentPlayersPoints, ConnectedPair), % a connected row in the current players points with Point as a member
 %     findall([_,_,_],row(_,_,_), Mill),
       report_mill( Player ),      %reporting the new mill
       display_board( Board ),  %DISPLAY_BOARD so current player can easily make a choice on what piece to remove
       get_and_remove_point(Player, Board, NewBoard).
       
is_there_a_mill(Point, Player, Board, Board).    %If first is_there_a_mill/4 predicate fails, call this to unify CurrentBoard and NewBoard in play/3.
%%%%%%%%%%%%%%%%%%%%%%
% Finding a connected row containing the New point as a member
%%%%%%%%%%%%%%%%%%%%%%%
%CAN I ACCOUNT FOR A SINGLE PLAY WHICH PRODUCES 2 MILLS? I MUST FIND A WAY OF IMPROVING THIS PREDICATE!!!!!
connected_row_with_member_Point(Point, CurrentPlayersPoints, ConnectedPair) :-
        row(X,Y,Z),
        subset([X,Y,Z], CurrentPlayersPoints),
        member(Point,[X,Y,Z]),
        ConnectedPair = [X,Y,Z].
%%%%%%%%%%%%%%%%%%%%%%
% Get and remove a piece of the Other player
%%%%%%%%%%%%%%%%%%%%%%%
get_and_remove_point(Player, Board, NewBoard) :-
        get_remove_point( Player, PointToRemove, Board),
        other_player(Player, OtherPlayer),
        subtract(Board, [[PointToRemove, OtherPlayer]], NewBoard), %remove a piece of the other player.
        report_remove( Player, Point ).                           %report remove.
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Running a game for 1 human and the computer@17.11.2021::07:09
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
play(MerelsInHand, Player, Board) :-
        \+ (Player = '2'), %PLAYER_1_IS_CURRENT? IF_NOT FAIL  --- (Can i just put '1' in Arg Player?)
        \+ (MerelsInHand = 0),  %Fail, if MerelsInHand = 0 so that other play/3 predicates will execute
        get_legal_place( Player, Point, Board ),  %GET_LEGAL_PLACING( +Player, -Point, +Board)
        report_move( Player, Point ), %REPORT_PLACING  ---(Not really neccessary. Computer doesn't care)
        append([[Point, Player]], Board, CurrentBoard), %ADD_THE_NEW_PAIR_IN_THE_BOARD
        is_there_a_mill(Point, Player, CurrentBoard, NewBoard ),%LOOK_FOR_NEW_MILLS IF_EXISTS -> REPORT_MILL GET_REMOVE_POINT
        display_board( NewBoard ),  %DISPLAY_BOARD
        other_player(Player, OtherPlayer),  %OTHER_PLAYER
        MerelsRemainingInHand is MerelsInHand-1,   %MERELS_REMAINING_IN_HAND
        play(MerelsRemainingInHand , OtherPlayer, NewBoard). %play(MERELS_REMAINING_IN_HAND, OTHER_PLAYER, NEW_BOARD)
%play/3 of placing a merel with Player 2 being the current player
play(MerelsInHand, Player, Board) :-
        \+ (Player = '2'),  %PLAYER_2_IS_CURRENT? IF_NOT FAIL  --- (Can i just put '2' in Arg Player?),
        %Fail, if MerelsInHand = 0 so that other play/3 predicates will execute,
        %GET_LEGAL_PLACING( +Player, -Point, +Board),
        %ADD_NEW_PAIR_ON_THE_BOARD or MOVE_PIECE_TO_NEW_POINT,
        %REPORT_PLACING,
        %DISPLAY_BOARD,
        %play(MERELS_REMAINING_IN_HAND, OTHER_PLAYER, NEW_BOARD).
%play/3 of moving a merel with Player 2 being the current player
play(0, Player, Board) :-
        \+ (Player = '2'),%PLAYER_1_IS_CURRENT? IF_NOT FAIL  --- (Can i just put '1' in Arg Player?)
        get_legal_move( Player, OldPoint, NewPoint, Board ),%GET_LEGAL_MOVE( Player, OldPoint, NewPoint, Board )
        subtract(Board, [[OldPoint, Player]], UpdatedBoard),%MOVE_MEREL_TO_NEWPOINT -> REMOVE_MEREL_FROM_OLDPOINT
        append(UpdatedBoard, [[NewPoint, Player]], CurrentBoard),   %MOVE_MEREL_TO_NEWPOINT -> ADD_MEREL_TO_NEWPOINT
        is_there_a_mill(NewPoint, Player, CurrentBoard, NewBoard ),%LOOK_FOR_NEW_MILLS, IF_EXISTS -> REPORT_MILL, %IF_EXISTS_MILL, GET_REMOVE_POINT
        display_board( NewBoard ),   %DISPLAY_BOARD
        other_player(Player, OtherPlayer),  %OTHER_PLAYER
        play(0, OtherPlayer, NewBoard). %play(0, OTHER_PLAYER, NEW_BOARD)
play(0, Player, Board) :-
        %PLAYER_2_IS_CURRENT? IF_NOT FAIL  --- (Can i just put '2' in Arg Player?),
        %GET_LEGAL_MOVE( Player, OldPoint, NewPoint, Board ) or GET_LEGAL_PLACING( +Player, -Point, +Board)
        %REPORT_MOVE OR REPORT_PLACING
        %ADD_NEW_PAIR_ON_THE_BOARD or MOVE_PIECE_TO_NEW_POINT
        %DISPLAY_BOARD
        %play(MERELS_REMAINING_IN_HAND, OTHER_PLAYER, NEW_BOARD)
        
play(0, Player, Board) :- TODO.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  check if there is a new mill @DD.MM.YYYY::HH:MM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check_mill/4   ->AMS --- check if the heauristics on Page 2 under Strategies which will help a computer win can be helpful here
/*check_mill() :- ...,TODO -> NOT FULLY IMPLEMENTED
              .*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dumbly choose a point. Succeeds when it can find a place to put a new merel.          %
% choose_place/3 can have different versions, one for each heuristic @DD.MM.YYYY::HH:MM %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*choose_place( _Player, Point, Board ) :- TODO??*/
/*choose_place( _Player, Point, Board ) :- TODO??*/
/*choose_place( _Player, Point, Board ) :- TODO??*/
/*choose_place( _Player, Point, Board ) :- TODO??*/
%The last choose_place/3 predicate.
choose_place( _Player, Point, Board ) :-
         connected( Point, _ ),
         empty_point( Point, Board ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dumbly choose a move. Succeeds when it can find a merel to move and a place to move it to. %         %
% choose_move/3 can have different versions, one for each heuristic @DD.MM.YYYY::HH:MM       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*choose_move( Player, OldPoint, NewPoint, Board ):- TODO??*/
/*choose_move( Player, OldPoint, NewPoint, Board ):- TODO??*/
/*choose_move( Player, OldPoint, NewPoint, Board ):- TODO??*/
/*choose_move( Player, OldPoint, NewPoint, Board ):- TODO??*/
%The last choose_move/4 predicate.
choose_move( Player, OldPoint, NewPoint, Board ) :-
       pair( Pair, OldPoint, Player ),
       merel_on_board( Pair, Board ),
       connected( OldPoint, NewPoint ),
       empty_point( NewPoint, Board ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dumbly choose a removal. Succeeds when it can find a merel to remove.                      %
% choose_remove/4 can have different versions, one for each heuristic @DD.MM.YYYY::HH:MM     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%choose_remove/4 predicate
choose_remove( Player, Point, Board ) :-
       pair( Pair, Point, Player ),
       merel_on_board( Pair, Board ).
