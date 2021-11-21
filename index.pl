:- use_module([library(lists), io ]).

/*
 This code can be found on the Github repository https://github.com/AMS-JR/merels-game which is now private.
 Please note that this is my code and the repository is also mine.
*/
%%%%%%%%%%%%%%%%
%    Players   %
%%%%%%%%%%%%%%%%
 player1('1').
 player2('2').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Check if Player is represented %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_player1(Player) :- player1(Player).
is_player2(Player) :- player2(Player).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Check if merel is a valid player      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_merel(Player) :- is_player1(Player).
is_merel(Player) :- is_player2(Player).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Finding the other Player when One player is Known  %
% Args are player representations and different      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
other_player(FirstPlayer, SecondPlayer) :-
                            is_player1(FirstPlayer),
                            is_player2(SecondPlayer).

other_player(FirstPlayer, SecondPlayer) :-
                            is_player1(SecondPlayer),
                            is_player2(FirstPlayer).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Check if a pair is valid and can exist in the game  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%all pair does is pair Point in Arg2 and Merel in Arg3 into Arg1
/*pair([Point, Merel], Point, Merel) :- is_point(Point), %check if point is a valid point.
                                      is_merel(Merel).*/
pair([Point, Merel], Point, Merel) :-
              is_merel(Merel).
%MIGHT BE EASIER JUST USING pair([Point, Merel], Point, Merel). REVIEW LATER->AMS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% position of the merel on the board       %
% Argument 2 is assumed to be instantiated %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
merel_on_board([_Point, _Merel], []) :- false.    %Fail if Board is empty. This can be removed since anything not defined is already false. ->AMS

merel_on_board([Point, Merel ], [[Point, Merel ]|_Tail]):-
             is_merel(Merel).
merel_on_board([Point, Merel ], [[Point, _ ]|_Tail]):-
             is_merel(Merel).
merel_on_board([Point, Merel], [_Head|Tail]) :-
    merel_on_board([Point, Merel], Tail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check if two points on the board are connected %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
connected(Point1, Point2) :-
    row(X, Y, Z),
    %member(Point1, [X, Y, Z]),  %removing these
    %member(Point2, [X, Y, Z]),  % makes no difference to the result
    nextto(Point1, Point2, [X, Y, Z]).
connected(Point1, Point2) :-
    row(X, Y, Z),
    %member(Point1, [X, Y, Z]),   %removing these
    %member(Point2, [X, Y, Z]),   % makes no difference to the result
    nextto(Point2, Point1, [X, Y, Z]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                    iniatial state of the board                                                        %
% I will assumme it is empty at all times( a new game between players), and not empty only for testing other predicates %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%initial_board(_Board).
initial_board([]).               %board for the play/3 predicate between two players.
%initial_board([[a, '1'],[b, '1'],[d, '1'],[u, '2'],[v, '2']]).   %board for and_the_winner_is/2 predicate 1
%initial_board([[o, '1'],[w, '1'],[n, '1'],[t, '1'],[j, '1'],[x, '2'],[u, '2'],[v, '2']]).   %board for and_the_winner_is/2 predicate 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                     emty board                           %
% am using initial_board/1 here since it makes life easier %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%initial_board([]).  %an empty board is an initial_board/1 with and empty [] as Argument

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% deciding the winner @16.11.2021::19:49%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Opponent is reduced to two merels by winner
and_the_winner_is(Board, Winner) :-
                           is_opponent_reduced_to_two_merels_by(Board, Winner),
                           report_winner( Winner ).

%Opponent merels have been blocked and has no legal moves to make
and_the_winner_is(Board, Winner) :-
                            is_merel(Winner),
                            other_player(Winner, OtherPlayer),
                            findall(Point, member([Point, OtherPlayer], Board), OtherPlayerOldPoints ), %find all existing Points with OtherPlayer as Merel on the board
                            is_there_no_legal_move_for_old_points(OtherPlayerOldPoints, OtherPlayer, Board), % Check that OtherPlayer has no legal moves to make
                            report_winner(Winner).
                            
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Check if a Player has won the game  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_opponent_reduced_to_two_merels_by(Board, Winner) :-
                is_merel(Winner),
                findall(Point, member([Point, Winner], Board), Points ),
                length(Points, Length), Length > 2,
                other_player(Winner, SecondPlayer),
                findall(Point, member([Point, SecondPlayer], Board),Secondpoints),
                length(Secondpoints, LengthofPlayer2), LengthofPlayer2 < 3.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% checking if there is no legal move for each OldPoint i.e., if all legal moves have merels on the board. If they all have merels, %
% then Current player has no moves to make and has lost the game but if this fails, then current player has not lost the game yet. %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_there_no_legal_move_for_old_points([], _OtherPlayer, _Board). %base case after all points in Points array have been tested and popped off
is_there_no_legal_move_for_old_points([Point|Points], OtherPlayer, Board) :-    %Given the points of the OtherPlayer pieces on the board,
                            findall(NewPoint, connected(Point, NewPoint), OtherPlayerLegalMoves), %recursivily find the connected points for each point
                            has_merel( OtherPlayerLegalMoves, Board),   % and check if connected points has merels
                            is_there_no_legal_move_for_old_points(Points, OtherPlayer, Board).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  check if a connected point already has a merel %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
has_merel([], _Board). %base case. If there is no more connected point to test, then return true since this predicate is called by a recursive predicate
has_merel([Point|Points], Board) :-
                   member([Point, _ ], Board),          %EXCEPTION HERE IS THAT I CAN USE merel_on_board/2 here since merel Param. is anonymous
                   has_merel(Points, Board).

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
        play( 10, Player, Board ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% play/3 possible predicates %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%All merels have been placed.
play(0, _Player, Board) :-
         and_the_winner_is(Board, _Winner). %board represents a winning state
         %display_board( Board ),  %DISPLAY_BOARD
         %report_winner( Winner ).%REPORT_WINNER
%Not all merels have been placed.
/*code for section 3.6 @16.11.2021::19:49  */
/*play(MerelsInHand, Player, Board) :-
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
        move_merel(Player, OldPoint, NewPoint, Board, CurrentBoard),  %MOVE_MEREL_TO_NEWPOINT
        is_there_a_mill(NewPoint, Player, CurrentBoard, NewBoard ),%LOOK_FOR_NEW_MILLS, IF_EXISTS -> REPORT_MILL, %IF_EXISTS_MILL, GET_REMOVE_POINT
        display_board( NewBoard ),   %DISPLAY_BOARD
        other_player(Player, OtherPlayer),  %OTHER_PLAYER
        play(0, OtherPlayer, NewBoard).
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Look for a mill, report it and remove Other players piece %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_there_a_mill(Point, Player, Board, NewBoard) :-
       points_on_board(Player, Board, CurrentPlayersPoints), %current players Points on the Board
       length(CurrentPlayersPoints, Length), Length > 2,                %If Total Pieces of Current Player on the Board is not more than 2, there cannot be a mill
       a_mill_with_member_Point(Point, CurrentPlayersPoints, _ConnectedPair), % a connected row in the current players points with Point as a member
       report_mill( Player ),      %reporting the new mill
       display_board( Board ),  %DISPLAY_BOARD so current player can easily make a choice on what piece to remove
       get_and_remove_point(Player, Board, NewBoard).
       
is_there_a_mill(_Point, _Player, Board, Board).    %If first is_there_a_mill/4 predicate fails, call this to unify CurrentBoard and NewBoard in play/3.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Find all the Current Players' points on the Board %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
points_on_board(Player, Board, CurrentPlayersPoints) :-
      findall(NewPoint,(pair(Pair,NewPoint, Player), member(Pair,Board)), CurrentPlayersPoints). %current players Points on the Board
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Finding a mill with Point as a member %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%CAN I ACCOUNT FOR A SINGLE PLAY WHICH PRODUCES 2 MILLS?? I MUST FIND A WAY OF IMPROVING THIS PREDICATE!!!!!  MAYBE NOT
a_mill_with_member_Point(Point, CurrentPlayersPoints, ConnectedPair) :-
        row(X,Y,Z),
        subset([X,Y,Z], CurrentPlayersPoints),
        member(Point,[X,Y,Z]),
        ConnectedPair = [X,Y,Z].
%VERSION of a_mill_with_member_Point/3 TO BE USE?? OR AVOID FINDALL ALTOGETHER
%findall(MillPoint, (row(X,Y,Z),subset([X,Y,Z], CurrentPlayersPoints), member(Point,[X,Y,Z]),member(MillPoint,[X,Y,Z])), MillPoints).
%%%%%%%%%%%%%%%%%%%%%%
% Get and remove a piece of the Other player
%%%%%%%%%%%%%%%%%%%%%%%
get_and_remove_point(Player, Board, NewBoard) :-
        get_remove_point( Player, PointToRemove, Board),
        remove_if_not_in_mill(Player, PointToRemove, Board, NewBoard), %->AMS
        report_remove( Player, PointToRemove ).                           %report remove.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Remove the merel on PointToRemove chosen by current player for the merel points of    %
% the other player if PointToRemove is not in a mill. We cannot remove points in a mill %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_if_not_in_mill(Player, PointToRemove, Board, NewBoard) :-
          other_player(Player, OtherPlayer),
          points_on_board(OtherPlayer, Board, OtherPlayersPoints), %find the points of OtherPlayer on the board
          \+ (member([PointToRemove, Player], Board)), % A player should not remove their own points. Is this really important?? or address this differently later
          \+ (a_mill_with_member_Point(PointToRemove, OtherPlayersPoints, _ConnectedPair)), % the point to remove should not be in a mill
          subtract(Board, [[PointToRemove, OtherPlayer]], NewBoard). %remove a piece of the other player.
remove_if_not_in_mill(Player, _PointToRemove, Board, NewBoard) :-
          format( '\nThat piece cannot be removed. Please remove another piece.\n', [] ),%REPORT THAT YOU CAN NOT REMOVE PIECE
          get_and_remove_point(Player, Board, NewBoard).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Move a merel from OldPoint to NewPoint. %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
move_merel(Player, OldPoint, NewPoint, Board, CurrentBoard) :-
        subtract(Board, [[OldPoint, Player]], UpdatedBoard),%MOVE_MEREL_TO_NEWPOINT -> REMOVE_MEREL_FROM_OLDPOINT
        append(UpdatedBoard, [[NewPoint, Player]], CurrentBoard).   %MOVE_MEREL_TO_NEWPOINT -> ADD_MEREL_TO_NEWPOINT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Running a game for 1 human and the computer@17.11.2021::07:09  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%play/3 of placing a merel with Player 1 being the current player
play(MerelsInHand, Player, Board) :-
        \+ (Player = '2'), %PLAYER_1_IS_CURRENT? IF_NOT FAIL,
        \+ (MerelsInHand = 0),  %Fail, if MerelsInHand = 0 so that other play/3 predicates will execute
        get_legal_place( Player, Point, Board ),  %GET_LEGAL_PLACING( +Player, -Point, +Board)
        append([[Point, Player]], Board, CurrentBoard), %ADD_THE_NEW_PAIR_IN_THE_BOARD
        report_move( Player, Point ), %REPORT_PLACING  ---(Not really neccessary. Computer doesn't care)
        is_there_a_mill(Point, Player, CurrentBoard, NewBoard ),%LOOK_FOR_NEW_MILLS IF_EXISTS -> REPORT_MILL GET_REMOVE_POINT
        display_board( NewBoard ),  %DISPLAY_BOARD
        MerelsRemainingInHand is MerelsInHand-1,   %MERELS_REMAINING_IN_HAND
        play(MerelsRemainingInHand , '2', NewBoard). %play(MERELS_REMAINING_IN_HAND, OTHER_PLAYER, NEW_BOARD)
%play/3 of placing a merel with Player 2 being the current player
play(MerelsInHand, Player, Board) :-
        \+ (Player = '1'),    %PLAYER_2_IS_CURRENT? IF_NOT FAIL,
        \+ (MerelsInHand = 0),  %Fail, if MerelsInHand = 0 so that other play/3 predicates will execute,
        choose_place( Player, Point, Board ), %GET_LEGAL_PLACING( +Player, -Point, +Board), _Player = Player, only that we say it appears only once by adding _
        append([[Point, Player]], Board, CurrentBoard),  %ADD_NEW_PAIR_ON_THE_BOARD,
        report_move( Player, Point ),    %REPORT_PLACING,
        check_mill(Point, Player, CurrentBoard, NewBoard),  %LOOK_FOR_NEW_MILLS IF_EXISTS -> REPORT_MILL GET_REMOVE_POINT
        display_board( NewBoard ),  %DISPLAY_BOARD,
        MerelsRemainingInHand is MerelsInHand-1,   %MERELS_REMAINING_IN_HAND
        play(MerelsRemainingInHand , '1', NewBoard).%play(MERELS_REMAINING_IN_HAND, OTHER_PLAYER, NEW_BOARD).
%play/3 of moving a merel with Player 1 being the current player
play(0, Player, Board) :-
        \+ (Player = '2'),%PLAYER_1_IS_CURRENT? IF_NOT FAIL
        get_legal_move( Player, OldPoint, NewPoint, Board ),%GET_LEGAL_MOVE( Player, OldPoint, NewPoint, Board )
        move_merel(Player, OldPoint, NewPoint, Board, CurrentBoard),   %MOVE_MEREL_TO_NEWPOINT
        is_there_a_mill(NewPoint, Player, CurrentBoard, NewBoard ),%LOOK_FOR_NEW_MILLS, IF_EXISTS -> REPORT_MILL, %IF_EXISTS_MILL, GET_REMOVE_POINT
        display_board( NewBoard ),   %DISPLAY_BOARD
        play(0, '2', NewBoard). %play(0, OTHER_PLAYER, NEW_BOARD)
%play/3 of moving a merel with Player 2 being the current player
play(0, Player, Board) :-
        \+ (Player = '1'),   %PLAYER_2_IS_CURRENT? IF_NOT FAIL,
        choose_move( Player, OldPoint, NewPoint, Board ), %GET_LEGAL_MOVE( Player, OldPoint, NewPoint, Board )
        move_merel(Player, OldPoint, NewPoint, Board, CurrentBoard),  %MOVE_MEREL_TO_NEWPOINT
        report_move( Player, OldPoint, NewPoint ),  %REPORT_MOVE OR REPORT_PLACING
        check_mill(NewPoint, Player, CurrentBoard, NewBoard),  %LOOK_FOR_NEW_MILLS IF_EXISTS -> REPORT_MILL GET_AND_REMOVE_POINT
        display_board( NewBoard ),  %DISPLAY_BOARD
        play(0, '1', NewBoard). %play(MERELS_REMAINING_IN_HAND, OTHER_PLAYER, NEW_BOARD)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check_mill/4 is for Player 2 or the computer
% check if there is a new mill @DD.MM.YYYY::HH:MM
% if opponent is able to make a mill, remove one of the relevant pieces;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check_mill/4
check_mill(Point, Player, Board, NewBoard) :-
       points_on_board(Player, Board, CurrentPlayersPoints), %current players Points on the Board
       length(CurrentPlayersPoints, Length), Length > 2,                %If Total Pieces of Current Player on the Board is not more than 2, there cannot be a mill
       a_mill_with_member_Point(Point, CurrentPlayersPoints, _ConnectedPair), % a connected row in the current players points with Point as a member
       report_mill( Player ),      %reporting the new mill
       display_board( Board ),  %DISPLAY_BOARD so current player can easily make a choice on what piece to remove
       other_player(Player, OtherPlayer),
       choose_remove(OtherPlayer, PointToRemove, Board), %Choose a point of the OtherPlayer to remove on the Board
       subtract(Board, [[PointToRemove, '1']], NewBoard), %remove a piece of the other player.
       report_remove( Player, PointToRemove ). %report the point removed
       
check_mill(_Point, _Player, Board, Board).    %If first check_mill/4 predicate fails, call this to unify CurrentBoard and NewBoard in play/3.
%%%%%%%%%%%%%%%%%%%%
%   HEURISTICS     %
%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Succeeds when it can find a place to put a new merel to form a MILL or defend againt a mill by opponent %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* Choose a place where a mill will be formed if a merel is place there.... Playing Offensive */
choose_place( Player, Point, Board ) :-
        points_on_board(Player, Board, PlayersPoints), %find the points of Current Player on the board,
        member(PointX, PlayersPoints),    %Randomly pick a point, we have a choice pointer here,
        member(PointY, PlayersPoints),    %Randomly pick another point, we have a choice pointer here,
        \+ (PointX = PointY),             %The points have to be different,
        relevant_point(PointX, PointY, Point, Board). % That relevant point to place a merel in order to make a mill
/* Choose a place which is empty and together with two other connecting places in row, the opponent can form a mill .... Playing defensive.*/
choose_place( Player, Point, Board ) :-
        other_player(Player, OtherPlayer),
        points_on_board(OtherPlayer, Board, OtherPlayersPoints), %find the points of OtherPlayer on the board
        member(PointX, OtherPlayersPoints),    %Randomly pick a point, we have a choice pointer here,
        member(PointY, OtherPlayersPoints),    %Randomly pick another point, we have a choice pointer here,
        \+ (PointX = PointY),             %The points have to be different,
        relevant_point(PointX, PointY, Point, Board).     %Point to place Piece.

/*CASE: ...Place pieces on points with many connections*/
/*choose_place( _Player, Point, Board ) :- TODO??*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dumbly choose a point. Succeeds when it can find a place to put a new merel.          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
choose_place( _Player, Point, Board ) :-
         connected( Point, _ ),
         empty_point( Point, Board ).
/*CASE: when two points are connected and one of those points is connected to a third empty Point such that
 together all these three points can make a mill because they are a connected row.*/
 relevant_point(PointX, PointY, Point, Board) :-
        connected(PointX, PointY),        %the two points have to be connected,
        member(PointZ, [PointX, PointY]), %Picking one point at a time, we have a choice pointer here
        connected(PointZ, Point),         %Find a connected point for our chosen Point, we have a choice pointer here
        empty_point(Point, Board),        %If our chosen Point is empty, then we can choose this place.
        can_make_mill([PointX,PointY,Point]).  %the points are in no particular order
/*CASE: when two points are not connected, but have a middle empty Point connecting them and
 together all these three points can make a mill because they are a connected row.*/
 relevant_point(PointX, PointY, Point, Board)  :-
        connected(PointX, Point),     %PointX is connected to some Point,
        connected(Point, PointY),     %PointY is connected to that same Point as well,
        empty_point(Point, Board),        %If our chosen Point is empty,
        can_make_mill([PointX,PointY,Point]).  %the points are in no particular order

%Check if any given three points can make a mill, in no particular order
can_make_mill([Point1,Point2,Point3]) :-               %the points are in no particular order
         row(X,Y,Z),
         member(X,[Point1,Point2,Point3]),
         member(Y,[Point1,Point2,Point3]),
         member(Z,[Point1,Point2,Point3]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Succeeds when it can find a place to put a new merel to form a MILL or defend againt a mill by opponent %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* Choose a place where a mill will be formed if a merel is moved there.... Playing Offensive */
choose_move( Player, OldPoint, NewPoint, Board ):-
          points_on_board(Player, Board, PlayersPoints), %find the points of Current Player on the board,
          member(OldPoint, PlayersPoints),
          connected(OldPoint, NewPoint),
          empty_point(NewPoint, Board),
          member(PointX,PlayersPoints), \+ (PointX = OldPoint),
          member(PointY,PlayersPoints), \+ (PointY = OldPoint), \+(PointY = PointX),
          can_make_mill([NewPoint,PointX,PointY]).
/* Choose a place which is empty and together with two other connecting places in row, the opponent can form a mill .... Playing defensive.*/
choose_move( Player, OldPoint, NewPoint, Board ):-
          other_player(Player, OtherPlayer),
          points_on_board(OtherPlayer, Board, OtherPlayersPoints), %find the points of OtherPlayer on the board
          member(PointX, OtherPlayersPoints),    %Randomly pick a point, we have a choice pointer here,
          member(PointY, OtherPlayersPoints),    %Randomly pick another point, we have a choice pointer here,
          \+ (PointX = PointY),             %The points have to be different,
          relevant_point(PointX, PointY, NewPoint, Board),     %Point to move a Piece.
          points_on_board(Player, Board, PlayersPoints), %find the points of Current Player on the board,
          member(OldPoint, PlayersPoints),
          connected(OldPoint, NewPoint).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dumbly choose a move. Succeeds when it can find a merel to move and a place to move it to. %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%The last choose_move/4 predicate.
choose_move( Player, OldPoint, NewPoint, Board ) :-
       pair( Pair, OldPoint, Player ),
       %merel_on_board( Pair, Board ),  /*Because of the way empty_point/2 calls merel_on_board/2, i won't be using it and only empty_point/2 calls it*/
       member( Pair, Board ),
       connected( OldPoint, NewPoint ),
       empty_point( NewPoint, Board ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5%%%%%%%%%%%%%%%%%%%%%%%%%
%       remove a relevant piece if opponent is about to make a mill            %
% if there is no such relevant piece, then default to the dump choose_remove/3 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* Remove that point of the opponent which, together with two other points, can make a mill*/
choose_remove( Player, Point, Board ) :-  % note, Player here is the other player. Calling predicate passes OtherPlayer to Player
        points_on_board(Player, Board, OtherPlayersPoints), %find the points of OtherPlayer on the board
        member(PointX, OtherPlayersPoints),  %Randomly pick point, we have a choice pointer here,
        member(PointY, OtherPlayersPoints),  %Randomly pick another point, we have a choice pointer here,
        \+ (PointX = PointY),     %The points have to be different,
        relevant_point_to_remove(PointX, PointY, Point, Board).  %point to remove
/*dumbly choose a removal. Succeeds when it can find a merel to remove.*/
choose_remove( Player, Point, Board ) :-
       pair( Pair, Point, Player ),
       %merel_on_board( Pair, Board ).
       member( Pair, Board ).
       
/*CASE: when two points are connected and one of those points is connected to a third empty point such that
together all these three points can make a mill because they are a connected row. We choose to remove the
point directly connected to the empty point*/
relevant_point_to_remove(PointX, PointY, Point, Board):-
        connected(PointX, PointY),
        member(Point, [PointX, PointY]),  % Picking one point at a time, we have a choice pointer here
        connected(Point, PointZ),          % Find a connected point for our chosen Point, we have a choice pointer here
        empty_point( PointZ, Board ),      % if that connected point is empty
        can_make_mill([PointX,PointY,PointZ]).
/*CASE: when two points are not connected, but have a middle empty point connecting them and
together all these three points can make a mill because they are a connected row.We can choose to
remove any of the other points i.e., first or last point in the row */
relevant_point_to_remove(PointX, PointY, Point, Board):-
        connected(PointX, SomePoint),     %PointX is connected to some Point,
        connected(SomePoint, PointY),     %PointY is connected to that same Point as well,
        empty_point( SomePoint, Board ),      % if that connected point is empty
        can_make_mill([PointX,PointY,SomePoint]),
        member(Point, [PointX, PointY]).  % We can pick any point, we have a choice pointer here

%END!
%Author: Amadou Sarjo Jallow.
