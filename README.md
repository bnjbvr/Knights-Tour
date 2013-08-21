# Knight's Tour

This program solves the Knight's Tour Problem, as stated by Euler. If you have a knight in a chess game,
you can make it go through the whole chessboard without going twice on the same square, if the start
square is well chosen.

This Prolog program (comments in french) solves this problem using the following heuristic : the knight
should go to the square which has the littlest number of possible next moves. It can be used on
chessboards of size 25x25 and it finds solutions in a sufficient response time.

# Usage

`swipl -s knights_tour.pl`

Then answer the questions about the size of the chessboard, then type:

`go.`
