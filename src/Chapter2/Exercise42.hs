module Chapter2.Exercise42 (queens) where
-- | The "eight-queens puzzle" asks how to place eight queens on a chessboard
-- so that no queen ius in check from any other (i.e. no two queens are in the
-- same row, column or diagonal). One possible solution is shown below (X marks
-- the position of a queen).
--
--             ┌─────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┐
--             │     │     │     │     │     │  X  │     │     │
--             ├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
--             │     │     │  X  │     │     │     │     │     │
--             ├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
--             │  X  │     │     │     │     │     │     │     │
--             ├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
--             │     │     │     │     │     │     │  X  │     │
--             ├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
--             │     │     │     │     │  X  │     │     │     │
--             ├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
--             │     │     │     │     │     │     │     │  X  │
--             ├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
--             │     │  X  │     │     │     │     │     │     │
--             ├─────┼─────┼─────┼─────┼─────┼─────┼─────┼─────┤
--             │     │     │     │  X  │     │     │     │     │
--             └─────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┘
--
-- One way to solve the puzzle is to work across the board, placing a queen in
-- each column. Once we have placed k-1 queens, we must place the kth queen in
-- a position where it does not check any of the queens already on the board.
-- We can formulate this approach recursively: Assume that we have already
-- generated the sequence of of all possible ways to place k-1 queens in the
-- first k-1 columns of the board. For each of these ways, generate an extended
-- set of positions by placing a queen in each row of the kth column. Now filter
-- these, keeping only the positions for which the queen in the kth column is
-- safe with respect to the other queens. This produces a sequence of all ways
-- to place k queens in the first k columns. By continuing this process, we will
-- produce not only one solution, by all solutions to the puzzle.
--
-- We implement this solution as a procedure `queens`, which returns a sequence
-- of all solutions to the problem of placing n queens on a n-by-n chessboard.
-- `queens` has an internal procedure `queen-cols` that returns the sequence of
-- all ways to place queens in the first k columns of that board.
--
-- In this procedure, `restOfQueens` is a way to place k-1 queens in the first
-- k-1 columns, and `newRow` is a proposed row in which to place the queens in
-- the kth column, Complete the program by implementing the representation for
-- sets of board positions, including the procedure `adjoin`, which adjoins a
-- new (row, column) position to the set if positions. You must also write the
-- procedure `isSafe` which determines for a set of positions, whether a queen
-- in the kth column is safe with repect to the others. (Note that we need only
-- check if the new queen is safe - the other queens are aready guaranteed safe
-- with respect to each other)
queens :: Integer -> [[(Integer, Integer)]]
queens n = go n
  where
    go 0 = [[]]
    go k = let restOfQueens = restOfQueens in do
        position <- [(k, i) | i <- [1..n]]
        map (adjoin position) . filter (isSafe position) restOfQueens
    adjoin = (:)
    isSafe queen = not . any (checks queen)
    checks (x, y) (p, q) = x == p || y == q || abs (x - p) == abs (y - q)

