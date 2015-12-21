# Solving the GCHQ puzzle "by hand"

The [GCHQ 2015 Christmas puzzle](http://www.gchq.gov.uk/press_and_media/news_and_features/Pages/Directors-Christmas-puzzle-2015.aspx) is a [Nonogram](https://en.wikipedia.org/wiki/Nonogram) puzzle, which involves filling in squares on a grid to reveal a picture, satisfying some constraints. For a computer, a nice way to solve this problem is using a [SAT solver](https://matthewearl.github.io/2015/12/10/gchq-xmas-card/). But humans aren't great at SAT solving, and I was given a print-out of this puzzle while on holiday, with no computer. I'd never encountered such a puzzle before, so working with a friend (and some wine) we came up with an approach, and set about applying it. Alas, applying an algorithmic approach over a large grid is not easy for a human, and we eventually ended up with contradictions. On returning from holiday, I automated our approach, and tested it. Our approach worked, and the code is below.

### The Problem

The puzzle is:

![](http://www.gchq.gov.uk/SiteCollectionImages/grid-shading-puzzle.jpg)

It comprises a 25x25 grid, some filled in squares, and alongside each row/column are the number of consecutive squares that must be filled in each line. For example, the 8th row down must have two runs of three filled squares, with a gap in between, and potentially gaps before or after.

### Our Approach

Our approach was to take each line and compute the number of "free" gaps - how many spaces could be inserted with choice. For one row (4 from the bottom) the entire grid is constrained, with no free gaps. Starting with the most constrained lines, we tried to figure out where the pieces could go, based on the existing squares. We quickly realised that _negative_ information was important, so tagged each square with "don't know" (left blank), must be filled (we shaded it in) or must be unfilled (we drew a circle in it). For each line in isolation, looking at the constraints, we inferred squares to be filled or unfilled by examining the possible locations of each run.

### The Code

Our constraint system works over a grid where each square is in one of three states. Using [Haskell](https://haskell.org), we can encode that as `[[Maybe Bool]]`. The `[[.]]` is a list of lists, where the outer list is a list of rows, and the inner list is a list of squares. Each of the inner lists must be the same length, and for the GCHQ puzzle, they must all be 25 elements long. For the squares we use `Maybe Bool`, with `Nothing` for unknown and `Just` for known, using `True` as filled and `False` as unfilled.

Given the `[[Maybe Bool]]` grid and the constraints, our approach was to pick a single line, and try to layout the runs, identifying squares that must be `True`/`False`. To replicate that process on a computer, I wrote a function `tile` that given the constraints and the existing line, produces all possible lines that fit. That code reads as:

    tile :: [Int] -> [Maybe Bool] -> [[Bool]]
    tile [] xs = maybeToList $ xs ~> replicate (length xs) False
    tile (c:cs) xs = concat [map (\r -> a ++ b ++ c ++ r) $ tile cs xs
        | gap <- [0 .. length xs - (c + sum cs + length cs)]
        , (false,xs) <- [splitAt gap xs]
        , (true,xs) <- [splitAt c xs]
        , (space,xs) <- [splitAt 1 xs]
        , Just a <- [false ~> replicate gap False]
        , Just b <- [true ~> replicate c True]
        , Just c <- [space ~> replicate (length space) False]]

The first equation (second line) says that if there are no remaining constraints we set all remaining elements to `False`. We use the `~>` operator to check our desired assignment is consistent with the information already in the line:

    (~>) :: [Maybe Bool] -> [Bool] -> Maybe [Bool]
    (~>) xs ys | length xs == length ys &&
                 and (zipWith (\x y -> maybe True (== y) x) xs ys)
               = Just ys
    (~>) _ _ = Nothing

This function takes a line of the grid (which may have unknowns), and a possible line (which is entirely concrete), and either returns `Nothing` (inconsistent) or `Just` the proposed line. The check first checks the sizes are consistent, then ensures everything which is concrete (not a `Nothing`) matches the proposed value (we assume both lists are the same length, which we always ensure).

Returning to the second equation in `tile`, the idea is to compute how many gaps could occur at this point. Taking the example of a line 25 long, with two runs of size 3, we could have anywhere between 0 and 18 (25-3-3-1) spaces first. For each possible size of gap, we split the row up (the `splitAt` calls), then constrain each piece to match the pattern (using `~>`).

Given a way of returning all possible lines, we then need to collapse that into a single line, by marking all squares which could be either `True` or `False` as `Nothing`:

    constrainLine :: [Int] -> [Maybe Bool] -> Maybe [Maybe Bool]
    constrainLine cs xs = if null xs2 then Nothing else mapM f $ transpose xs2
        where xs2 = tile cs xs
              f (x:xs) = Just $ if not x `elem` xs then Nothing else Just x

If there are no satisfying assignments for the line, we return `Nothing` - that implies the constraints are unsatisfiable. Next, we scale up to a side of constraints, by combining all the constraints and lines:

    constrainSide :: [[Int]] -> [[Maybe Bool]] -> Maybe [[Maybe Bool]]
    constrainSide cs xs = sequence $ zipWith constrainLine cs xs

Finally, to constrain the entire grid, we just constrain one side, then the other. To simplify the code, we just transpose the grid in between, so we can treat the rows and columns identically:

    constrainGrid :: [[Int]] -> [[Int]] -> [[Maybe Bool]] -> Maybe [[Maybe Bool]]
    constrainGrid rows cols xs = fmap transpose . constrainSide cols . transpose =<< constrainSide rows xs

To constrain the whole problem we apply `constrainGrid` repeatedly, until it returns `Nothing` (the problem is unsatisfiable), we have a complete solution (problem solved), or nothing changes. If nothing changes then there might be two solutions, or our approach might not be powerful enough without using search.

### The Result

After four iterations we end up with a fully constrained answer. To see the progress, after one iteration we have:

    ..XXXXX...X.OO..X.XXXXX..
    ..OOOOX.X.O.....O.XOOOO..
    ..XXX.X....O...OX.X.XX...
    X.XXX.X....XXXXXX.X.XX...
    X.XXX.X..XXXX..XX.X.XX..X
    X.OOOOX...XO...OO.XOOO..X
    XXXXXXXOXOXOXOXOXOXXXXXXX
    ..OOO.OO..XOOOX.XOOOOO.O.
    ..XX..XX.OXOXOXXXOXO...X.
    ..XO..OO....OXX.O.O....X.
    ..X...X......X..X......O.
    ..O...O......XO.X........
    ..XX..X.X....O.OO.X......
    ..OXX.O.X....XXXX.X......
    ..XX..XXXXX..O.OO........
    ..X...O.X..O..O.X...O....
    ..X...X.X.OXO.O.X...X....
    ..OOO.O.X..O..O.X...X..X.
    X.XXXXX.......O.X...X..X.
    X.OOO.X.....XOO.X...X..X.
    X.XXX.X.....XXOOX...X...O
    XOXXXOXOXXXOXXXXXXXXXXOXX
    ..XXX.X.....XXXXX..XXXX.O
    ..OOOOX......OOOO...O.X..
    ..XXXXX......XOXX.O.X.X..

Here a `.` stands for `Nothing`. After four iterations we reach the answer in 0.34s:

    XXXXXXXOXXXOOOXOXOXXXXXXX
    XOOOOOXOXXOXXOOOOOXOOOOOX
    XOXXXOXOOOOOXXXOXOXOXXXOX
    XOXXXOXOXOOXXXXXXOXOXXXOX
    XOXXXOXOOXXXXXOXXOXOXXXOX
    XOOOOOXOOXXOOOOOOOXOOOOOX
    XXXXXXXOXOXOXOXOXOXXXXXXX
    OOOOOOOOXXXOOOXXXOOOOOOOO
    XOXXOXXXOOXOXOXXXOXOOXOXX
    XOXOOOOOOXXXOXXOOOOXOOOXO
    OXXXXOXOXXXXOXXOXOOOOXXOO
    OXOXOOOXOOOXOXOXXXXOXOXXX
    OOXXOOXOXOXOOOOOOXXOXXXXX
    OOOXXXOXXOXXOXXXXXXOXXXOX
    XOXXXXXXXXXOXOXOOXXOOOOXO
    OXXOXOOXXOOOXXOXXXOOOOOXO
    XXXOXOXOXOOXOOOOXXXXXOXOO
    OOOOOOOOXOOOXXOXXOOOXXXXX
    XXXXXXXOXOOXXOOOXOXOXOXXX
    XOOOOOXOXXOOXOOXXOOOXXOXO
    XOXXXOXOOOXXXXOOXXXXXOOXO
    XOXXXOXOXXXOXXXXXXXXXXOXX
    XOXXXOXOXOOXXXXXXOXXXXXXO
    XOOOOOXOOXXOOOOOOXOXOXXOO
    XXXXXXXOXXOOOXOXXOOOXXXXX
