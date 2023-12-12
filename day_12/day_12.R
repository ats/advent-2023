In the first line (???.### 1,1,3), there is exactly one way separate groups of one, one, and three broken springs (in that order) can appear in that row: the first three unknown springs must be broken, then operational, then broken (#.#), making the whole row #.#.###.

                   The second line is more interesting: .??..??...?##. 1,1,3 could be a total of four different arrangements. The last ? must always be broken (to satisfy the final contiguous group of three broken springs), and each ?? must hide exactly one of the two broken springs. (Neither ?? could be both broken springs or they would form a single contiguous group of two; if that were true, the numbers afterward would have been 2,3 instead.) Since each ?? can either be #. or .#, there are four possible arrangements of springs.

                     The last line is actually consistent with ten different arrangements! Because the first number is 3, the first and second ? must both be . (if either were #, the first number would have to be 4 or higher). However, the remaining run of unknown spring conditions have many different ways they could hold groups of two and one broken springs:

 ?###???????? 3,2,1
   .###.##.#...
 .###.##..#..
 .###.##...#.
 .###.##....#
 .###..##.#..
 .###..##..#.
 .###..##...#
 .###...##.#.
 .###...##..#
 .###....##.#
 In this example, the number of possible arrangements for each row is:

   ???.### 1,1,3 - 1 arrangement
 .??..??...?##. 1,1,3 - 4 arrangements
   ?#?#?#?#?#?#?#? 1,3,1,6 - 1 arrangement
   ????.#...#... 4,1,1 - 1 arrangement
 ????.######..#####. 1,6,5 - 4 arrangements
 ?###???????? 3,2,1 - 10 arrangements
                                                                                                                                                                   Adding all of the possible arrangement counts together produces a total of 21 arrangements.


