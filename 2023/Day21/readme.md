<div>

[Advent of Code](/) {#advent-of-code .title-global}
===================

-   [\[About\]](/2023/about)
-   [\[Events\]](/2023/events)
-   [\[Shop\]](https://teespring.com/stores/advent-of-code)
-   [\[Settings\]](/2023/settings)
-   [\[Log Out\]](/2023/auth/logout)

::: {.user}
Christopher Pritchard [42\*]{.star-count}
:::

</div>

<div>

   [var y=]{.title-event-wrap}[2023](/2023)[;]{.title-event-wrap} {#var-y2023 .title-event}
=================================================================

-   [\[Calendar\]](/2023)
-   [\[AoC++\]](/2023/support)
-   [\[Sponsors\]](/2023/sponsors)
-   [\[Leaderboard\]](/2023/leaderboard)
-   [\[Stats\]](/2023/stats)

</div>

::: {#sidebar}
::: {#sponsor}
::: {.quiet}
Our [sponsors](/2023/sponsors) help make Advent of Code possible:
:::

::: {.sponsor}
[REWE digital](https://www.rewe-digital.com/en) - Ho Ho Home of IT: Give
yourself a gift and reimagine the digital future of retail in Germany,
Austria, Bulgaria or Spain with us!
:::
:::
:::

::: {role="main"}
\-\-- Day 21: Step Counter \-\--
--------------------------------

You manage to catch the [airship](7) right as it\'s dropping someone
else off on their all-expenses-paid trip to Desert Island! It even
helpfully drops you off near the [gardener](5) and his massive farm.

\"You got the sand flowing again! Great work! Now we just need to wait
until we have enough sand to filter the water for Snow Island and we\'ll
have snow again in no time.\"

While you wait, one of the Elves that works with the gardener heard how
good you are at solving problems and would like your help. He needs to
get his [steps](https://en.wikipedia.org/wiki/Pedometer) in for the day,
and so he\'d like to know *which garden plots he can reach with exactly
his remaining `64` steps*.

He gives you an up-to-date map (your puzzle input) of his starting
position (`S`), garden plots (`.`), and rocks (`#`). For example:

    ...........
    .....###.#.
    .###.##..#.
    ..#.#...#..
    ....#.#....
    .##..S####.
    .##..#...#.
    .......##..
    .##.#.####.
    .##..##.##.
    ...........

The Elf starts at the starting position (`S`) which also counts as a
garden plot. Then, he can take one step north, south, east, or west, but
only onto tiles that are garden plots. This would allow him to reach any
of the tiles marked `O`:

    ...........
    .....###.#.
    .###.##..#.
    ..#.#...#..
    ....#O#....
    .##.OS####.
    .##..#...#.
    .......##..
    .##.#.####.
    .##..##.##.
    ...........

Then, he takes a second step. Since at this point he could be at
*either* tile marked `O`, his second step would allow him to reach any
garden plot that is one step north, south, east, or west of *any* tile
that he could have reached after the first step:

    ...........
    .....###.#.
    .###.##..#.
    ..#.#O..#..
    ....#.#....
    .##O.O####.
    .##.O#...#.
    .......##..
    .##.#.####.
    .##..##.##.
    ...........

After two steps, he could be at any of the tiles marked `O` above,
including the starting position (either by going north-then-south or by
going west-then-east).

A single third step leads to even more possibilities:

    ...........
    .....###.#.
    .###.##..#.
    ..#.#.O.#..
    ...O#O#....
    .##.OS####.
    .##O.#...#.
    ....O..##..
    .##.#.####.
    .##..##.##.
    ...........

He will continue like this until his steps for the day have been
exhausted. After a total of `6` steps, he could reach any of the garden
plots marked `O`:

    ...........
    .....###.#.
    .###.##.O#.
    .O#O#O.O#..
    O.O.#.#.O..
    .##O.O####.
    .##.O#O..#.
    .O.O.O.##..
    .##.#.####.
    .##O.##.##.
    ...........

In this example, if the Elf\'s goal was to get exactly `6` more steps
today, he could use them to reach any of `16` garden plots.

However, the Elf *actually needs to get `64` steps today*, and the map
he\'s handed you is much larger than the example map.

Starting from the garden plot marked `S` on your map, *how many garden
plots could the Elf reach in exactly `64` steps?*

Your puzzle answer was `3764`.

\-\-- Part Two \-\-- {#part2}
--------------------

The Elf seems confused by your answer until he realizes his mistake: he
was reading from a [list]{title="Next up: 729."} of his favorite numbers
that are both perfect squares and perfect cubes, not his step counter.

The *actual* number of steps he needs to get today is exactly
`26501365`.

He also points out that the garden plots and rocks are set up so that
the map *repeats infinitely* in every direction.

So, if you were to look one additional map-width or map-height out from
the edge of the example map above, you would find that it keeps
repeating:

    .................................
    .....###.#......###.#......###.#.
    .###.##..#..###.##..#..###.##..#.
    ..#.#...#....#.#...#....#.#...#..
    ....#.#........#.#........#.#....
    .##...####..##...####..##...####.
    .##..#...#..##..#...#..##..#...#.
    .......##.........##.........##..
    .##.#.####..##.#.####..##.#.####.
    .##..##.##..##..##.##..##..##.##.
    .................................
    .................................
    .....###.#......###.#......###.#.
    .###.##..#..###.##..#..###.##..#.
    ..#.#...#....#.#...#....#.#...#..
    ....#.#........#.#........#.#....
    .##...####..##..S####..##...####.
    .##..#...#..##..#...#..##..#...#.
    .......##.........##.........##..
    .##.#.####..##.#.####..##.#.####.
    .##..##.##..##..##.##..##..##.##.
    .................................
    .................................
    .....###.#......###.#......###.#.
    .###.##..#..###.##..#..###.##..#.
    ..#.#...#....#.#...#....#.#...#..
    ....#.#........#.#........#.#....
    .##...####..##...####..##...####.
    .##..#...#..##..#...#..##..#...#.
    .......##.........##.........##..
    .##.#.####..##.#.####..##.#.####.
    .##..##.##..##..##.##..##..##.##.
    .................................

This is just a tiny three-map-by-three-map slice of the
inexplicably-infinite farm layout; garden plots and rocks repeat as far
as you can see. The Elf still starts on the one middle tile marked `S`,
though - every other repeated `S` is replaced with a normal garden plot
(`.`).

Here are the number of reachable garden plots in this new infinite
version of the example map for different numbers of steps:

-   In exactly `6` steps, he can still reach `16` garden plots.
-   In exactly `10` steps, he can reach any of `50` garden plots.
-   In exactly `50` steps, he can reach `1594` garden plots.
-   In exactly `100` steps, he can reach `6536` garden plots.
-   In exactly `500` steps, he can reach `167004` garden plots.
-   In exactly `1000` steps, he can reach `668697` garden plots.
-   In exactly `5000` steps, he can reach `16733044` garden plots.

However, the step count the Elf needs is much larger! Starting from the
garden plot marked `S` on your infinite map, *how many garden plots
could the Elf reach in exactly `26501365` steps?*

Your puzzle answer was `622926941971282`.

Both parts of this puzzle are complete! They provide two gold stars:
\*\*

At this point, you should [return to your Advent calendar](/2023) and
try another puzzle.

If you still want to see it, you can [get your puzzle input](21/input).

You can also [\[Share[on
[Twitter](https://twitter.com/intent/tweet?text=I%27ve+completed+%22Step+Counter%22+%2D+Day+21+%2D+Advent+of+Code+2023&url=https%3A%2F%2Fadventofcode%2Ecom%2F2023%2Fday%2F21&related=ericwastl&hashtags=AdventOfCode)
[Mastodon](javascript:void(0);)]{.share-content}\]]{.share} this puzzle.
:::
