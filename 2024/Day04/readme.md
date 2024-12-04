<div>

[Advent of Code](/) {#advent-of-code .title-global}
===================

-   [\[About\]](/2024/about)
-   [\[Events\]](/2024/events)
-   [\[Shop\]](https://cottonbureau.com/people/advent-of-code)
-   [\[Settings\]](/2024/settings)
-   [\[Log Out\]](/2024/auth/logout)

::: {.user}
Christopher Pritchard [8\*]{.star-count}
:::

</div>

<div>

      [/\*]{.title-event-wrap}[2024](/2024)[\*/]{.title-event-wrap} {#section .title-event}
===================================================================

-   [\[Calendar\]](/2024)
-   [\[AoC++\]](/2024/support)
-   [\[Sponsors\]](/2024/sponsors)
-   [\[Leaderboard\]](/2024/leaderboard)
-   [\[Stats\]](/2024/stats)

</div>

::: {#sidebar}
::: {#sponsor}
::: {.quiet}
Our [sponsors](/2024/sponsors) help make Advent of Code possible:
:::

::: {.sponsor}
[Jane
Street](/2024/sponsors/redirect?url=https%3A%2F%2Fwww%2Ejanestreet%2Ecom%2F)
- We\'re a research-driven trading firm where curious people work
together to solve the puzzle of global markets. Will our next great idea
come from you? Our largest offices are in NYC, London, Hong Kong, and
Singapore.
:::
:::
:::

::: {role="main"}
\-\-- Day 4: Ceres Search \-\--
-------------------------------

\"Looks like the Chief\'s not here. Next!\" One of The Historians pulls
out a device and pushes the only button on it. After a brief flash, you
recognize the interior of the [Ceres monitoring station](/2019/day/10)!

As the search for the Chief continues, a small Elf who lives on the
station tugs on your shirt; she\'d like to know if you could help her
with her *word search* (your puzzle input). She only has to find one
word: `XMAS`.

This word search allows words to be horizontal, vertical, diagonal,
written backwards, or even overlapping other words. It\'s a little
unusual, though, as you don\'t merely need to find one instance of
`XMAS` - you need to find *all of them*. Here are a few ways `XMAS`
might appear, where irrelevant characters have been replaced with `.`:

    ..X...
    .SAMX.
    .A..A.
    XMAS.S
    .X....

The actual word search will be full of letters instead. For example:

    MMMSXXMASM
    MSAMXMSMSA
    AMXSXMAAMM
    MSAMASMSMX
    XMASAMXAMM
    XXAMMXXAMA
    SMSMSASXSS
    SAXAMASAAA
    MAMMMXMMMM
    MXMXAXMASX

In this word search, `XMAS` occurs a total of `18` times; here\'s the
same word search again, but where letters not involved in any `XMAS`
have been replaced with `.`:

    ....XXMAS.
    .SAMXMS...
    ...S..A...
    ..A.A.MS.X
    XMASAMX.MM
    X.....XA.A
    S.S.S.S.SS
    .A.A.A.A.A
    ..M.M.M.MM
    .X.X.XMASX

Take a look at the little Elf\'s word search. *How many times does
`XMAS` appear?*

Your puzzle answer was `2500`.

\-\-- Part Two \-\-- {#part2}
--------------------

The Elf looks quizzically at you. Did you misunderstand the assignment?

Looking for the instructions, you flip over the word search to find that
this isn\'t actually an `XMAS` puzzle; it\'s an
[`X-MAS`]{title="This part originally involved searching for something else, but this joke was too dumb to pass up."}
puzzle in which you\'re supposed to find two `MAS` in the shape of an
`X`. One way to achieve that is like this:

    M.S
    .A.
    M.S

Irrelevant characters have again been replaced with `.` in the above
diagram. Within the `X`, each `MAS` can be written forwards or
backwards.

Here\'s the same example from before, but this time all of the `X-MAS`es
have been kept instead:

    .M.S......
    ..A..MSMS.
    .M.S.MAA..
    ..A.ASMSM.
    .M.S.M....
    ..........
    S.S.S.S.S.
    .A.A.A.A..
    M.M.M.M.M.
    ..........

In this example, an `X-MAS` appears `9` times.

Flip the word search from the instructions back over to the word search
side and try again. *How many times does an `X-MAS` appear?*

Your puzzle answer was `1933`.

Both parts of this puzzle are complete! They provide two gold stars:
\*\*

At this point, you should [return to your Advent calendar](/2024) and
try another puzzle.

If you still want to see it, you can [get your puzzle input](4/input).

You can also [\[Share[on
[Bluesky](https://bsky.app/intent/compose?text=I%27ve+completed+%22Ceres+Search%22+%2D+Day+4+%2D+Advent+of+Code+2024+%23AdventOfCode+https%3A%2F%2Fadventofcode%2Ecom%2F2024%2Fday%2F4)
[Twitter](https://twitter.com/intent/tweet?text=I%27ve+completed+%22Ceres+Search%22+%2D+Day+4+%2D+Advent+of+Code+2024&url=https%3A%2F%2Fadventofcode%2Ecom%2F2024%2Fday%2F4&related=ericwastl&hashtags=AdventOfCode)
[Mastodon](javascript:void(0);)]{.share-content}\]]{.share} this puzzle.
:::
