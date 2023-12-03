<div>

[Advent of Code](/) {#advent-of-code .title-global}
===================

-   [\[About\]](/2023/about)
-   [\[Events\]](/2023/events)
-   [\[Shop\]](https://teespring.com/stores/advent-of-code)
-   [\[Settings\]](/2023/settings)
-   [\[Log Out\]](/2023/auth/logout)

::: {.user}
Christopher Pritchard [6\*]{.star-count}
:::

</div>

<div>

          []{.title-event-wrap}[2023](/2023)[]{.title-event-wrap} {#section .title-event}
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
[Boot.dev](https://www.boot.dev?promo=ADVENTOFCODE) - Ready to become a
backend developer? If you like AoC, you might be like us. We think
smartest way to learn to code is to ensure you\'re never bored. Try the
most captivating, addictive way to learn to code on Boot.dev.
:::
:::
:::

::: {role="main"}
\-\-- Day 3: Gear Ratios \-\--
------------------------------

You and the Elf eventually reach a [gondola
lift](https://en.wikipedia.org/wiki/Gondola_lift) station; he says the
gondola lift will take you up to the *water source*, but this is as far
as he can bring you. You go inside.

It doesn\'t take long to find the gondolas, but there seems to be a
problem: they\'re not moving.

\"Aaah!\"

You turn around to see a slightly-greasy Elf with a wrench and a look of
surprise. \"Sorry, I wasn\'t expecting anyone! The gondola lift isn\'t
working right now; it\'ll still be a while before I can fix it.\" You
offer to help.

The engineer explains that an engine part seems to be missing from the
engine, but nobody can figure out which one. If you can *add up all the
part numbers* in the engine schematic, it should be easy to work out
which part is missing.

The engine schematic (your puzzle input) consists of a visual
representation of the engine. There are lots of numbers and symbols you
don\'t really understand, but apparently *any number adjacent to a
symbol*, even diagonally, is a \"part number\" and should be included in
your sum. (Periods (`.`) do not count as a symbol.)

Here is an example engine schematic:

    467..114..
    ...*......
    ..35..633.
    ......#...
    617*......
    .....+.58.
    ..592.....
    ......755.
    ...$.*....
    .664.598..

In this schematic, two numbers are *not* part numbers because they are
not adjacent to a symbol: `114` (top right) and `58` (middle right).
Every other number is adjacent to a symbol and so *is* a part number;
their sum is `4361`.

Of course, the actual engine schematic is much larger. *What is the sum
of all of the part numbers in the engine schematic?*

Your puzzle answer was `559667`.

\-\-- Part Two \-\-- {#part2}
--------------------

The engineer finds the missing part and installs it in the engine! As
the engine springs to life, you jump in the closest gondola, finally
ready to ascend to the water source.

You don\'t seem to be going very fast, though. Maybe something is still
wrong? Fortunately, the gondola has a phone labeled \"help\", so you
pick it up and the engineer answers.

Before you can explain the situation, she suggests that you look out the
window. There stands the engineer, holding a phone in one hand and
waving with the other. You\'re going so slowly that you haven\'t even
left the station. You exit the gondola.

The missing part wasn\'t the only issue - one of the gears in the engine
is wrong. A *gear* is any `*` symbol that is adjacent to *exactly two
part numbers*. Its *gear ratio* is the result of
[multiplying]{title="They're magic gears."} those two numbers together.

This time, you need to find the gear ratio of every gear and add them
all up so that the engineer can figure out which gear needs to be
replaced.

Consider the same engine schematic again:

    467..114..
    ...*......
    ..35..633.
    ......#...
    617*......
    .....+.58.
    ..592.....
    ......755.
    ...$.*....
    .664.598..

In this schematic, there are *two* gears. The first is in the top left;
it has part numbers `467` and `35`, so its gear ratio is `16345`. The
second gear is in the lower right; its gear ratio is `451490`. (The `*`
adjacent to `617` is *not* a gear because it is only adjacent to one
part number.) Adding up all of the gear ratios produces `467835`.

*What is the sum of all of the gear ratios in your engine schematic?*

Your puzzle answer was `86841457`.

Both parts of this puzzle are complete! They provide two gold stars:
\*\*

At this point, you should [return to your Advent calendar](/2023) and
try another puzzle.

If you still want to see it, you can [get your puzzle input](3/input).

You can also [\[Share[on
[Twitter](https://twitter.com/intent/tweet?text=I%27ve+completed+%22Gear+Ratios%22+%2D+Day+3+%2D+Advent+of+Code+2023&url=https%3A%2F%2Fadventofcode%2Ecom%2F2023%2Fday%2F3&related=ericwastl&hashtags=AdventOfCode)
[Mastodon](javascript:void(0);)]{.share-content}\]]{.share} this puzzle.
:::
