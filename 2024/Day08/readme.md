<div>

[Advent of Code](/) {#advent-of-code .title-global}
===================

-   [\[About\]](/2024/about)
-   [\[Events\]](/2024/events)
-   [\[Shop\]](https://cottonbureau.com/people/advent-of-code)
-   [\[Settings\]](/2024/settings)
-   [\[Log Out\]](/2024/auth/logout)

::: {.user}
Christopher Pritchard [16\*]{.star-count}
:::

</div>

<div>

  [0.0.0.0:]{.title-event-wrap}[2024](/2024)[]{.title-event-wrap} {#section .title-event}
=================================================================

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
[Ahrefs](/2024/sponsors/redirect?url=https%3A%2F%2Fahrefs%2Ecom%2Fabout)
- We crawl the internet, all of it. Then we parse, store, index and
query, all of it. And that\'s how we build web analytics and a search
engine. What else could we do with 400 billion htmls? :thinking\_face:
BTW we use OCaml
:::
:::
:::

::: {role="main"}
\-\-- Day 8: Resonant Collinearity \-\--
----------------------------------------

You find yourselves on the [roof](/2016/day/25) of a top-secret Easter
Bunny installation.

While The Historians do their thing, you take a look at the familiar
*huge antenna*. Much to your surprise, it seems to have been
reconfigured to emit a signal that makes people 0.1% more likely to buy
Easter Bunny brand [Imitation
Mediocre]{title="They could have imitated delicious chocolate, but the mediocre chocolate is WAY easier to imitate."}
Chocolate as a Christmas gift! Unthinkable!

Scanning across the city, you find that there are actually many such
antennas. Each antenna is tuned to a specific *frequency* indicated by a
single lowercase letter, uppercase letter, or digit. You create a map
(your puzzle input) of these antennas. For example:

    ............
    ........0...
    .....0......
    .......0....
    ....0.......
    ......A.....
    ............
    ............
    ........A...
    .........A..
    ............
    ............

The signal only applies its nefarious effect at specific *antinodes*
based on the resonant frequencies of the antennas. In particular, an
antinode occurs at any point that is perfectly in line with two antennas
of the same frequency - but only when one of the antennas is twice as
far away as the other. This means that for any pair of antennas with the
same frequency, there are two antinodes, one on either side of them.

So, for these two antennas with frequency `a`, they create the two
antinodes marked with `#`:

    ..........
    ...#......
    ..........
    ....a.....
    ..........
    .....a....
    ..........
    ......#...
    ..........
    ..........

Adding a third antenna with the same frequency creates several more
antinodes. It would ideally add four antinodes, but two are off the
right side of the map, so instead it adds only two:

    ..........
    ...#......
    #.........
    ....a.....
    ........a.
    .....a....
    ..#.......
    ......#...
    ..........
    ..........

Antennas with different frequencies don\'t create antinodes; `A` and `a`
count as different frequencies. However, antinodes *can* occur at
locations that contain antennas. In this diagram, the lone antenna with
frequency capital `A` creates no antinodes but has a
lowercase-`a`-frequency antinode at its location:

    ..........
    ...#......
    #.........
    ....a.....
    ........a.
    .....a....
    ..#.......
    ......A...
    ..........
    ..........

The first example has antennas with two different frequencies, so the
antinodes they create look like this, plus an antinode overlapping the
topmost `A`-frequency antenna:

    ......#....#
    ...#....0...
    ....#0....#.
    ..#....0....
    ....0....#..
    .#....A.....
    ...#........
    #......#....
    ........A...
    .........A..
    ..........#.
    ..........#.

Because the topmost `A`-frequency antenna overlaps with a `0`-frequency
antinode, there are `14` total unique locations that contain an antinode
within the bounds of the map.

Calculate the impact of the signal. *How many unique locations within
the bounds of the map contain an antinode?*

Your puzzle answer was `299`.

\-\-- Part Two \-\-- {#part2}
--------------------

Watching over your shoulder as you work, one of The Historians asks if
you took the effects of resonant harmonics into your calculations.

Whoops!

After updating your model, it turns out that an antinode occurs at *any
grid position* exactly in line with at least two antennas of the same
frequency, regardless of distance. This means that some of the new
antinodes will occur at the position of each antenna (unless that
antenna is the only one of its frequency).

So, these three `T`-frequency antennas now create many antinodes:

    T....#....
    ...T......
    .T....#...
    .........#
    ..#.......
    ..........
    ...#......
    ..........
    ....#.....
    ..........

In fact, the three `T`-frequency antennas are all exactly in line with
two antennas, so they are all also antinodes! This brings the total
number of antinodes in the above example to `9`.

The original example now has `34` antinodes, including the antinodes
that appear on every antenna:

    ##....#....#
    .#.#....0...
    ..#.#0....#.
    ..##...0....
    ....0....#..
    .#...#A....#
    ...#..#.....
    #....#.#....
    ..#.....A...
    ....#....A..
    .#........#.
    ...#......##

Calculate the impact of the signal using this updated model. *How many
unique locations within the bounds of the map contain an antinode?*

Your puzzle answer was `1032`.

Both parts of this puzzle are complete! They provide two gold stars:
\*\*

At this point, you should [return to your Advent calendar](/2024) and
try another puzzle.

If you still want to see it, you can [get your puzzle input](8/input).

You can also [\[Share[on
[Bluesky](https://bsky.app/intent/compose?text=I%27ve+completed+%22Resonant+Collinearity%22+%2D+Day+8+%2D+Advent+of+Code+2024+%23AdventOfCode+https%3A%2F%2Fadventofcode%2Ecom%2F2024%2Fday%2F8)
[Twitter](https://twitter.com/intent/tweet?text=I%27ve+completed+%22Resonant+Collinearity%22+%2D+Day+8+%2D+Advent+of+Code+2024&url=https%3A%2F%2Fadventofcode%2Ecom%2F2024%2Fday%2F8&related=ericwastl&hashtags=AdventOfCode)
[Mastodon](javascript:void(0);)]{.share-content}\]]{.share} this puzzle.
:::
