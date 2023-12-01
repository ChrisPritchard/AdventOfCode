<div>

[Advent of Code](/) {#advent-of-code .title-global}
===================

-   [\[About\]](/2023/about)
-   [\[Events\]](/2023/events)
-   [\[Shop\]](https://teespring.com/stores/advent-of-code)
-   [\[Settings\]](/2023/settings)
-   [\[Log Out\]](/2023/auth/logout)

::: {.user}
Christopher Pritchard [2\*]{.star-count}
:::

</div>

<div>

       [λy.]{.title-event-wrap}[2023](/2023)[]{.title-event-wrap} {#λy.2023 .title-event}
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
[Accenture Federal
Services](https://www.accenture.com/us-en/industries/afs-index) -
Technology & ingenuity moving missions forward -- come solve problems
with us. Hiring software engineers, developers, and more now. Refer
someone to earn up to \$20K.
:::
:::
:::

::: {role="main"}
\-\-- Day 1: Trebuchet?! \-\--
------------------------------

Something is wrong with global snow production, and you\'ve been
selected to take a look. The Elves have even given you a map; on it,
they\'ve used stars to mark the top fifty locations that are likely to
be having problems.

You\'ve been doing this long enough to know that to restore snow
operations, you need to check all *fifty stars* by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on
each day in the Advent calendar; the second puzzle is unlocked when you
complete the first. Each puzzle grants *one star*. Good luck!

You try to ask why they can\'t just use a [weather machine](/2015/day/1)
(\"not powerful enough\") and where they\'re even sending you (\"the
sky\") and why your map looks mostly blank (\"you sure ask a lot of
questions\")
[and]{title="My hope is that this abomination of a run-on sentence somehow conveys the chaos of being hastily loaded into a trebuchet."}
hang on did you just say the sky (\"of course, where do you think snow
comes from\") when you realize that the Elves are already loading you
into a [trebuchet](https://en.wikipedia.org/wiki/Trebuchet) (\"please
hold still, we need to strap you in\").

As they\'re making the final adjustments, they discover that their
calibration document (your puzzle input) has been *amended* by a very
young Elf who was apparently just excited to show off her art skills.
Consequently, the Elves are having trouble reading the values on the
document.

The newly-improved calibration document consists of lines of text; each
line originally contained a specific *calibration value* that the Elves
now need to recover. On each line, the calibration value can be found by
combining the *first digit* and the *last digit* (in that order) to form
a single *two-digit number*.

For example:

    1abc2
    pqr3stu8vwx
    a1b2c3d4e5f
    treb7uchet

In this example, the calibration values of these four lines are `12`,
`38`, `15`, and `77`. Adding these together produces `142`.

Consider your entire calibration document. *What is the sum of all of
the calibration values?*

Your puzzle answer was `56108`.

\-\-- Part Two \-\-- {#part2}
--------------------

Your calculation isn\'t quite right. It looks like some of the digits
are actually *spelled out with letters*: `one`, `two`, `three`, `four`,
`five`, `six`, `seven`, `eight`, and `nine` *also* count as valid
\"digits\".

Equipped with this new information, you now need to find the real first
and last digit on each line. For example:

    two1nine
    eightwothree
    abcone2threexyz
    xtwone3four
    4nineeightseven2
    zoneight234
    7pqrstsixteen

In this example, the calibration values are `29`, `83`, `13`, `24`,
`42`, `14`, and `76`. Adding these together produces `281`.

*What is the sum of all of the calibration values?*

Your puzzle answer was `55652`.

Both parts of this puzzle are complete! They provide two gold stars:
\*\*

At this point, you should [return to your Advent calendar](/2023) and
try another puzzle.

If you still want to see it, you can [get your puzzle input](1/input).

You can also [\[Share[on
[Twitter](https://twitter.com/intent/tweet?text=I%27ve+completed+%22Trebuchet%3F%21%22+%2D+Day+1+%2D+Advent+of+Code+2023&url=https%3A%2F%2Fadventofcode%2Ecom%2F2023%2Fday%2F1&related=ericwastl&hashtags=AdventOfCode)
[Mastodon](javascript:void(0);)]{.share-content}\]]{.share} this puzzle.
:::
