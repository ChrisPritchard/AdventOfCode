curl -s --cookie $(cat ../.env) https://adventofcode.com/2024/day/20 | pandoc -f html -t markdown -o readme.md
rm get-readme.sh
