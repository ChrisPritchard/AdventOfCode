
year=2023
cookie="session=53616c7465645f5fbfd281cafeb37d70d432c41db6e0725d4f200d0ad02747e711be3efb26f46531d8d1694596adaa1e5f3c7c2928485626d84d3d010239ef0e"

day=$(date +%-d)
curl -s --cookie $cookie https://adventofcode.com/$year/day/$day | pandoc -f html -t markdown -o Day$(date +%d)/readme.md
