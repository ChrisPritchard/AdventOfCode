
year=2024
cookie=$(cat .env)

day=$1
if [[ -z $day ]]; then
    echo "please specify day"
    exit
fi

foldername=Day$(printf %02d $day)

curl -s --cookie $cookie https://adventofcode.com/$year/day/$day/input -fo $foldername/input.txt