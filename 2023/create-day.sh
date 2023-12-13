
year=2023
cookie=$(cat .env)

day=$1
if [[ -z $day ]]; then
    echo "please specify day"
    exit
fi

foldername=Day$(printf %02d $day)
mkdir $foldername

echo -e "<Project Sdk=\"Microsoft.NET.Sdk\">\n\t<PropertyGroup>\n\t\t<OutputType>Exe</OutputType>\n\t\t<TargetFramework>net8.0</TargetFramework>\n\t</PropertyGroup>\n\t<ItemGroup>" > $foldername/$foldername.fsproj

input=$(curl -s --cookie $cookie https://adventofcode.com/$year/day/$day/input)
if [[ $input != "404 Not Found" ]]; then
    echo -e "module Input\n\nlet value=\"\"\"$input\"\"\"" > $foldername/Input.fs
    echo -e "\t\t<Compile Include=\"Input.fs\" />" >> $foldername/$foldername.fsproj
    echo -e "let input = Input.value\n\nprintfn \"%s\" input\n" >> $foldername/Program.fs
else
    echo -e "\nprintfn \"hello\"\n" >> $foldername/Program.fs
fi

echo -e "\t\t<Compile Include=\"Program.fs\" />\n\t</ItemGroup>\n</Project>" >> $foldername/$foldername.fsproj

echo -e "curl -s --cookie \$(cat ../.env) https://adventofcode.com/$year/day/$day | pandoc -f html -t markdown -o readme.md\nrm get-readme.sh" > $foldername/get-readme.sh