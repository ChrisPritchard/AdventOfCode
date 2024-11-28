
year=2024
cookie=$(cat .env)

day=$1
if [[ -z $day ]]; then
    echo "please specify day"
    exit
fi

foldername=Day$(printf %02d $day)
mkdir $foldername

# project file, just including Program.fs
echo -e "<Project Sdk=\"Microsoft.NET.Sdk\">\n\t<PropertyGroup>\n\t\t<OutputType>Exe</OutputType>\n\t\t<TargetFramework>net8.0</TargetFramework>\n\t</PropertyGroup>\n\t<ItemGroup>\n\t\t<Compile Include=\"Program.fs\" />\n\t</ItemGroup>\n</Project>" > $foldername/$foldername.fsproj

curl -s --cookie $cookie https://adventofcode.com/$year/day/$day/input -fo $foldername/input.txt
if [ -f $foldername/input.txt ]; then
    echo -e "let input = System.IO.File.ReadAllLines \"input.txt\"\n\nprintfn \"%A\" input\n" >> $foldername/Program.fs
else
    echo -e "\nprintfn \"hello\"\n" >> $foldername/Program.fs
fi

# script that will fetch and create the readme with the challenge page content
echo -e "curl -s --cookie \$(cat ../.env) https://adventofcode.com/$year/day/$day | pandoc -f html -t markdown -o readme.md\nrm get-readme.sh" > $foldername/get-readme.sh