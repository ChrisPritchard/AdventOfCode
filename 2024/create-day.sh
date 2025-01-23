# update the year, ensure the full cookie is in a .env file (format is 'session=sdadadasd' etc)
# run as `bash create-day.sh 1` etc, note the non-padded day number; it will be padded in the folder creation automatically

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
