
year=2023
cookie="session=53616c7465645f5fbfd281cafeb37d70d432c41db6e0725d4f200d0ad02747e711be3efb26f46531d8d1694596adaa1e5f3c7c2928485626d84d3d010239ef0e"

foldername=Day$(date +%d)
mkdir $foldername

echo -e "<Project Sdk=\"Microsoft.NET.Sdk\">\n\t<PropertyGroup>\n\t\t<OutputType>Exe</OutputType>\n\t\t<TargetFramework>net8.0</TargetFramework>\n\t</PropertyGroup>\n\t<ItemGroup>" > $foldername/$foldername.fsproj

input=$(curl -s --cookie $cookie https://adventofcode.com/$year/day/$(date +%-d)/input)
if [[ $input != "404 Not Found" ]]; then
    echo -e "module Input\n\nlet value=\"\"\"$input\"\"\"" > $foldername/Input.fs
    echo -e "\t\t<Compile Include=\"Input.fs\" />" >> $foldername/$foldername.fsproj
    echo -e "let input = Input.value\n\nprintfn \"%s\" input\n" >> $foldername/Program.fs
else
    echo -e "\nprintfn \"hello\"\n" >> $foldername/Program.fs
fi

echo -e "\t\t<Compile Include=\"Program.fs\" />\n\t</ItemGroup>\n</Project>" >> $foldername/$foldername.fsproj

