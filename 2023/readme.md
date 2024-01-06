# Advent of Code 2023

Hardest rooms were 12, 17 and 21. Later rooms, 22 onwards mainly were math heavy and some (23-24-25) involved heavy use of other user solutions for me to solve them.

New technique for this year was generating projects and downloading the problem text using bash scripts: [create-day.sh](./create-day.sh), which also creates a 'get-readme.sh' script to use when the problem is finished.

Up to day 16 the input was automatically saved into an Input.fs module as a string, however day 16 took far longer than it should have due to the bash script accidentally mangling the input for that day (it uses `echo -e` to process the input into the file, preserving new lines `\n`, however that day had `\\` in its input which echo mistakenly translated to `\`, ruining my results). From 17 onwards the input was saved as a text file and read by the code.

After the competition all input files were removed as per the comp's rules/preferences.
