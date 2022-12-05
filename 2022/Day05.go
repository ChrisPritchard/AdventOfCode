package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func main() {
	fmt.Printf("Day 05 Part 1: %s\n", crateMover(true))
	fmt.Printf("Day 05 Part 2: %s\n", crateMover(false))
}

func crateMover(reverse bool) string {
	file, err := os.Open("./inputs/day05.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	stacks := []string{}

	for scanner.Scan() {
		line := scanner.Text()

		if !strings.HasPrefix(line, "move") {
			if len(line) == 0 || line[1] == '1' {
				continue
			}
			for i := 0; i < (len(line)+1)/4; i++ {
				if len(stacks) < (i + 1) {
					stacks = append(stacks, "")
				}
				crate := line[(i*4)+1]
				if crate != ' ' {
					stacks[i] = stacks[i] + string(crate)
				}
			}
			continue
		}

		parts := strings.Split(line, " ")
		amount, _ := strconv.Atoi(parts[1])
		from, _ := strconv.Atoi(parts[3])
		to, _ := strconv.Atoi(parts[5])

		toMove := stacks[from-1][0:amount]
		if reverse {
			rev := ""
			for _, v := range toMove {
				rev = string(v) + rev
			}
			toMove = rev
		}

		stacks[from-1] = stacks[from-1][amount:]
		stacks[to-1] = toMove + stacks[to-1]
	}

	result := ""
	for _, v := range stacks {
		result += string(v[0])
	}
	return result
}
