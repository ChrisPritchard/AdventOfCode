package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func main() {
	file, err := os.Open("./inputs/day03.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	part1, part2 := 0, 0

	group := []string{}

	for scanner.Scan() {
		line := scanner.Text()

		mid := len(line) / 2
		left := line[0:mid]
		right := line[mid:]
		m := intersect(left, right)
		part1 += score(m)

		group = append(group, line)
		if len(group) == 3 {
			n := intersect(group[0], group[1])
			n = intersect(n, group[2])
			part2 += score(n)
			group = []string{}
		}
	}

	fmt.Printf("Day 03 Part 1: %d\n", part1)
	fmt.Printf("Day 03 Part 2: %d\n", part2)
}

func intersect(left, right string) string {
	matched := ""
	for _, l := range left {
		for _, r := range right {
			if l == r {
				matched += string(l)
				break
			}
		}
	}
	return matched
}

func score(m string) int {
	if m[0] <= 'Z' {
		return int(m[0]-'A') + 27
	} else {
		return int(m[0]-'a') + 1
	}
}
