package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func main() {
	input_file, _ := os.Open("input.txt")
	defer input_file.Close()

	input := make([]string, 0)
	scanner := bufio.NewScanner(input_file)
	for scanner.Scan() {
		input = append(input, scanner.Text())
	}

	part1 := 0
	part2 := 0

	start := strings.Index(input[0], "S")
	beams := make(map[int]int)
	beams[start] = 1

	for i := 1; i < len(input); i++ {
		for j, c := range input[i] {
			if c == '^' {
				if n, exists := beams[j]; exists {
					delete(beams, j)
					l := beams[j-1]
					beams[j-1] = l + n
					r := beams[j+1]
					beams[j+1] = r + n
					part1++
				}
			}
		}
	}

	for _, v := range beams {
		part2 += v
	}

	fmt.Println("Day 07 Part 01: ", part1)
	fmt.Println("Day 07 Part 02: ", part2)
}
