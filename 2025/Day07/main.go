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
	part2 := 1

	start := strings.Index(input[0], "S")
	beams := make(map[int]struct{})
	beams[start] = struct{}{}

	for i := 1; i < len(input); i++ {
		for j, c := range input[i] {
			if c == '^' {
				if _, exists := beams[j]; exists {
					delete(beams, j)
					beams[j-1] = struct{}{}
					beams[j+1] = struct{}{}
					part1++
					part2 *= 2
				}
			}
		}
	}

	fmt.Println("Day 07 Part 01: ", part1)
	fmt.Println("Day 07 Part 02: ", part2)
}
