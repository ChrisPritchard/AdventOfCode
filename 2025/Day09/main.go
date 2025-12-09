package main

import (
	"bufio"
	"fmt"
	"os"
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

	fmt.Println("Day 09 Part 01: ", part1)
	fmt.Println("Day 09 Part 02: ", part2)
}
