package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	input_file, _ := os.Open("input.txt")
	defer input_file.Close()

	scanner := bufio.NewScanner(input_file)

	input := make([][]int, 0)

	part1 := 0

	for scanner.Scan() {
		line := scanner.Text()
		p := strings.Split(line, " ")

		row := make([]int, 0)
		for _, s := range p {
			if s == "" {
				fmt.Println("skipping empty")
				continue
			}
			if s == "+" {
				row = append(row, 0)
			} else if s == "*" {
				row = append(row, 1)
			} else {
				i, _ := strconv.Atoi(s)
				row = append(row, i)
			}
		}

		input = append(input, row)
	}

	for j, o := range input[len(input)-1] {
		if o == 0 {
			r := 0
			for i := 0; i < len(input)-2; i++ {
				r += input[i][j]
			}
			part1 += r
		} else {
			r := 1
			for i := 0; i < len(input)-2; i++ {
				r *= input[i][j]
			}
			part1 += r
		}
	}

	part2 := 0

	fmt.Println("Day 06 Part 01: ", part1)
	fmt.Println("Day 06 Part 02: ", part2)
}
