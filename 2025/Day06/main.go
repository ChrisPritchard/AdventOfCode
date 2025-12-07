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

	input := make([]string, 0)
	scanner := bufio.NewScanner(input_file)
	for scanner.Scan() {
		input = append(input, scanner.Text())
	}

	op_row := len(input) - 1
	part1 := 0
	part2 := 0

	s := 2 // start high for last col
	for i := len(input[op_row]) - 1; i >= 0; i-- {
		switch input[op_row][i] {
		case ' ':
			s++
		case '+':
			r1 := 0
			for j := range op_row {
				n := input[j][i : i+s]
				v, _ := strconv.Atoi(strings.TrimSpace(n))
				r1 += v
			}
			part1 += r1

			r2 := 0
			for k := range s {
				v := 0
				for j := range op_row {
					c := input[j][i+k]
					if c != ' ' {
						v += int(c) - int('0')
						v *= 10
					}
				}
				fmt.Println(v)
				r2 += v
			}
			part2 += r2

			s = 0
		case '*':
			r1 := 1
			for j := range op_row {
				n := input[j][i : i+s]
				v, _ := strconv.Atoi(strings.TrimSpace(n))
				r1 *= v
			}
			part1 += r1

			r2 := 1
			for k := range s {
				v := 0
				for j := range op_row {
					c := input[j][i+k]
					if c != ' ' {
						v += int(c) - int('0')
						v *= 10
					}
				}
				fmt.Println(v)
				r2 *= v
			}
			part2 += r2

			s = 0
		}
	}

	fmt.Println("Day 06 Part 01: ", part1)
	fmt.Println("Day 06 Part 02: ", part2)
}
