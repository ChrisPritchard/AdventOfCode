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

	read_val_1 := func(j, i, s int) int {
		n := input[j][i : i+s]
		v, _ := strconv.Atoi(strings.TrimSpace(n))
		return v
	}

	read_val_2 := func(i, k int) int {
		v := 0
		for j := range op_row {
			c := input[j][i+k]
			if c != ' ' {
				if v != 0 {
					v *= 10
				}
				v += int(c) - int('0')
			}
		}
		return v
	}

	s := 0
	for i := len(input[op_row]) - 1; i >= 0; i-- {
		c := input[op_row][i]
		if c == ' ' {
			s++
			continue
		}

		d := 0
		op := func(i *int, j int) {
			*i += j
		}
		if c == '*' {
			d = 1
			op = func(i *int, j int) {
				*i *= j
			}
		}

		r1 := d
		for j := range op_row {
			op(&r1, read_val_1(j, i, s))
		}
		part1 += r1

		r2 := d
		for k := range s {
			op(&r2, read_val_2(i, k))
		}
		part2 += r2

		s = 0
	}

	fmt.Println("Day 06 Part 01: ", part1)
	fmt.Println("Day 06 Part 02: ", part2)
}
