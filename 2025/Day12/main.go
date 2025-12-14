package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {

	presents, areas := parse_input()

	part1 := presents
	part2 := areas

	fmt.Println("Day 12 Part 01: ", part1)
	fmt.Println("Day 12 Part 02: ", part2)
}

type area struct {
	width, height int
	required      [6]int
}

func parse_input() ([6][3][3]bool, []area) {
	input_file, _ := os.Open("input.txt")
	defer input_file.Close()

	scanner := bufio.NewScanner(input_file)
	lines := []string{}

	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	shapes := [6][3][3]bool{}
	for i := range 6 {
		cols := [3][3]bool{}
		for j := range 3 {
			row := [3]bool{}
			if lines[i*5+(j+1)][0] == '#' {
				row[0] = true
			}
			if lines[i*5+(j+1)][1] == '#' {
				row[1] = true
			}
			if lines[i*5+(j+1)][2] == '#' {
				row[2] = true
			}
			cols[j] = row
		}
		shapes[i] = cols
	}

	atoi := func(s string) int {
		n, _ := strconv.Atoi(s)
		return n
	}

	area_lines := lines[30:]
	areas := []area{}

	for _, line := range area_lines {
		parts := strings.Split(line, " ")
		size := strings.Split(strings.Trim(parts[0], ":"), "x")
		counts := [6]int{}
		for i := range counts {
			counts[i] = atoi(parts[i+1])
		}
		areas = append(areas, area{width: atoi(size[0]), height: atoi(size[1]), required: counts})
	}

	return shapes, areas
}
