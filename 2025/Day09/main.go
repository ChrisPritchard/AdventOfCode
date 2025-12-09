package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type point struct {
	x, y int
}

func main() {
	input_file, _ := os.Open("input.txt")
	defer input_file.Close()

	input := make([]point, 0)
	scanner := bufio.NewScanner(input_file)
	for scanner.Scan() {
		line := scanner.Text()
		p := strings.Split(line, ",")
		x, _ := strconv.Atoi(p[0])
		y, _ := strconv.Atoi(p[1])
		input = append(input, point{x, y})
	}

	part1 := 0
	for i, a := range input {
		for j, b := range input {
			if i == j {
				continue
			}
			size := ((b.x - a.x) + 1) * ((b.y - a.y) + 1)
			if size > part1 {
				part1 = size
			}
		}
	}

	part2 := 0

	fmt.Println("Day 09 Part 01: ", part1)
	fmt.Println("Day 09 Part 02: ", part2)
}
