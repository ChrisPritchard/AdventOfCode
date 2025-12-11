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

	type edge struct {
		a, b point
		d    bool // true = horizontal, false = vertical
	}

	edges := make([]edge, 0)
	for i, a := range input {
		b := point{}
		if i == 0 {
			b = input[len(input)-1]
		} else {
			b = input[i-1]
		}
		d := a.x == b.x
		edges = append(edges, edge{a, b, d})
	}

	verts := 

	part2 := 0
	for i, a := range input {
		for j, b := range input {
			if i == j {
				continue
			}
			size := ((b.x - a.x) + 1) * ((b.y - a.y) + 1)
			if size > part2 {
				tl := point{x: min(a.x, b.x), y: min(a.y, b.y)}
				br := point{x: max(a.x, b.x), y: max(a.y, b.y)}
				valid := true
				for x := tl.x; x <= br.x && valid; x++ {
					for y := tl.y; y <= br.y && valid; y++ {
						if _, exists := full_grid[point{x, y}]; !exists {
							valid = false
						}
					}
				}
				if valid {
					part2 = size
				}
			}
		}
	}

	fmt.Println("Day 09 Part 01: ", part1)
	fmt.Println("Day 09 Part 02: ", part2)
}
