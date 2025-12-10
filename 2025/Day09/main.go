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

	find_tl := func(a, b point) point {
		x := min(a.x, b.x)
		y := min(a.y, b.y)
		return point{x, y}
	}

	find_br := func(a, b point) point {
		x := max(a.x, b.x)
		y := max(a.y, b.y)
		return point{x, y}
	}

	edge_within := func(e edge, tl point, br point) bool {
		a_in := (e.a.x > tl.x && e.a.x < br.x && e.a.y > tl.y && e.a.y < br.y)
		b_in := (e.b.x > tl.x && e.b.x < br.x && e.b.y > tl.y && e.b.y < br.y)
		return a_in || b_in
	}

	edge_intersects := func(e edge, tl point, br point) bool {
		if e.d && e.a.y > tl.y && e.a.y < br.y {
			if e.a.x < tl.x && e.b.x > br.x {
				return false
			} else if e.b.x < tl.x && e.a.x > br.x {
				return false
			}
		} else if !e.d && e.a.x > tl.x && e.a.x < br.x {
			if e.a.y < tl.y && e.b.y > br.y {
				return false
			} else if e.b.y < tl.y && e.a.y > br.y {
				return false
			}
		}
		return false
	}

	part2 := 0
	for i, a := range input {
		for j, b := range input {
			if i == j {
				continue
			}
			size := ((b.x - a.x) + 1) * ((b.y - a.y) + 1)
			if size > part2 {
				tl := find_tl(a, b)
				br := find_br(a, b)
				valid := true
				for _, e := range edges {
					if edge_within(e, tl, br) || edge_intersects(e, tl, br) {
						valid = false
						break
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
