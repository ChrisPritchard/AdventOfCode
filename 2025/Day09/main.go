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

type edge struct {
	a, b point
}

func (e edge) top_left() point {
	if e.a.x <= e.b.x && e.a.y <= e.b.y {
		return e.a
	}
	return e.b
}

func (e edge) bottom_right() point {
	if e.top_left() == e.a {
		return e.b
	}
	return e.a
}

func (e edge) vertical() bool {
	return e.a.x == e.b.x
}

func main() {

	all_points := parse_input()

	part1 := find_largest_rect(all_points)

	edges := get_edges(all_points)

	part2 := find_largest_interior_rect(all_points, edges)

	fmt.Println("Day 09 Part 01: ", part1)
	fmt.Println("Day 09 Part 02: ", part2)
}

func parse_input() []point {
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
	return input
}

func find_largest_rect(all_points []point) int {
	max_size := 0
	for i, a := range all_points {
		for j, b := range all_points {
			if i == j {
				continue
			}
			size := ((b.x - a.x) + 1) * ((b.y - a.y) + 1)
			if size > max_size {
				max_size = size
			}
		}
	}
	return max_size
}

func get_edges(all_points []point) []edge {
	edges := make([]edge, 0)
	for i, a := range all_points {
		var b point
		if i == 0 {
			b = all_points[len(all_points)-1]
		} else {
			b = all_points[i-1]
		}
		edges = append(edges, edge{a, b})
	}
	return edges
}

func edge_intercects(e edge, tl point, br point) bool {
	a := e.top_left()
	b := e.bottom_right()
	if e.vertical() {
		if e.a.x <= tl.x || e.a.x >= br.x {
			return false
		} else if b.y <= tl.y || a.y >= br.y {
			return false
		}
		return true
	} else {
		if e.a.y <= tl.y || e.a.y >= br.y {
			return false
		} else if b.x <= tl.x || a.x >= br.x {
			return false
		}
		return true
	}
}

func find_largest_interior_rect(all_points []point, edges []edge) int {
	max_size := 0
	for i, a := range all_points {
		for j, b := range all_points {
			if i == j {
				continue
			}
			size := ((b.x - a.x) + 1) * ((b.y - a.y) + 1)
			if size > max_size {
				tl := point{x: min(a.x, b.x), y: min(a.y, b.y)}
				br := point{x: max(a.x, b.x), y: max(a.y, b.y)}
				valid := true
				for _, e := range edges {
					if edge_intercects(e, tl, br) {
						valid = false
						break
					}
				}
				if valid {
					max_size = size
				}
			}
		}
	}
	return max_size
}
