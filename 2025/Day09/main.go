package main

import (
	"bufio"
	"cmp"
	"fmt"
	"os"
	"slices"
	"strconv"
	"strings"
)

type point struct {
	x, y int
}

type edge struct {
	a, b point
}

type rect struct {
	size int
	a, b point
}

func main() {

	all_points := parse_input()
	all_rects := all_possible_rects(all_points)

	part1 := all_rects[0].size

	// for part 2 i needed to work with https://github.com/mgtezak/Advent_of_Code/blob/master/2025/09/p2.py

	edges := get_edges(all_points)
	part2 := first_uncrossed_rect(edges, all_rects)

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

func abs(i int) int {
	if i < 0 {
		i *= -1
	}
	return i
}

func get_size(a, b point) int {
	return (abs(b.x-a.x) + 1) * (abs(b.y-a.y) + 1)
}

func all_possible_rects(all_points []point) []rect {
	rects := []rect{}
	for i, a := range all_points {
		for j, b := range all_points[i+1:] {
			if i == j {
				continue
			}
			size := get_size(a, b)
			new_rect := rect{size, a, b}
			if a.x >= b.x && a.y >= b.y {
				new_rect.a = b
				new_rect.b = a
			}
			rects = append(rects, new_rect)
		}
	}
	slices.SortFunc(rects, func(a, b rect) int {
		return -1 * cmp.Compare(a.size, b.size)
	})
	return rects
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
		new_edge := edge{a, b}
		if a.x >= b.x && a.y >= b.y {
			new_edge = edge{b, a}
		}
		edges = append(edges, new_edge)
	}
	slices.SortFunc(edges, func(a, b edge) int {
		return -1 * cmp.Compare(get_size(a.a, a.b), get_size(b.a, b.b))
	})
	return edges
}

func first_uncrossed_rect(edges []edge, rects []rect) int {
	for _, rect := range rects {
		valid := true
		x1 := rect.a.x
		x2 := rect.b.x
		y1 := rect.a.y
		y2 := rect.b.y
		for _, edge := range edges {
			x3 := edge.a.x
			x4 := edge.b.x
			y3 := edge.a.y
			y4 := edge.b.y

			if x4 > x1 && x3 < x2 && y4 > y1 && y3 < y2 {
				valid = false
				break
			}
		}
		if valid {
			return rect.size
		}
	}
	return -1
}
