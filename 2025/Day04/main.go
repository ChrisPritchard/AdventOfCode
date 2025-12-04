package main

import (
	"bufio"
	"fmt"
	"os"
)

type point struct {
	X int
	Y int
}

func (p point) Neighbours() []point {
	return []point{
		point{p.X - 1, p.Y - 1},
		point{p.X, p.Y - 1},
		point{p.X + 1, p.Y - 1},
		point{p.X - 1, p.Y},
		point{p.X + 1, p.Y},
		point{p.X - 1, p.Y + 1},
		point{p.X, p.Y + 1},
		point{p.X + 1, p.Y + 1},
	}
}

func main() {
	input_file, _ := os.Open("input.txt")
	defer input_file.Close()

	scanner := bufio.NewScanner(input_file)

	roll_pos := make([]point, 0)
	roll_map := make(map[point]struct{}, 0)

	y := 0
	for scanner.Scan() {
		line := scanner.Text()
		for x, c := range line {
			if c == '@' {
				roll_pos = append(roll_pos, point{x, y})
				roll_map[point{x, y}] = struct{}{}
			}
		}

		y++
	}

	find_removable := func() []point {
		r := make([]point, 0)
		for _, p := range roll_pos {
			if _, exists := roll_map[p]; !exists {
				continue
			}
			c := 0
			for _, n := range p.Neighbours() {
				if _, exists := roll_map[n]; exists {
					c++
				}
			}
			if c < 4 {
				r = append(r, p)
			}
		}
		return r
	}

	part1 := len(find_removable())

	part2 := 0
	for {
		r := find_removable()
		if len(r) == 0 {
			break
		}

		part2 += len(r)

		for _, p := range r {
			delete(roll_map, p)
		}
	}

	fmt.Println("Day 04 Part 01: ", part1)
	fmt.Println("Day 04 Part 02: ", part2)
}
