package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type fresh_range struct {
	start, end int
}

func main() {
	input_file, _ := os.Open("input.txt")
	defer input_file.Close()

	scanner := bufio.NewScanner(input_file)

	ranges := make([]fresh_range, 0)
	in_ranges := true

	part1 := 0

	for scanner.Scan() {
		line := scanner.Text()
		if in_ranges {
			if line == "" {
				in_ranges = false
				continue
			}
			p := strings.Split(line, "-")
			s, _ := strconv.Atoi(p[0])
			e, _ := strconv.Atoi(p[1])
			ranges = append(ranges, fresh_range{start: s, end: e})
		} else {
			i, _ := strconv.Atoi(line)
			valid := false
			for _, r := range ranges {
				if i >= r.start && i <= r.end {
					valid = true
					break
				}
			}
			if valid {
				part1++
			}
		}
	}

	for i, o := range ranges {
		c := o
		for j, r := range ranges {
			if i == j {
				continue
			}
			if c.start >= r.start && c.end <= r.end {
				c.start = 0
				c.end = 0
				break
			}
			if c.start >= r.start && c.start <= r.end {
				c.start = r.end + 1
			}
			if c.end >= r.start && c.end <= r.end {
				c.end = r.start - 1
			}
		}
		ranges[i] = c
	}

	part2 := 0

	for _, nr := range ranges {
		part2 += (nr.end - nr.start) + 1
	}

	fmt.Println("Day 05 Part 01: ", part1)
	fmt.Println("Day 05 Part 02: ", part2)
}
