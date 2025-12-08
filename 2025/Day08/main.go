package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

type junction struct {
	x, y, z int
}

func main() {
	input_file, _ := os.Open("input.txt")
	defer input_file.Close()

	junctions := make([]junction, 0)
	scanner := bufio.NewScanner(input_file)
	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Split(line, ",")
		x, _ := strconv.Atoi(parts[0])
		y, _ := strconv.Atoi(parts[1])
		z, _ := strconv.Atoi(parts[2])
		junctions = append(junctions, junction{x, y, z})
	}

	dists := make(map[junction]map[junction]float64)
	sq := func(a int) float64 {
		return math.Pow(float64(a), float64(2))
	}

	for _, j := range junctions {
		for _, k := range junctions {
			if j == k {
				continue
			}
			if d, exists := dists[k]; exists {
				if _, exists := d[j]; exists {
					continue
				}
			} else if d, exists := dists[j]; exists {
				if _, exists := d[k]; exists {
					continue
				}
			}
			distance := math.Sqrt(sq(k.x-j.x) + sq(k.y-j.y) + sq(k.z-j.z))
			if d, exists := dists[j]; exists {
				d[k] = distance
			} else {
				dists[j] = make(map[junction]float64)
				dists[j][k] = distance
			}

			if d, exists := dists[k]; exists {
				d[j] = distance
			} else {
				dists[k] = make(map[junction]float64)
				dists[k][j] = distance
			}
		}
	}

	fmt.Println(dists)

	part1 := 0
	part2 := 0

	fmt.Println("Day 08 Part 01: ", part1)
	fmt.Println("Day 08 Part 02: ", part2)
}
