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

	junction_to_junction := make(map[junction]map[junction]float64)
	all_distances := make([]float64, 0)
	distance_to_junctions := make(map[float64][]junction)

	sq := func(a int) float64 {
		return math.Pow(float64(a), float64(2))
	}

	for _, j := range junctions {
		for _, k := range junctions {
			if j == k {
				continue
			}
			if d, exists := junction_to_junction[k]; exists {
				if _, exists := d[j]; exists {
					continue
				}
			} else if d, exists := junction_to_junction[j]; exists {
				if _, exists := d[k]; exists {
					continue
				}
			}

			distance := math.Sqrt(sq(k.x-j.x) + sq(k.y-j.y) + sq(k.z-j.z))
			all_distances = append(all_distances, distance)
			if d, exists := distance_to_junctions[distance]; exists {
				d = append(append(d, j), k)
			} else {
				distance_to_junctions[distance] = []junction{j, k}
			}

			if d, exists := junction_to_junction[j]; exists {
				d[k] = distance
			} else {
				junction_to_junction[j] = make(map[junction]float64)
				junction_to_junction[j][k] = distance
			}

			if d, exists := junction_to_junction[k]; exists {
				d[j] = distance
			} else {
				junction_to_junction[k] = make(map[junction]float64)
				junction_to_junction[k][j] = distance
			}
		}
	}

	fmt.Println(distance_to_junctions)

	part1 := 0
	part2 := 0

	fmt.Println("Day 08 Part 01: ", part1)
	fmt.Println("Day 08 Part 02: ", part2)
}
