package main

import (
	"bufio"
	"cmp"
	"fmt"
	"maps"
	"math"
	"os"
	"slices"
	"strconv"
	"strings"
)

type junction struct {
	x, y, z int
}

type pair struct {
	a, b junction
}

func (o pair) rev() pair {
	return pair{a: o.b, b: o.a}
}

type dist_pair struct {
	d    float64
	pair pair
}

type circuit struct {
	id        int
	junctions map[junction]struct{}
}

func new_circuit(id int, a, b junction) circuit {
	return circuit{
		id: id,
		junctions: map[junction]struct{}{
			a: {},
			b: {},
		},
	}
}

func (o circuit) size() int {
	return len(o.junctions)
}

func (o circuit) contains(c junction) bool {
	_, exists := o.junctions[c]
	return exists
}

func (o circuit) add(c junction) {
	o.junctions[c] = struct{}{}
}

func (o circuit) merge_into(d circuit) {
	maps.Copy(d.junctions, o.junctions)
	for j := range o.junctions {
		delete(o.junctions, j)
	}
}

func sq(a int) float64 {
	return math.Pow(float64(a), float64(2))
}

func straight_line(a, b junction) float64 {
	return math.Sqrt(sq(b.x-a.x) + sq(b.y-a.y) + sq(b.z-a.z))
}

func find(a []circuit, j junction) (circuit, bool, int) {
	for i, o := range a {
		if o.contains(j) {
			return o, true, i
		}
	}
	var z circuit
	return z, false, 0
}

func main() {

	junctions := read_input()
	distances := find_unique_connections(junctions)

	circuits := make([]circuit, 0)

	conn_count := 10
	if len(junctions) > 100 {
		conn_count = 1000 // real input data
	}

	part1 := 0
	part2 := 0

	for i, candidate := range distances {
		circ_a, exists_a, index := find(circuits, candidate.pair.a)
		circ_b, exists_b, _ := find(circuits, candidate.pair.b)

		if exists_a && exists_b && circ_a.id == circ_b.id {
			// do nothing
		} else if exists_a && exists_b {
			circ_a.merge_into(circ_b)
			circuits = slices.Delete(circuits, index, index+1)
		} else if exists_a {
			circ_a.add(candidate.pair.b)
		} else if exists_b {
			circ_b.add(candidate.pair.a)
		} else {
			circuits = append(circuits, new_circuit(i, candidate.pair.a, candidate.pair.b))
		}

		if i == conn_count-1 {
			slices.SortFunc(circuits, func(a, b circuit) int {
				return -1 * cmp.Compare(a.size(), b.size())
			})
			part1 = circuits[0].size() * circuits[1].size() * circuits[2].size()
		}
		if len(circuits) == 1 && circuits[0].size() == len(junctions) {
			part2 = candidate.pair.a.x * candidate.pair.b.x
			break
		}
	}

	fmt.Println("Day 08 Part 01: ", part1)
	fmt.Println("Day 08 Part 02: ", part2)
}

func read_input() []junction {
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

	return junctions
}

func find_unique_connections(junctions []junction) []dist_pair {
	distances := make([]dist_pair, 0)

	pairs := make(map[pair]struct{})
	contains := func(m map[pair]struct{}, key pair) bool {
		_, exists := m[key]
		if exists {
			return true
		}
		_, exists = m[key.rev()]
		return exists
	}

	for i, a := range junctions {
		for j, b := range junctions {
			if i == j || contains(pairs, pair{a, b}) {
				continue
			}

			d := straight_line(a, b)
			pair := pair{a, b}
			pairs[pair] = struct{}{} // don't reexamine this pair
			distances = append(distances, dist_pair{d, pair})
		}
	}

	slices.SortFunc(distances, func(a, b dist_pair) int {
		return cmp.Compare(a.d, b.d)
	})

	return distances
}
