package main

import (
	"bufio"
	"fmt"
	"os"
	"slices"
	"strconv"
	"strings"

	"github.com/draffensperger/golp"
)

type machine struct {
	lights  []bool
	buttons [][]int
	volts   []int
}

func main() {
	machines := parse_input()

	part1 := 0
	part2 := 0

	for _, m := range machines {
		part1 += find_light_button_combo_length(m.buttons, m.lights)
		part2 += find_voltage_button_combo_length(m.buttons, m.volts)
	}

	fmt.Println("Day 10 Part 01: ", part1)
	fmt.Println("Day 10 Part 02: ", part2)
}

func parse_input() []machine {
	machines := make([]machine, 0)

	input_file, _ := os.Open("input.txt")
	defer input_file.Close()
	scanner := bufio.NewScanner(input_file)

	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Split(line, " ")

		lights := make([]bool, len(parts[0])-2)
		for i := range len(lights) {
			lights[i] = parts[0][i+1] == '#'
		}

		buttons := make([][]int, len(parts)-2)
		for i := range len(buttons) {
			text := parts[i+1]
			indices := strings.Split(text[1:len(text)-1], ",")
			button := make([]int, len(indices))
			for j, b := range indices {
				n, _ := strconv.Atoi(b)
				button[j] = n
			}
			buttons[i] = button
		}

		last := parts[len(parts)-1]
		values := strings.Split(last[1:len(last)-1], ",")
		volts := make([]int, len(values))
		for i := range len(volts) {
			volts[i], _ = strconv.Atoi(values[i])
		}

		machines = append(machines, machine{lights, buttons, volts})
	}

	return machines
}

// simple bfs for part 1
func find_light_button_combo_length(buttons [][]int, lights []bool) int {

	// optimise by converting int arrays to bit masks

	light_mask := 0
	for i, b := range lights {
		if b {
			light_mask |= 1 << i
		}
	}

	button_masks := make([]int, len(buttons))
	for i, b := range buttons {
		mask := 0
		for _, n := range b {
			mask |= 1 << n
		}
		button_masks[i] = mask
	}

	// bfs starts here

	queue := []int{0}
	path := make(map[int]int) // curr -> prev
	visited := make(map[int]struct{})

	for len(queue) > 0 {
		current := queue[0]
		queue = queue[1:]

		if current == light_mask {
			// reconstruct steps
			count := 0
			for current != 0 {
				current = path[current]
				count++
			}

			return count
		}

		for _, b := range button_masks {
			candidate := current ^ b
			if _, e := visited[candidate]; e {
				continue
			}
			visited[candidate] = struct{}{}
			path[candidate] = current
			queue = append(queue, candidate)
		}
	}

	panic("no solution found")
}

// using integer linear programming via draffensperger/golp && LPSolve
func find_voltage_button_combo_length(buttons [][]int, volts []int) int {

	lp := golp.NewLP(0, len(buttons))
	for i, v := range volts {
		constraints := make([]golp.Entry, 0)
		for j, b := range buttons {
			if slices.Contains(b, i) {
				constraints = append(constraints, golp.Entry{Col: j, Val: 1.0})
			}
		}
		lp.AddConstraintSparse(constraints, golp.EQ, float64(v))
	}

	for i := range buttons {
		lp.SetInt(i, true)
	}

	obj := make([]float64, 0)
	for range buttons {
		obj = append(obj, 1.0)
	}

	lp.SetObjFn(obj)
	lp.Solve()

	return int(lp.Objective())
}
