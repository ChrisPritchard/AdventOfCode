package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
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

	for i, m := range machines {
		fmt.Printf("processing %d/%d\n", i, len(machines))
		press_count, exists := find_light_button_combo_length(m.buttons, m.lights)
		if !exists {
			fmt.Println("could not find light combo sequence for", m)
		} else {
			part1 += press_count
		}

		press_count, exists = find_voltage_button_combo_length(m.buttons, m.volts)
		if !exists {
			fmt.Println("could not find voltage combo sequence for", m)
		} else {
			part2 += press_count
		}
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
func find_light_button_combo_length(buttons [][]int, lights []bool) (int, bool) {

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

			return count, true
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

	return 0, false
}

func find_voltage_button_combo_length(buttons [][]int, volts []int) (int, bool) {

	type node struct {
		state    [10]int
		count    int
		index    int
		sequence []int
	}

	max_presses := func(button []int, target []int, current [10]int) int {
		max := math.MaxInt
		for _, n := range button {
			diff := target[n] - current[n]
			if diff < max {
				max = diff
			}
		}
		return max
	}

	apply_button := func(button []int, count int, current [10]int) [10]int {
		new := [10]int{}
		copy(new[:], current[:])
		for _, n := range button {
			new[n] = current[n] + count
		}
		return new
	}

	target_state := [10]int{}
	copy(target_state[:], volts)

	var dfs func(current node) int
	dfs = func(current node) int {
		if current.state == target_state {
			return current.count
		}

		if current.index == len(buttons) {
			return -1
		}

		button := buttons[current.index]
		max := max_presses(button, volts, current.state)

		for n := range max + 1 {
			new_state := apply_button(button, n, current.state)
			next := node{new_state, current.count + n, current.index + 1, append(current.sequence, n)}
			r := dfs(next)
			if r != -1 {
				return r
			}
		}

		return -1
	}

	start := node{[10]int{}, 0, 0, []int{}}
	r := dfs(start)
	fmt.Println(r)

	return r, r != -1
}
