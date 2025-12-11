package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type machine struct {
	light_mask   int
	buttons      [][]int
	button_masks []int
	volts        []int
}

func main() {
	machines := parse_input()

	part1 := 0
	part2 := 0

	for _, m := range machines {
		path, exists := find_min_presses(m.button_masks, m.light_mask)
		if !exists {
			fmt.Println("could not find sequence for", m)
		} else {
			part1 += len(path)
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
		light_mask := 0
		for i, b := range lights {
			if b {
				light_mask |= 1 << i
			}
		}

		buttons := make([][]int, len(parts)-2)
		button_masks := make([]int, len(parts)-2)
		for i := range len(buttons) {
			text := parts[i+1]
			indices := strings.Split(text[1:len(text)-1], ",")
			button := make([]int, len(indices))
			button_mask := 0
			for j := range len(indices) {
				n, _ := strconv.Atoi(indices[j])
				button[j] = n
				button_mask |= 1 << n
			}
			buttons[i] = button
			button_masks[i] = button_mask
		}

		last := parts[len(parts)-1]
		values := strings.Split(last[1:len(last)-1], ",")
		volts := make([]int, len(values))
		for i := range len(volts) {
			volts[i], _ = strconv.Atoi(values[i])
		}

		machines = append(machines, machine{light_mask, buttons, button_masks, volts})
	}

	return machines
}

func find_min_presses(buttons []int, target int) ([]int, bool) {

	start := 0
	queue := []int{start}
	prev := make(map[int]int)   // state -> previous state
	action := make(map[int]int) // state -> button pressed to get here
	visited := make(map[int]bool)
	visited[start] = true

	for len(queue) > 0 {
		current := queue[0]
		queue = queue[1:]

		if current == target {
			path := []int{}
			state := current
			for state != start {
				btn := action[state]
				path = append([]int{btn}, path...)
				state = prev[state]
			}
			return path, true
		}

		for btn := range buttons {
			next := current ^ buttons[btn]
			if !visited[next] {
				visited[next] = true
				prev[next] = current
				action[next] = btn
				queue = append(queue, next)
			}
		}
	}

	return nil, false
}
