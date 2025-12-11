package main

import (
	"bufio"
	"fmt"
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

	fmt.Println(machines)

	part1 := 0
	part2 := 0

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
			for j := range len(indices) {
				button[j], _ = strconv.Atoi(indices[j])
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
