package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func main() {

	_ := parse_input()

	part1 := 0
	part2 := 0

	fmt.Println("Day 12 Part 01: ", part1)
	fmt.Println("Day 12 Part 02: ", part2)
}

func parse_input() map[string][]string {
	input_file, _ := os.Open("input.txt")
	defer input_file.Close()

	devices := make(map[string][]string)
	scanner := bufio.NewScanner(input_file)
	for scanner.Scan() {
		line := scanner.Text()
		p := strings.Split(line, ":")
		k := strings.Trim(p[0], " ")
		vs := []string{}
		for v := range strings.SplitSeq(p[1], " ") {
			v = strings.Trim(v, " ")
			if len(v) == 0 {
				continue
			}
			vs = append(vs, v)
		}
		devices[k] = vs
	}
	return devices
}
