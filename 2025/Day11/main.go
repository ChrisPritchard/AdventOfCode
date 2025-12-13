package main

import (
	"bufio"
	"fmt"
	"maps"
	"os"
	"strings"
)

func main() {

	devices := parse_input()

	part1 := all_paths(devices)
	part2 := valid_paths(devices)

	fmt.Println("Day 11 Part 01: ", part1)
	fmt.Println("Day 11 Part 02: ", part2)
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

func all_paths(devices map[string][]string) int {
	count := 0
	queue := []string{"you"}
	for len(queue) > 0 {
		n := queue[0]
		queue = queue[1:]

		if n == "out" {
			count++
			continue
		}

		queue = append(queue, devices[n]...)
	}
	return count
}

type path struct {
	visited map[string]struct{}
	head    string
}

func valid_paths(devices map[string][]string) int {
	count := 0
	queue := []path{{visited: map[string]struct{}{"svr": {}}, head: "svr"}}

	for len(queue) > 0 {
		n := queue[0]
		queue = queue[1:]

		if n.head == "out" {
			_, a := n.visited["dac"]
			_, b := n.visited["fft"]
			if a && b {
				count++
			}
			continue
		}

		for _, d := range devices[n.head] {
			if _, e := n.visited[d]; e {
				continue
			}
			nm := make(map[string]struct{})
			maps.Copy(nm, n.visited)
			nm[d] = struct{}{}
			new := path{visited: nm, head: d}
			queue = append(queue, new)
		}
	}
	return count
}
