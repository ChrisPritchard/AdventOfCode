package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func main() {

	devices := parse_input()

	part1 := all_paths(devices)
	part2 := compress(devices)

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

type end_counts struct {
	normal, fft, dac, both int
}

func (e *end_counts) merge(o end_counts) {
	e.normal += o.normal
	e.dac += o.dac
	e.fft += o.fft
	e.both += o.both
}

type junction struct {
	counts end_counts
	name   string
}

func (j junction) is_name() bool {
	return len(j.name) != 0
}

// i wrote this in an attempt to speed up my part 2 search, and accidentally discovered it compresses down to nothing so i can just solve it this way
func compress(devices map[string][]string) int {

	// find all a -> b mappings (only a single result for a given key)
	equivs := make(map[string]string)
	for k, v := range devices {
		if k == "dac" || k == "fft" || k == "svr" {
			continue
		}
		if len(v) == 1 {
			equivs[k] = v[0]
		}
	}

	// combine multiple single chains into one, a -> b -> c becomes a -> c
	// repeat until no more such chains can be reduced
	for {
		compressable := false
		for a, b := range equivs {
			if c, e := equivs[b]; e {
				compressable = true
				equivs[a] = c
			}
		}
		if !compressable {
			break
		}
	}

	// convert device map to a new map with the chains replaced with their outcome
	compressed := make(map[string][]string)
	for k, v := range devices {
		if _, e := equivs[k]; e {
			continue
		}

		new_values := []string{}
		for _, o := range v {
			if r, e := equivs[o]; e {
				new_values = append(new_values, r)
			} else {
				new_values = append(new_values, o)
			}
		}

		compressed[k] = new_values
	}

	// convert compressed into a reduced map, with more info on junctions (used for next step)
	reduced := make(map[string][]junction)
	for k, v := range compressed {
		junctions := []junction{}
		for _, o := range v {
			junctions = append(junctions, junction{end_counts{}, o})
		}
		reduced[k] = junctions
	}

	for {
		can_reduce := false

		// find all junctions whose children / out links are all either 'out' or some count of out values
		replacements := make(map[string]junction)
		for k, v := range reduced {
			all_ends := true
			counts := end_counts{}
			for _, o := range v {
				if o.is_name() && o.name != "out" {
					all_ends = false
					break
				} else if o.is_name() {
					counts.normal++
				} else {
					counts.merge(o.counts)
				}
			}
			if all_ends {
				can_reduce = true
				if k == "dac" {
					counts.both += counts.fft
					counts.dac += counts.normal
					counts.fft = 0
					counts.normal = 0
				}
				if k == "fft" {
					counts.both += counts.dac
					counts.fft += counts.normal
					counts.dac = 0
					counts.normal = 0
				}
				replacements[k] = junction{counts, ""}
			}
		}

		if !can_reduce {
			panic("assumption that it can be reduced to nothing failed")
		} else {
			// create new reduced with values that have been compressed into a single junction inserted
			new_reduced := make(map[string][]junction)
			for k, v := range reduced {
				if _, e := replacements[k]; e {
					continue
				}
				junctions := []junction{}
				for _, o := range v {
					if !o.is_name() {
						junctions = append(junctions, o)
					} else if o.is_name() {
						if r, e := replacements[o.name]; e {
							junctions = append(junctions, r)
						} else {
							junctions = append(junctions, o)
						}
					}
				}
				new_reduced[k] = junctions
			}
			if len(new_reduced) == 1 {
				final_count := 0
				// we assume this is the svr node
				for _, v := range new_reduced["svr"] {
					final_count += v.counts.both
				}
				return final_count
			}
			reduced = new_reduced
		}
	}
}
