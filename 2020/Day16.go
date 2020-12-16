package main

import (
	"io/ioutil"
	"log"
	"strconv"
	"strings"
)

var newline = "\r\n"

type rule struct {
	name           string
	n1, m1, n2, m2 int
}

func (r rule) valid(v int) bool {
	return (v >= r.n1 && v <= r.m1) || (v >= r.n2 && v <= r.m2)
}

type rulePossibility struct {
	name    string
	indices map[int]bool
}

func conv(s string) int {
	n, _ := strconv.Atoi(s)
	return n
}

func line(s string) []int {
	parts := strings.Split(s, ",")
	result := make([]int, 0)
	for _, v := range parts {
		result = append(result, conv(v))
	}
	return result
}

func main() {
	log.SetFlags(0)

	text, _ := ioutil.ReadFile("./inputs/day16.txt")
	parts := strings.Split(string(text), newline+newline)

	rules := make([]rule, 0)
	for _, v := range strings.Split(parts[0], newline) {
		s := strings.Split(v, ": ")
		n := strings.Split(s[1], " or ")
		a := strings.Split(n[0], "-")
		b := strings.Split(n[1], "-")
		rules = append(rules, rule{
			name: s[0],
			n1:   conv(a[0]),
			m1:   conv(a[1]),
			n2:   conv(b[0]),
			m2:   conv(b[1]),
		})
	}

	yours := line(strings.Split(parts[1], newline)[1])

	others := make([][]int, 0)
	for _, v := range strings.Split(parts[2], newline)[1:] {
		others = append(others, line(v))
	}

	log.Printf("part 1: %d%s", part1(rules, yours, others), newline)
	log.Printf("part 2: %d%s", part2(rules, yours, others), newline)
}

func part1(rules []rule, yours []int, others [][]int) int {
	sum := 0
	for _, line := range others {
		for _, v := range line {
			valid := false
			for _, r := range rules {
				if r.valid(v) {
					valid = true
					break
				}
			}
			if !valid {
				sum += v
			}
		}
	}
	return sum
}

func part2(rules []rule, yours []int, others [][]int) uint64 {
	toTest := make([][]int, 0)
	toTest = append(toTest, yours)

	for _, line := range others {
		allValid := true
		for _, v := range line {
			valid := false
			for _, r := range rules {
				if r.valid(v) {
					valid = true
					break
				}
			}
			if !valid {
				allValid = false
				break
			}
		}
		if allValid {
			toTest = append(toTest, line)
		}
	}

	rulePossibilities := make([]rulePossibility, 0)
	for _, r := range rules {
		indices := make(map[int]bool)
		for i := 0; i < len(yours); i++ {
			valid := true
			for _, line := range toTest {
				if !r.valid(line[i]) {
					valid = false
					break
				}
			}
			if valid {
				indices[i] = true
			}
		}
		rulePossibilities = append(rulePossibilities, rulePossibility{r.name, indices})
	}

	ruleIndex := make(map[int]string)
	for len(rulePossibilities) > 0 {
		index := -1
		for _, r := range rulePossibilities {
			if len(r.indices) == 1 {
				for i := range r.indices {
					index = i
					ruleIndex[index] = r.name
					break
				}
			}
		}
		newRules := make([]rulePossibility, 0)
		for _, r := range rulePossibilities {
			delete(r.indices, index)
			if len(r.indices) > 0 {
				newRules = append(newRules, r)
			}
		}
		rulePossibilities = newRules
	}

	var result uint64 = 1
	for i, v := range yours {
		if strings.HasPrefix(ruleIndex[i], "departure") {
			result = result * uint64(v)
		}
	}
	return result
}
