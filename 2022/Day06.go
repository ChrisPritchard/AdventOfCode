package main

import (
	"fmt"
	"io/ioutil"
)

func main() {
	fmt.Printf("Day 06 Part 1: %d\n", findPattern(4))
	fmt.Printf("Day 06 Part 2: %d\n", findPattern(14))
}

func findPattern(n int) int {
	line, _ := ioutil.ReadFile("./inputs/day06.txt")
	for i := n; i < len(line); i++ {
		check := make(map[byte]interface{})
		valid := true
		for j := i - n; j < i; j++ {
			if _, exists := check[line[j]]; exists {
				valid = false
				break
			}
			check[line[j]] = true
		}
		if valid {
			return i
		}
	}
	return -1
}
