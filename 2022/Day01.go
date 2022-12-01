package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

var current, first, second, third = 0, 0, 0, 0

func main() {
	file, err := os.Open("./inputs/day01.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)

	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			shuffleVals()
		} else {
			val, err := strconv.Atoi(line)
			if err != nil {
				log.Fatal(err)
			}
			current += val
		}
	}
	shuffleVals()

	fmt.Printf("Day 01 Part 1: %d\n", first)
	fmt.Printf("Day 01 Part 2: %d\n", first+second+third)
}

func shuffleVals() {
	if first == 0 {
		first = current
	} else if second == 0 {
		second = current
	} else if third == 0 {
		third = current
	} else if current > first {
		third = second
		second = first
		first = current
	} else if current > second {
		third = second
		second = current
	} else if current > third {
		third = current
	}
	current = 0
}
