package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func main() {
	input_file, _ := os.Open("input.txt")
	defer input_file.Close()

	pos := 50
	pass1 := 0
	pass2 := 0

	scanner := bufio.NewScanner(input_file)
	for scanner.Scan() {
		line := scanner.Text()

		left := line[0] == 'L'
		dist, _ := strconv.Atoi(line[1:])

		for range dist {
			if left {
				pos--
			} else {
				pos++
			}

			if pos == 100 {
				pos = 0
			} else if pos == -1 {
				pos = 99
			}

			if pos == 0 {
				pass2++
			}
		}

		if pos == 0 {
			pass1++
		}
	}

	fmt.Println("Day 01 Part 01: ", pass1)
	fmt.Println("Day 01 Part 02: ", pass2)
}
