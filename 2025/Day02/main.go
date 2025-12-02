package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	input_file, _ := os.Open("input.txt")
	defer input_file.Close()

	scanner := bufio.NewScanner(input_file)
	scanner.Scan()
	line := scanner.Text()

	total := 0

	for r := range strings.SplitSeq(line, ",") {
		ends := strings.Split(r, "-")
		start, _ := strconv.Atoi(ends[0])
		end, _ := strconv.Atoi(ends[1])

		for i := start; i <= end; i++ {
			cur := strconv.Itoa(i)
			if len(cur)%2 != 0 {
				continue
			}
			valid := true
			for j := 0; j < len(cur)/2; j++ {
				if cur[j] != cur[len(cur)/2+j] {
					valid = false
					break
				}
			}
			if valid {
				fmt.Println(cur)
				total += i
			}
		}
	}

	fmt.Println("Day 02 Part 01: ", total)
	// fmt.Println("Day 01 Part 02: ", pass2)
}
