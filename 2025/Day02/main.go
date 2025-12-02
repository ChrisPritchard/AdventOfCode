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

	part1 := 0
	part2 := 0

	for r := range strings.SplitSeq(line, ",") {
		ends := strings.Split(r, "-")
		start, _ := strconv.Atoi(ends[0])
		end, _ := strconv.Atoi(ends[1])

		for i := start; i <= end; i++ {
			cur := strconv.Itoa(i)
			l := len(cur)

			for k := 2; k <= l; k++ {

				valid := true
				if l%k != 0 {
					continue
				}
				jmp := l / k
				for j := 0; j < jmp && valid; j++ {
					c := cur[j]
					for m := 1; m < k && valid; m++ {
						if cur[m*jmp+j] != c {
							valid = false
						}
					}
				}

				if valid {
					if k == 2 {
						part1 += i
					}
					part2 += i
					break
				}
			}
		}
	}

	fmt.Println("Day 02 Part 01: ", part1)
	fmt.Println("Day 02 Part 02: ", part2)
}
