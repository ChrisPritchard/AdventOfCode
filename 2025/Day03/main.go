package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
)

func main() {
	input_file, _ := os.Open("input.txt")
	defer input_file.Close()

	scanner := bufio.NewScanner(input_file)
	part1 := 0
	part2 := 0

	for scanner.Scan() {
		line := scanner.Text()
		nums := make([]int, 0)
		for _, c := range line {
			nums = append(nums, int(c)-int('0'))
		}
		len := len(nums)
		max := 0

		for i := 0; i < len-1; i++ {
			for j := i + 1; j < len; j++ {
				n := nums[i]*10 + nums[j]
				if n > max {
					max = n
				}
			}
		}
		part1 += max

		max = 0

		find_highest_n := func(i, j int) int {
			c := i
			for k := i; k <= j; k++ {
				if nums[k] > nums[c] {
					c = k
				}
			}
			return c
		}

		var best_num func(i, j int) int
		best_num = func(i, j int) int {
			b := find_highest_n(i, len-j)
			if j == 1 {
				return nums[b]
			}
			return nums[b]*int(math.Pow10(j-1)) + best_num(b+1, j-1)
		}

		b := best_num(0, 12)
		part2 += b
	}

	fmt.Println("Day 03 Part 01: ", part1)
	fmt.Println("Day 03 Part 02: ", part2)
}
