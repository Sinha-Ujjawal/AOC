package main

import (
	"bufio"
	"errors"
	"flag"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func readFilePath() (*string, error) {
	filePath := flag.String("filepath", "", "Provide input filepath for the question")
	flag.Parse()
	if *filePath != "" {
		return filePath, nil
	}
	reader := bufio.NewReader(os.Stdin)
	fmt.Print("Enter file path: ")
	text, err := reader.ReadString('\n')
	if err != nil {
		return nil, err
	}
	*filePath = strings.TrimSpace(text)
	return filePath, nil
}

func readInput() ([]string, error) {
	filePath, err := readFilePath()
	if err != nil {
		return nil, err
	}
	f, err := os.Open(*filePath)
	if err != nil {
		return nil, err
	}
	scanner := bufio.NewScanner(f)
	lines := []string{}
	for scanner.Scan() {
		var line string = scanner.Text()
		lines = append(lines, line)
	}
	return lines, nil
}

type springStatus = rune

const (
	Unknown     springStatus = '?'
	Damaged                  = '#'
	Operational              = '.'
)

type springs = []springStatus

type springAndDesc struct {
	springs springs
	desc    []uint
}

func (s springAndDesc) Replicate(n uint) springAndDesc {
	if n == 0 {
		return s
	}
	springs := springs{}
	desc := []uint{}
	for i := uint(1); i <= n; i++ {
		springs = append(springs, s.springs...)
		if i < n {
			springs = append(springs, Unknown)
		}
		desc = append(desc, s.desc...)
	}
	return springAndDesc{springs, desc}
}

func springAndDescFromString(s string) (*springAndDesc, error) {
	parts := strings.Split(s, " ")
	if len(parts) != 2 {
		return nil, errors.New(fmt.Sprintf("Cannot split the string into two parts, got: %s", s))
	}
	springs := springs{}
	for _, chr := range parts[0] {
		switch chr {
		case Unknown:
			springs = append(springs, chr)
		case Damaged:
			springs = append(springs, chr)
		case Operational:
			springs = append(springs, chr)
		default:
			return nil, errors.New(fmt.Sprintf("Cannot parse the left part of the string: (%s) as spring status", s))
		}
	}
	desc := []uint{}
	for _, digitStr := range strings.Split(parts[1], ",") {
		digit, err := strconv.Atoi(digitStr)
		if err != nil {
			return nil, errors.New(fmt.Sprintf("Cannot parse the right part of the string: (%s) as description", s))
		}
		desc = append(desc, uint(digit))
	}
	return &springAndDesc{springs, desc}, nil
}

func parseInput(lines []string) ([]springAndDesc, error) {
	ret := []springAndDesc{}
	for _, line := range lines {
		springAndDesc, err := springAndDescFromString(line)
		if err != nil {
			return nil, err
		}
		ret = append(ret, *springAndDesc)
	}
	return ret, nil
}

func numPossibilities(springAndDesc springAndDesc) uint {
	springs := springAndDesc.springs
	desc := springAndDesc.desc

	var satisfy func(uint, uint) bool
	satisfy = func(springI uint, descI uint) bool {
		d := desc[descI]
		cntDamaged := uint(0)
		for _, chr := range springs[springI:min(springI+d, uint(len(springs)))] {
			if chr == Damaged || chr == Unknown {
				cntDamaged += 1
			} else {
				break
			}
		}
		nextIndex := springI + d
		return (cntDamaged == d) && ((nextIndex >= uint(len(springs))) || (springs[nextIndex] != Damaged))
	}

	var dp func(uint, uint) uint
	memo := make(map[[2]uint]uint)
	dp = func(springI uint, descI uint) uint {
		key := [2]uint{springI, descI}
		memoVal, ok := memo[key]
		if ok {
			return memoVal
		}
		if descI >= uint(len(desc)) {
			if springI < uint(len(springs)) {
				for chr := range springs[springI:] {
					if chr == Damaged {
						return 0
					}
				}
			}
			return 1
		}
		if springI >= uint(len(springs)) {
			memo[key] = 0
			return 0
		}
		currentSpring := springs[springI]
		if !satisfy(springI, descI) {
			if currentSpring != Damaged {
				ret := dp(springI+1, descI)
				memo[key] = ret
				return ret
			} else {
				memo[key] = 0
				return 0
			}
		} else {
			d := desc[descI]
			ret := dp(springI+d+1, descI+1)
			if currentSpring != Damaged {
				ret += dp(springI+1, descI)
			}
			memo[key] = ret
			return ret
		}
	}

	return dp(0, 0)
}

func solvePart1(input []springAndDesc) uint {
	ret := uint(0)
	for _, springAndDesc := range input {
		ret += numPossibilities(springAndDesc)
	}
	return ret
}

func solvePart2(input []springAndDesc) uint {
	ret := uint(0)
	for _, springAndDesc := range input {
		ret += numPossibilities(springAndDesc.Replicate(5))
	}
	return ret
}

func main() {
	lines, err := readInput()
	if err != nil {
		panic(err)
	}
	input, err := parseInput(lines)
	if err != nil {
		panic(err)
	}
	fmt.Printf("Part 1: %d\n", solvePart1(input))
	fmt.Printf("Part 2: %d\n", solvePart2(input))
}
