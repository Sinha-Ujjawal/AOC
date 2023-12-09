package main

import (
	"bufio"
	"flag"
	"fmt"
	"os"
	"sort"
	"strings"
	"unicode"
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

func solvePart1(lines []string) uint {
	extractNumberFromLine := func(line string) uint {
		var firstDigitPosn = -1
		var lastDigitPosn = -1
		for posn, char := range line {
			if unicode.IsDigit(char) {
				if firstDigitPosn == -1 {
					firstDigitPosn = posn
				}
				lastDigitPosn = posn
			}
		}
		if firstDigitPosn == -1 {
			return 0
		}
		return uint(line[firstDigitPosn]-'0')*10 + uint(line[lastDigitPosn]-'0')
	}

	var ret = uint(0)
	for _, line := range lines {
		ret += extractNumberFromLine(line)
	}

	return ret
}

func solvePart2(lines []string) uint {
	type digitMap struct {
		s     string
		digit uint
	}

	mapping := []digitMap{
		{s: "0", digit: 0},
		{s: "1", digit: 1},
		{s: "2", digit: 2},
		{s: "3", digit: 3},
		{s: "4", digit: 4},
		{s: "5", digit: 5},
		{s: "6", digit: 6},
		{s: "7", digit: 7},
		{s: "8", digit: 8},
		{s: "9", digit: 9},
		{s: "eight", digit: 8},
		{s: "five", digit: 5},
		{s: "four", digit: 4},
		{s: "nine", digit: 9},
		{s: "one", digit: 1},
		{s: "seven", digit: 7},
		{s: "six", digit: 6},
		{s: "three", digit: 3},
		{s: "two", digit: 2},
		{s: "zero", digit: 0},
	}

	extractNumberFromLine := func(line string) uint {
		var firstDigit *uint = nil
		var lastDigit *uint = nil
		for posn := range line {
			suffix := line[posn:]
			idx := sort.Search(len(mapping), func(i int) bool { return mapping[i].s > suffix })
			if idx <= 0 || idx > len(mapping) {
				continue
			}
			match := mapping[idx-1]
			if strings.HasPrefix(suffix, match.s) {
				if firstDigit == nil {
					firstDigit = &match.digit
				}
				lastDigit = &match.digit
			}
		}
		if firstDigit == nil {
			return 0
		}
		return (*firstDigit * 10) + *lastDigit
	}

	var ret = uint(0)
	for _, line := range lines {
		ret += extractNumberFromLine(line)
	}

	return ret
}

func main() {
	lines, err := readInput()
	if err != nil {
		panic(err)
	}

	part1Ans := solvePart1(lines)
	fmt.Printf("Part 1: %d\n", part1Ans)

	part2Ans := solvePart2(lines)
	fmt.Printf("Part 2: %d\n", part2Ans)
}
