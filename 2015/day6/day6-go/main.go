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

type instructionType = uint

const (
	TurnOn instructionType = iota
	TurnOff
	Toggle
)

type coord struct {
	x int
	y int
}

type box struct {
	topLeft     coord
	bottomRight coord
}

type instruction struct {
	instructionType instructionType
	box             box
}

func parseInstructions(lines []string) ([]instruction, error) {
	instructions := []instruction{}
	for lineNo, line := range lines {
		err := errors.New(fmt.Sprintf("Could not parse line: %d, \"%s\"\n", lineNo, line))
		instructionType, rest, couldParse := parseInstructionType(&line)
		if !couldParse {
			return nil, err
		}
		box, couldParse := parseBox(rest)
		if !couldParse {
			return nil, err
		}
		instruction := instruction{instructionType, *box}
		instructions = append(instructions, instruction)
	}
	return instructions, nil
}

func parseInstructionType(line *string) (instructionType, *string, bool) {
	var rest string
	var found bool

	rest, found = strings.CutPrefix(*line, "turn on ")
	if found {
		return TurnOn, &rest, true
	}

	rest, found = strings.CutPrefix(*line, "turn off ")
	if found {
		return TurnOff, &rest, true
	}

	rest, found = strings.CutPrefix(*line, "toggle ")
	if found {
		return Toggle, &rest, true
	}

	return 0, nil, false
}

func parseBox(line *string) (*box, bool) {
	words := strings.Split(*line, " through ")
	if len(words) != 2 {
		return nil, false
	}
	topLeft, couldParse := parseCoord(&words[0])
	if !couldParse {
		return nil, false
	}
	bottomRight, couldParse := parseCoord(&words[1])
	if !couldParse {
		return nil, false
	}
	box := box{topLeft: *topLeft, bottomRight: *bottomRight}
	return &box, true
}

func parseCoord(coordAsString *string) (*coord, bool) {
	words := strings.Split(*coordAsString, ",")
	if len(words) != 2 {
		return nil, false
	}
	x, err := strconv.Atoi(words[0])
	if err != nil {
		return nil, false
	}
	y, err := strconv.Atoi(words[1])
	if err != nil {
		return nil, false
	}
	coord := coord{x: x, y: y}
	return &coord, true
}

type brightnessUpdater = func(int) int

func solve(
	instructions []instruction,
	turnOnBrightnessUpdater brightnessUpdater,
	toggleBrightnessUpdater brightnessUpdater,
	turnOffBrightnessUpdater brightnessUpdater,
) []int {
	grid := make([]int, 1000*1000)
	for _, instruction := range instructions {
		var brightnessUpdater brightnessUpdater
		switch instruction.instructionType {
		case TurnOn:
			brightnessUpdater = turnOnBrightnessUpdater
		case Toggle:
			brightnessUpdater = toggleBrightnessUpdater
		case TurnOff:
			brightnessUpdater = turnOffBrightnessUpdater
		}
		topLeft := instruction.box.topLeft
		bottomRight := instruction.box.bottomRight
		for x := topLeft.x; x <= bottomRight.x; x += 1 {
			for y := topLeft.y; y <= bottomRight.y; y += 1 {
				idx := x*1000 + y
				grid[idx] = brightnessUpdater(grid[idx])
			}
		}
	}
	return grid
}

func solvePart1(instructions []instruction) int {
	grid := solve(
		instructions,
		func(b int) int { return 1 },
		func(b int) int {
			if b > 0 {
				return 0
			} else {
				return 1
			}
		},
		func(b int) int { return 0 },
	)
	var res int = 0
	for _, val := range grid {
		if val > 0 {
			res += 1
		}
	}
	return res
}

func solvePart2(instructions []instruction) int {
	grid := solve(
		instructions,
		func(b int) int { return b + 1 },
		func(b int) int { return b + 2 },
		func(b int) int {
			if b == 0 {
				return 0
			}
			return b - 1
		},
	)
	var res int = 0
	for _, val := range grid {
		res += val
	}
	return res
}

func main() {
	lines, err := readInput()
	if err != nil {
		panic(err)
	}
	instructions, err := parseInstructions(lines)
	if err != nil {
		panic(err)
	}
	fmt.Printf("Part 1: %d\n", solvePart1(instructions))
	fmt.Printf("Part 2: %d\n", solvePart2(instructions))
}
