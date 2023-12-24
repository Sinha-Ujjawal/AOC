package main

import (
	"bufio"
	"container/heap"
	"flag"
	"fmt"
	"math"
	"os"
	"strconv"
)

type Pair[F any, S any] struct {
	First  F
	Second S
}

// Item represents an item in the heap.
type Item[T any, P int | float32] struct {
	Value    T
	Priority P
	Index    int
}

// PriorityQueue is a priority queue based on a binary heap.
type PriorityQueue[T any, P int | float32] []*Item[T, P]

// Len returns the length of the priority queue.
func (pq PriorityQueue[T, P]) Len() int { return len(pq) }

// Less compares two items based on their priority.
func (pq PriorityQueue[T, P]) Less(i, j int) bool {
	return pq[i].Priority < pq[j].Priority
}

// Swap swaps two items in the priority queue.
func (pq PriorityQueue[T, P]) Swap(i, j int) {
	pq[i], pq[j] = pq[j], pq[i]
	pq[i].Index = i
	pq[j].Index = j
}

// Push adds an item to the priority queue.
func (pq *PriorityQueue[T, P]) Push(x interface{}) {
	item := x.(*Item[T, P])
	item.Index = len(*pq)
	*pq = append(*pq, item)
}

// Pop removes and returns the minimum element from the priority queue.
func (pq *PriorityQueue[T, P]) Pop() interface{} {
	n := len(*pq)
	item := (*pq)[n-1]
	item.Index = -1 // for safety
	*pq = (*pq)[0 : n-1]
	return item
}

// Update modifies the priority and value of an item in the priority queue.
func (pq *PriorityQueue[T, P]) Update(item *Item[T, P], value T, priority P) {
	item.Value = value
	item.Priority = priority
	heap.Fix(pq, item.Index)
}

func Dijkstra[N comparable, W int | float32](neighborFn func(N) []Pair[N, W], start N, isEnd func(N) bool) map[N]Pair[N, W] {
	pq := PriorityQueue[N, W]{}
	heap.Push(&pq, &Item[N, W]{Value: start, Priority: 0})
	path := map[N]Pair[N, W]{}
	visited := map[N]bool{}
	for len(pq) > 0 {
		uItem := heap.Pop(&pq).(*Item[N, W])
		visited[uItem.Value] = true
		if isEnd(uItem.Value) {
			continue
		}
		du := uItem.Priority
		for _, v := range neighborFn(uItem.Value) {
			_, ok := visited[v.First]
			if ok {
				continue
			}
			dv, ok := path[v.First]
			dv_ := du + v.Second
			if !ok || dv_ < dv.Second {
				heap.Push(&pq, &Item[N, W]{Value: v.First, Priority: dv_})
				path[v.First] = Pair[N, W]{First: uItem.Value, Second: dv_}
			}
		}
	}
	return path
}

type direction = uint8

const (
	left direction = iota
	right
	up
	down
)

type position struct {
	row int
	col int
}

func (p position) forward(facing direction) position {
	switch facing {
	case left:
		return position{row: p.row, col: p.col - 1}
	case right:
		return position{row: p.row, col: p.col + 1}
	case up:
		return position{row: p.row - 1, col: p.col}
	case down:
		return position{row: p.row + 1, col: p.col}
	default:
		return p
	}
}

func (p position) turnLeft(facing direction) Pair[position, direction] {
	switch facing {
	case left:
		return Pair[position, direction]{position{row: p.row + 1, col: p.col}, down}
	case right:
		return Pair[position, direction]{position{row: p.row - 1, col: p.col}, up}
	case up:
		return Pair[position, direction]{position{row: p.row, col: p.col - 1}, left}
	case down:
		return Pair[position, direction]{position{row: p.row, col: p.col + 1}, right}
	default:
		return Pair[position, direction]{p, facing}
	}
}

func (p position) turnRight(facing direction) Pair[position, direction] {
	switch facing {
	case left:
		return Pair[position, direction]{position{row: p.row - 1, col: p.col}, up}
	case right:
		return Pair[position, direction]{position{row: p.row + 1, col: p.col}, down}
	case up:
		return Pair[position, direction]{position{row: p.row, col: p.col + 1}, right}
	case down:
		return Pair[position, direction]{position{row: p.row, col: p.col - 1}, left}
	default:
		return Pair[position, direction]{p, facing}
	}
}

func solve(grid [][]int, maxConseq int, minTurnThresh int) int {
	if len(grid) == 0 {
		return 0
	}
	nrow := len(grid)
	ncol := 0
	if nrow > 0 {
		ncol = len(grid[0])
	}
	start := position{0, 0}
	end := position{nrow - 1, ncol - 1}

	type Node struct {
		posn    position
		facing  direction
		nconseq int
	}

	dist := func(pair Pair[position, direction]) int {
		posn, facing := pair.First, pair.Second
		switch facing {
		case right:
			return ncol - 1 - posn.col
		case left:
			return posn.col
		case up:
			return posn.row
		case down:
			return nrow - 1 - posn.row
		default:
			return math.MaxInt
		}
	}

	inBound := func(posn position) bool {
		return !((posn.row < 0) ||
			(posn.row >= nrow) ||
			(posn.col < 0) ||
			(posn.col >= ncol))
	}

	neighborFn := func(node Node) []Pair[Node, int] {
		posn, facing, nconseq := node.posn, node.facing, node.nconseq
		isStart := posn == start
		heatLoss := 0
		if !isStart {
			heatLoss = grid[posn.row][posn.col]
		}
		ret := []Pair[Node, int]{}
		if isStart || (nconseq < maxConseq) {
			pair := Pair[Node, int]{
				Node{
					posn.forward(facing),
					facing,
					nconseq + 1,
				},
				heatLoss,
			}
			if inBound(pair.First.posn) {
				ret = append(ret, pair)
			}
		}
		if isStart || (nconseq >= minTurnThresh) {
			p := posn.turnLeft(facing)
			if isStart || (1+dist(p) >= minTurnThresh) {
				pair := Pair[Node, int]{
					Node{
						p.First,
						p.Second,
						1,
					},
					heatLoss,
				}
				if inBound(pair.First.posn) {
					ret = append(ret, pair)
				}
			}
			p = posn.turnRight(facing)
			if isStart || (1+dist(p) >= minTurnThresh) {
				pair := Pair[Node, int]{
					Node{
						p.First,
						p.Second,
						1,
					},
					heatLoss,
				}
				if inBound(pair.First.posn) {
					ret = append(ret, pair)
				}
			}
		}
		return ret
	}

	path := Dijkstra(
		neighborFn,
		Node{start, right, 0},
		func(node Node) bool { return node.posn == end },
	)
	ret := math.MaxInt
	for node := range path {
		if node.posn == end {
			ret = min(ret, path[node].Second)
		}
	}
	return ret + grid[end.row][end.col]
}

func solvePart1(grid [][]int) int {
	return solve(grid, 3, 0)
}

func solvePart2(grid [][]int) int {
	return solve(grid, 10, 4)
}

func main() {
	fp := flag.String("filepath", "", "File path of the input")
	flag.Parse()
	file, err := os.Open(*fp)
	if err != nil {
		panic(err)
	}
	scanner := bufio.NewScanner(file)
	grid := [][]int{}
	for scanner.Scan() {
		line := scanner.Text()
		row := []int{}
		for _, char := range line {
			d, err := strconv.Atoi(string(char))
			if err != nil {
				panic(err)
			}
			row = append(row, d)
		}
		grid = append(grid, row)
	}
	fmt.Printf("Part 1: %d\n", solvePart1(grid))
	fmt.Printf("Part 2: %d\n", solvePart2(grid))
}
