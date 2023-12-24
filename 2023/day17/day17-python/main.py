from typing import List, Set, Dict, Tuple, Callable, TypeVar, Iterable, Optional
from enum import IntEnum
from dataclasses import dataclass, replace
import argparse
from pathlib import Path
from heapq import heappush, heappop
from itertools import chain

N = TypeVar("N")
Wt = TypeVar("Wt", int, float)


def dijkstra(
    neighbor_fn: Callable[[N], Iterable[Tuple[Wt, N]]],
    start: N,
    is_end: Callable[[N], bool],
) -> Dict[N, Tuple[Optional[N], Wt]]:
    pq: List[Tuple[Wt, N]] = [(0, start)]
    path: Dict[N, Tuple[Optional[N], Wt]] = {start: (None, 0)}
    visited: Set[N] = {start}
    while pq:
        du, u = heappop(pq)
        visited.add(u)
        if is_end(u):
            continue
        for wt, v in neighbor_fn(u):
            if v not in visited:
                dv = path.get(v, None)
                dv_ = du + wt
                if dv is None or dv_ < dv[1]:
                    heappush(pq, (dv_, v))
                    path[v] = (u, dv_)
    return path


def mk_path(path: Dict[N, Tuple[Optional[N], Wt]], end: N) -> List[N]:
    ret: List[N] = []
    node: Optional[N] = end
    while node is not None and node in path:
        ret.append(node)
        node, _ = path[node]
    return ret[::-1]


class Direction(IntEnum):
    Left: int = 0
    Right: int = 1
    Up: int = 2
    Down: int = 3


@dataclass(frozen=True, eq=True, order=True)
class Position:
    __slots__ = ["row", "col"]
    row: int
    col: int


def forward(posn: Position, facing: Direction) -> Position:
    if facing == Direction.Left:
        return replace(posn, col=posn.col - 1)
    elif facing == Direction.Right:
        return replace(posn, col=posn.col + 1)
    elif facing == Direction.Up:
        return replace(posn, row=posn.row - 1)
    elif facing == Direction.Down:
        return replace(posn, row=posn.row + 1)
    else:
        raise ValueError(f"Unrecognized direction for {facing=}!")


def turn_left(posn: Position, facing: Direction) -> Tuple[Position, Direction]:
    if facing == Direction.Left:
        return replace(posn, row=posn.row + 1), Direction.Down
    elif facing == Direction.Right:
        return replace(posn, row=posn.row - 1), Direction.Up
    elif facing == Direction.Up:
        return replace(posn, col=posn.col - 1), Direction.Left
    elif facing == Direction.Down:
        return replace(posn, col=posn.col + 1), Direction.Right
    else:
        raise ValueError(f"Unrecognized direction for {facing=}!")


def turn_right(posn: Position, facing: Direction) -> Tuple[Position, Direction]:
    if facing == Direction.Left:
        return replace(posn, row=posn.row - 1), Direction.Up
    elif facing == Direction.Right:
        return replace(posn, row=posn.row + 1), Direction.Down
    elif facing == Direction.Up:
        return replace(posn, col=posn.col + 1), Direction.Right
    elif facing == Direction.Down:
        return replace(posn, col=posn.col - 1), Direction.Left
    else:
        raise ValueError(f"Unrecognized direction for {facing=}!")


def solve(
    grid: List[List[int]], *, max_conseq: int = 3, min_turn_thresh: int = 0
) -> int:
    if not grid:
        return 0
    nrow, ncol = len(grid), 0 if len(grid) == 0 else len(grid[0])
    start = Position(0, 0)
    end = Position(nrow - 1, ncol - 1)

    Node = Tuple[Position, Direction, int]

    def dist(posn: Position, facing: Direction) -> int:
        if facing == Direction.Right:
            return ncol - 1 - posn.col
        elif facing == Direction.Left:
            return posn.col
        elif facing == Direction.Up:
            return posn.row
        elif facing == Direction.Down:
            return nrow - 1 - posn.row
        else:
            raise ValueError(f"Unrecognized direction for {facing=}!")

    def neighbor_fn_helper(node: Node) -> Iterable[Tuple[int, Node]]:
        posn, facing, nconseq = node

        is_start = posn == start
        heat_loss = 0 if is_start else grid[posn.row][posn.col]

        if is_start or (nconseq < max_conseq):
            yield heat_loss, (forward(posn, facing), facing, nconseq + 1)

        if is_start or (nconseq >= min_turn_thresh):
            next_posn, next_facing = turn_left(posn, facing)
            if is_start or (1 + dist(next_posn, next_facing) >= min_turn_thresh):
                yield heat_loss, (next_posn, next_facing, 1)

            next_posn, next_facing = turn_right(posn, facing)
            if is_start or (1 + dist(next_posn, next_facing) >= min_turn_thresh):
                yield heat_loss, (next_posn, next_facing, 1)

    def neighbor_fn(node: Node) -> Iterable[Tuple[(int, Node)]]:
        for neighbor in neighbor_fn_helper(node):
            _, (posn, _, _) = neighbor
            if (0 <= posn.row < nrow) and (0 <= posn.col < ncol):
                yield neighbor

    path = dijkstra(
        neighbor_fn,
        (start, Direction.Right, 0),
        lambda node: node[0] == end,
    )

    return grid[end.row][end.col] + min(
        chain(
            [1 << 31],
            map(
                lambda pair: pair[1][1],
                filter(lambda pair: pair[0][0] == end, path.items()),
            ),
        )
    )


def solve_part_1(grid: List[List[int]]) -> int:
    return solve(grid)


def solve_part_2(grid: List[List[int]]) -> int:
    return solve(grid, max_conseq=10, min_turn_thresh=4)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--filepath", help="File path of the input", required=True)
    args = parser.parse_args()
    grid = list(
        map(
            lambda row: list(map(int, row)), Path(args.filepath).read_text().split("\n")
        )
    )
    print(f"Part 1: {solve_part_1(grid)}")
    print(f"Part 2: {solve_part_2(grid)}")


if __name__ == "__main__":
    main()
