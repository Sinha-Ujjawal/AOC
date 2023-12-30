from typing import List, Dict, Iterable, Tuple, Optional, Set
from pathlib import Path
import argparse
from enum import IntEnum
from dataclasses import dataclass
from collections import deque
from itertools import islice
from math import gcd
from functools import reduce

BROADCASTER: str = "broadcaster"
FLIP_FLOP_PREFIX: str = "%"
CONJUNCTION_PREFIX: str = "&"


def lcm(a: int, b: int) -> int:
    return abs(a * b) // gcd(a, b)


class MachineType(IntEnum):
    Broadcaster: int = 0
    FlipFlop: int = 1
    Conjunction: int = 2


class Signal(IntEnum):
    Low: int = 0
    High: int = 1

    def __repr__(self) -> str:
        if self == Signal.Low:
            return "low"
        else:
            return "high"

    __str__ = __repr__

    def flip(self) -> "Signal":
        return Signal.High if self == Signal.Low else Signal.Low


@dataclass
class Machine:
    id: str
    typ: MachineType
    inputs: Dict[str, Signal]
    outputs: List[str]
    status: Signal

    def process_signal(self, signal: Signal) -> Optional[Signal]:
        if self.typ == MachineType.Broadcaster:
            self.status = signal
            return self.status
        elif self.typ == MachineType.FlipFlop:
            if signal == Signal.High:
                return None
            else:
                self.status = self.status.flip()
                return self.status
        elif self.typ == MachineType.Conjunction:
            self.status = (
                Signal.Low
                if all(map(lambda s: s == Signal.High, self.inputs.values()))
                else Signal.High
            )
            return self.status
        else:
            raise NotImplementedError(f"Not implemented for {self=}, {signal=}!")


@dataclass(frozen=True)
class Pulse:
    __slots__ = ["src", "signal", "dest"]
    src: str
    signal: Signal
    dest: str

    def __repr__(self) -> str:
        return f"{self.src} -{self.signal}-> {self.dest}"


@dataclass
class Circuit:
    machines: Dict[str, Machine]
    machine_ids: Set[str]

    def reset(self):
        for machine in self.machines.values():
            machine.status = Signal.Low
            for input in machine.inputs.keys():
                machine.inputs[input] = Signal.Low

    def penultimate_nodes(self, machine_id: str) -> List[str]:
        ret = []
        for machine in self.machines.values():
            if machine_id in machine.outputs:
                ret.append(machine.id)
        return ret

    def flip_flops(self) -> Iterable[Machine]:
        for machine in self.machines.values():
            if machine.typ == MachineType.FlipFlop:
                yield machine

    def flip_flop_statuses(self) -> Tuple[Signal, ...]:
        return tuple(
            map(
                lambda machine: machine.status,
                sorted(self.flip_flops(), key=lambda machine: machine.id),
            )
        )

    def push_button(self) -> Iterable[Pulse]:
        q = deque([Pulse("button", Signal.Low, BROADCASTER)])
        while q:
            pulse = q.popleft()
            yield pulse
            dest_machine = self.machines.get(pulse.dest)
            if dest_machine is not None:
                src_machine = self.machines.get(pulse.src)
                if src_machine is not None:
                    dest_machine.inputs[src_machine.id] = pulse.signal
                out_signal = dest_machine.process_signal(pulse.signal)
                if out_signal is not None:
                    for out in dest_machine.outputs:
                        q.append(Pulse(dest_machine.id, out_signal, out))


def parse_circuit(lines: str) -> Circuit:
    def parse_machine(line: str) -> Machine:
        parts = line.split(" -> ")
        value_error = ValueError(f"Could not parse the line as Machine: {line=}!")
        if len(parts) == 2:
            machine_id, outputs = parts
            if machine_id == BROADCASTER:
                machine_type = MachineType.Broadcaster
            elif machine_id.startswith(FLIP_FLOP_PREFIX):
                machine_id = machine_id[len(FLIP_FLOP_PREFIX) :]
                machine_type = MachineType.FlipFlop
            elif machine_id.startswith(CONJUNCTION_PREFIX):
                machine_id = machine_id[len(CONJUNCTION_PREFIX) :]
                machine_type = MachineType.Conjunction
            else:
                raise value_error
            return Machine(
                id=machine_id,
                typ=machine_type,
                inputs={},
                outputs=outputs.split(", "),
                status=Signal.Low,
            )
        else:
            raise value_error

    machines: Dict[str, Machine] = {}
    machine_ids: Set[str] = set()
    for line in lines.split("\n"):
        machine = parse_machine(line)
        machines[machine.id] = machine
        machine_ids.add(machine.id)
        machine_ids.update(machine.outputs)

    for machine_id, machine in machines.items():
        for out in machine.outputs:
            out_machine = machines.get(out, None)
            if out_machine is not None:
                out_machine.inputs[machine_id] = Signal.Low

    return Circuit(machines, machine_ids)


def solve_part_1(circuit: Circuit) -> int:
    def button_pushes() -> Iterable[Tuple[int, int, Tuple[int, ...]]]:
        while True:
            highs = lows = 0
            for pulse in circuit.push_button():
                if pulse.signal == Signal.High:
                    highs += 1
                else:
                    lows += 1
            yield highs, lows, circuit.flip_flop_statuses()

    max_push = 1000

    seen_before = set()
    cycle_highs = cycle_lows = num_pushes = 0
    cycle = []
    for hs, ls, flip_flop_statuses in button_pushes():
        if num_pushes >= max_push or flip_flop_statuses in seen_before:
            break
        cycle.append((hs, ls, flip_flop_statuses))
        cycle_highs += hs
        cycle_lows += ls
        num_pushes += 1
        seen_before.add(flip_flop_statuses)

    total_highs = cycle_highs * (max_push // num_pushes)
    total_lows = cycle_lows * (max_push // num_pushes)
    for hs, ls, _ in islice(cycle, max_push % len(cycle)):
        total_highs += hs
        total_lows += ls

    return total_highs * total_lows


def solve_part_2(circuit: Circuit) -> int:
    if "rx" not in circuit.machine_ids:
        print(f"rx not found in machine!")
        return -1

    def num_pushes(nodes: Set[str]) -> Dict[str, int]:
        num_pushes = {}
        pushes = 0
        while True:
            pushes += 1
            for pulse in circuit.push_button():
                if pulse.dest in nodes and pulse.signal == Signal.Low:
                    num_pushes[pulse.dest] = pushes
                    if len(num_pushes) == len(nodes):
                        return num_pushes

    [penultimate_node] = circuit.penultimate_nodes("rx")
    nodes = set(circuit.penultimate_nodes(penultimate_node))
    return reduce(lcm, num_pushes(nodes).values())


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--filepath", help="File path of the input", required=True)
    args = parser.parse_args()
    circuit = parse_circuit(Path(args.filepath).read_text())
    print(f"Part 1: {solve_part_1(circuit)}")
    circuit.reset()
    print(f"Part 2: {solve_part_2(circuit)}")


if __name__ == "__main__":
    main()
