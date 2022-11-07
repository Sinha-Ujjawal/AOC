from typing import Optional, List, Union, Text, TypeVar, Callable, Tuple, Dict
from dataclasses import dataclass
import sys
import re

T = TypeVar("T")

def lazy_or(*thunks: Callable[[], Optional[T]]) -> Optional[T]:
    for thunk in thunks:
        ret = thunk()
        if ret is not None:
            return ret
    return None

class U16:
    def __init__(self, x: int):
        upper_byte = (x >> 8) & 255
        lower_byte = x & 255
        self._num = (upper_byte << 8) | lower_byte

    @property
    def val(self):
        return self._num

    def __invert__(self) -> "U16":
        return U16(~self._num)

    def __and__(self, other: "U16") -> "U16":
        return U16(self._num & other._num)

    def __or__(self, other: "U16") -> "U16":
        return U16(self._num | other._num)

    def __lshift__(self, other: "U16") -> "U16":
        return U16(self._num << other._num)

    def __rshift__(self, other: "U16") -> "U16":
        return U16(self._num >> other._num)


Wire = Text
Value = U16
Probe = Union[Wire, Value]

@dataclass
class CopyFrom:
    p: Probe
    out: Wire

@dataclass
class AndGate:
    p1: Probe
    p2: Probe
    out: Wire

@dataclass
class OrGate:
    p1: Probe
    p2: Probe
    out: Wire

@dataclass
class NotGate:
    p: Probe
    out: Wire

@dataclass
class LShift:
    p1: Probe
    p2: Probe
    out: Wire

@dataclass
class RShift:
    p1: Probe
    p2: Probe
    out: Wire

Instruction = Union[CopyFrom, AndGate, OrGate, NotGate, LShift, RShift]

def try_parse_copy_from(line: str) -> Optional[CopyFrom]:
    matches = re.findall("^(\d+) -> ([a-z]+)$|^([a-z]+) -> ([a-z]+)$", line)
    if not matches:
        return None
    [n, w, w1, w2] = matches[0]
    if n != "":
        return CopyFrom(U16(int(n)), w)
    return CopyFrom(w1, w2)

def try_parse_not_gate(line: str) -> Optional[NotGate]:
    matches = re.findall("^NOT (\d+) -> ([a-z]+)$|^NOT ([a-z]+) -> ([a-z]+)$", line)
    if not matches:
        return None
    [n, w, w1, w2] = matches[0]
    if n != "":
        return NotGate(U16(int(n)), w)
    return NotGate(w1, w2)

def try_parse_bin_op(bin_op: str, line: str) -> Optional[Tuple[Probe, Probe, Wire]]:
    pattern = ""
    pattern += f"^(\d+) {bin_op} (\d+) -> ([a-z]+)$"
    pattern += f"|^([a-z]+) {bin_op} (\d+) -> ([a-z]+)$"
    pattern += f"|^(\d+) {bin_op} ([a-z]+) -> ([a-z]+)$"
    pattern += f"|^([a-z]+) {bin_op} ([a-z]+) -> ([a-z]+)$"
    matches = re.findall(pattern, line)
    if not matches:
        return None
    
    n, n, w, *rest = matches[0]
    if n != "":
        return (U16(int(n)), U16(int(n)), w)
    
    w1, n, w2, *rest = rest
    if w1 != "":
        return (w1, U16(int(n)), w2)

    n, w1, w2, *rest = rest
    if n != "":
        return (U16(int(n)), w1, w2)
    
    w1, w2, w3, *rest = rest
    if w1 != "":
        return (w1, w2, w3)
    
    return None    

def try_parse_and_gate(line: str) -> Optional[AndGate]:
    ret = try_parse_bin_op("AND", line)
    if ret is None:
        return None
    p1, p2, w = ret
    return AndGate(p1, p2, w)

def try_parse_or_gate(line: str) -> Optional[OrGate]:
    ret = try_parse_bin_op("OR", line)
    if ret is None:
        return None
    p1, p2, w = ret
    return OrGate(p1, p2, w)

def try_parse_lshift(line: str) -> Optional[LShift]:
    ret = try_parse_bin_op("LSHIFT", line)
    if ret is None:
        return None
    p1, p2, w = ret
    return LShift(p1, p2, w)

def try_parse_rshift(line: str) -> Optional[RShift]:
    ret = try_parse_bin_op("RSHIFT", line)
    if ret is None:
        return None
    p1, p2, w = ret
    return RShift(p1, p2, w)


def parse_instruction(line: str) -> Optional[Instruction]:
    # This ordering of check is important
    # As NOT x -> y can match the pattern x -> y
    return lazy_or(  # type: ignore
        lambda: try_parse_and_gate(line),
        lambda: try_parse_or_gate(line),
        lambda: try_parse_lshift(line),
        lambda: try_parse_rshift(line),
        lambda: try_parse_not_gate(line),
        lambda: try_parse_copy_from(line),
    )

def parse_instructions_from_lines(lines: List[str]) -> Optional[List[Instruction]]:
    ret = []
    for line in lines:
        ins = parse_instruction(line)
        if ins is None:
            return None
        ret.append(ins)
    return ret

def parse_instructions_from_file(file_name: str) -> Optional[List[Instruction]]:
    with open(file_name, "r") as fp:
        return parse_instructions_from_lines(fp.readlines())

def wire_from_instruction(ins: Instruction) -> Wire:
    if isinstance(ins, CopyFrom):
        return ins.out
    elif isinstance(ins, NotGate):
        return ins.out
    elif isinstance(ins, AndGate):
        return ins.out
    elif isinstance(ins, OrGate):
        return ins.out
    elif isinstance(ins, LShift):
        return ins.out
    elif isinstance(ins, RShift):
        return ins.out
    raise Exception("Unreachable!")

def collect_wires(instructions: List[Instruction]) -> Dict[Wire, Instruction]:
    return {wire_from_instruction(ins): ins for ins in instructions}

def evaulate_at(instructions: List[Instruction], w: Wire) -> Optional[Value]:
    ins_map = collect_wires(instructions)
    memo: Dict[Wire, Optional[Value]] = {}

    def evaulate_probe(p: Probe) -> Optional[Value]:
        if isinstance(p, Wire):
            return evalulate_wire(p)
        return p

    def evalulate_wire(w: Wire) -> Optional[Value]:
        if w in memo:
            return memo[w]
        ins = ins_map.get(w)
        if ins is None:
            return None
        val = evalulate_ins(ins)
        memo[w] = val
        return val

    def evalulate_ins(ins: Instruction) -> Optional[Value]:
        if ins is None:
            return None
        if isinstance(ins, CopyFrom):
            return evaulate_probe(ins.p)
        elif isinstance(ins, NotGate):
            val = evaulate_probe(ins.p)
            if val is None:
                return val
            return ~val
        elif isinstance(ins, AndGate):
            p1_val = evaulate_probe(ins.p1)
            if p1_val is None:
                return None
            p2_val = evaulate_probe(ins.p2)
            if p2_val is None:
                return None
            return p1_val & p2_val
        elif isinstance(ins, OrGate):
            p1_val = evaulate_probe(ins.p1)
            if p1_val is None:
                return None
            p2_val = evaulate_probe(ins.p2)
            if p2_val is None:
                return None
            return p1_val | p2_val
        elif isinstance(ins, LShift):
            p1_val = evaulate_probe(ins.p1)
            if p1_val is None:
                return None
            p2_val = evaulate_probe(ins.p2)
            if p2_val is None:
                return None
            return p1_val << p2_val
        elif isinstance(ins, RShift):
            p1_val = evaulate_probe(ins.p1)
            if p1_val is None:
                return None
            p2_val = evaulate_probe(ins.p2)
            if p2_val is None:
                return None
            return p1_val >> p2_val
        return None
    return evalulate_wire(w)

if __name__ == "__main__":
    if len(sys.argv) > 1:
        file_name = sys.argv[1]
    else:
        file_name = input("Enter file name: ")

    instructions = parse_instructions_from_file(file_name)
    if instructions is None:
        raise  Exception(f"Unable to parse instructions from the file: {file_name}")
    
    at = input("Evalulate at? ")
    part1_ans = evaulate_at(instructions, at)
    if part1_ans is None:
        raise Exception(f"Could not evalulate the cicuit at wire: {at}")
    print(f"Part 1: {part1_ans.val}")
    override_wire = input(f"Override which wire with value: {part1_ans.val}? ")
    instructions = (
        [
            ins
            for ins in instructions
            if wire_from_instruction(ins) != override_wire
        ]
        + [CopyFrom(part1_ans, override_wire)]
    )
    part2_ans = evaulate_at(instructions, at)
    if part2_ans is None:
        raise Exception(f"Could not evalulate the overriden cicuit at wire: {at}")
    print(f"Part 2: {part2_ans.val}")
