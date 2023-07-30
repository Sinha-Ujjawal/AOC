:- module solve.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module char, int, string, list, hash_table.

:- pred prompt(string::in, io.result(string)::out, io::di, io::uo) is det.
prompt(Msg, Result, !IO) :-
    io.write_string(Msg, !IO),
    io.read_line_as_string(LineRes, !IO),
    (
        if LineRes = ok(String) then
            Result = ok(string.rstrip(String))
        else
            Result = LineRes
    ).

:- pred read_input(list(string)::out, io::di, io::uo) is det.
read_input(Lines, !IO) :-
    prompt("Enter file path: ", FilePathRes, !IO),
    (
        FilePathRes = ok(FilePath),
        io.read_named_file_as_lines(FilePath, LinesRes, !IO),
        (
            LinesRes = ok(Lines)
        ;
            LinesRes = error(ErrorCode),
            Lines = [],
            io.format("%s\n", [s(io.error_message(ErrorCode))], !IO)
        )
    ;   FilePathRes = eof,
        Lines = [],
        io.write_string("bye!\n", !IO)
    ;   FilePathRes = error(ErrorCode),
        Lines = [],
        io.format("%s\n", [s(io.error_message(ErrorCode))], !IO)
    )
    .

:- type coord ---> coord(x :: int, y :: int).

:- func hash_coord(coord) = int.
:- mode hash_coord(in) = out is det.
hash_coord(Coord) = hash(hash(Coord^x) + hash(Coord^y)).

:- pred hash_coord(coord::in, int::out) is det.
hash_coord(Coord, Hash) :- Hash = hash_coord(Coord).

:- func next_coord(coord, char) = coord.
next_coord(Coord, Instruction) =
    (
        if next_coord(Coord, Instruction, NextCoord) then
            NextCoord
        else
            Coord
    ).

:- pred next_coord(coord::in, char::in, coord::out) is semidet.
next_coord(Coord, '^', coord(Coord^x,   Coord^y-1)).
next_coord(Coord, 'v', coord(Coord^x,   Coord^y+1)).
next_coord(Coord, '<', coord(Coord^x-1, Coord^y)).
next_coord(Coord, '>', coord(Coord^x+1, Coord^y)).

:- pred print_hash_table(string::in, hash_table(K, V)::hash_table_ui, io::di, io::uo) is det.
print_hash_table(HashTableLabel, HashTable, !IO) :-
    io.format("%s(size: %d): ", [s(HashTableLabel), i(hash_table.num_occupants(HashTable))], !IO),
    io.print(hash_table.to_assoc_list(HashTable), !IO), io.nl(!IO).

:- func part_1_visits(string, int, int, coord, hash_table(coord, int)) = hash_table(coord, int).
:- mode part_1_visits(in, in, in, in, hash_table_di) = hash_table_uo is det.
part_1_visits(Instructions, Index, Length, CurrentCoordSanta, HashTable) = HashTableOut :-
    HashTable1 = hash_table.set(HashTable, CurrentCoordSanta, 1),
    (
        if Index < Length then
            CurrentInstruction = string.unsafe_index(Instructions, Index),
            NextCoordSanta = next_coord(CurrentCoordSanta, CurrentInstruction),
            HashTableOut = part_1_visits(Instructions, Index+1, Length, NextCoordSanta, HashTable1)
        else
            HashTableOut = HashTable1
    ).

:- func part_1_visits(string) = hash_table(coord, int).
:- mode part_1_visits(in) = hash_table_uo is det.
part_1_visits(Instructions) =
    part_1_visits(
        Instructions,
        0, string.length(Instructions),
        coord(0, 0),
        hash_table.init_default(hash_coord)
    ).

:- func solve_part_1(string) = int.
solve_part_1(Instructions) = Ans :-
    HashTable = part_1_visits(Instructions),
    Ans = hash_table.num_occupants(HashTable).

:- func part_2_visits(string, int, int, coord, coord, hash_table(coord, int)) = hash_table(coord, int).
:- mode part_2_visits(in, in, in, in, in, hash_table_di) = hash_table_uo is det.
part_2_visits(Instructions, Index, Length, CurrentCoordSanta, CurrentCoordRoboSanta, HashTable) = HashTableOut :-
    HashTable1 = hash_table.set(HashTable, CurrentCoordSanta, 1),
    HashTable2 = hash_table.set(HashTable1, CurrentCoordRoboSanta, 1),
    (
        if Index < Length then
            CurrentInstruction = string.unsafe_index(Instructions, Index),
            (
                if (Index `mod` 2) = 0 then
                    NextCoordSanta = next_coord(CurrentCoordSanta, CurrentInstruction),
                    NextCoordRoboSanta = CurrentCoordRoboSanta
                else
                    NextCoordSanta = CurrentCoordSanta,
                    NextCoordRoboSanta = next_coord(CurrentCoordRoboSanta, CurrentInstruction)
            ),
            HashTableOut = part_2_visits(Instructions, Index+1, Length, NextCoordSanta, NextCoordRoboSanta, HashTable2)
        else
            HashTableOut = HashTable2
    ).

:- func part_2_visits(string) = hash_table(coord, int).
:- mode part_2_visits(in) = hash_table_uo is det.
part_2_visits(Instructions) =
    part_2_visits(
        Instructions,
        0, string.length(Instructions),
        coord(0, 0), coord(0, 0),
        hash_table.init_default(hash_coord)
    ).

:- func solve_part_2(string) = int.
solve_part_2(Instructions) = Ans :-
    HashTable = part_2_visits(Instructions),
    Ans = hash_table.num_occupants(HashTable).

main(!IO) :-
    read_input(Lines, !IO),
    (
        if Lines = [Line] then
            io.format("Part 1: %d\n", [i(solve_part_1(Line))], !IO),
            io.format("Part 2: %d\n", [i(solve_part_2(Line))], !IO)
        else
            io.write_string("Exactly one line was expected in the file!\n", !IO)
    ).
