:- module solve.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module string, list, int, array.

:- pred array_width(int::out) is det.
array_width(1000).

:- pred array_size(int::out) is det.
array_size(Size) :- array_width(Width), Size = Width * Width.

:- pred prompt(string::in, io.result(string)::out, io::di, io::uo) is det.
prompt(Msg, Result, !IO) :-
    io.write_string(Msg, !IO),
    io.read_line_as_string(LineRes, !IO),
    (
        if LineRes = ok(Line) then
            Result = ok(string.rstrip(Line))
        else
            Result = LineRes
    ).

:- pred prompt_for_file_path(string::out, io::di, io::uo) is det.
prompt_for_file_path(FilePath, !IO) :-
    prompt("Enter file path: ", FilePathRes, !IO),
    (
        FilePathRes = ok(String),
        FilePath = String
    ;
        FilePathRes = eof,
        FilePath = "",
        io.set_exit_status(1, !IO),
        io.write_string("Read EOF in file path!\n", !IO)
    ;
        FilePathRes = error(ErrorCode),
        FilePath = "",
        io.set_exit_status(1, !IO),
        io.format("%s\n", [s(io.error_message(ErrorCode))], !IO)
    ).

:- pred read_file_path(string::out, io::di, io::uo) is det.
read_file_path(FilePath, !IO) :-
    io.command_line_arguments(Args, !IO),
    (
        if Args = [String | _] then
            FilePath = String
        else
            prompt_for_file_path(FilePath, !IO)
    ).

:- func string_as_lines_array(string) = array(string).
:- mode string_as_lines_array(in) = array_uo is det.
string_as_lines_array(String) = Array :-
    string_as_lines_array(String, 0, string.length(String), array.make_empty_array, Array).

:- pred string_as_lines_array(
    string::in,
    int::in,
    int::in,
    array(string)::array_di,
    array(string)::array_uo
) is det.
string_as_lines_array(String, Frm, Length, !.Array, !:Array) :-
    (
        if string.sub_string_search_start(String, "\n", Frm, To), Frm =< To then
            array_push(string.between(String, Frm, To), !.Array, !:Array),
            string_as_lines_array(String, To+1, Length, !.Array, !:Array)
        else if Frm < Length then
            array_push(string.between(String, Frm, Length), !.Array, !:Array),
            !:Array = !.Array
        else
            !:Array = !.Array
    ).


:- pred read_named_file_as_lines_array(
    string::in,
    io.res(array(string))::out,
    io::di, io::uo
) is det.
read_named_file_as_lines_array(FilePath, LinesRes, !IO) :-
    io.read_named_file_as_string(FilePath, Res, !IO),
    (
        Res = ok(String),
        LinesRes = ok(string_as_lines_array(String))
    ;
        Res = error(ErrorCode),
        LinesRes = error(ErrorCode)
    ).

:- pred read_input(array(string)::out, io::di, io::uo) is det.
read_input(Lines, !IO) :-
    read_file_path(FilePath, !IO),
    read_named_file_as_lines_array(FilePath, LinesRes, !IO),
    (
        LinesRes = ok(Lines),
        io.format("Solving File: %s\n", [s(FilePath)], !IO)
    ;
        LinesRes = error(ErrorCode),
        Lines = array.make_empty_array,
        io.set_exit_status(1, !IO),
        io.format("%s\n", [s(io.error_message(ErrorCode))], !IO)
    ).

:- pred array_push(T::in, array(T)::array_di, array(T)::array_uo) is det.
array_push(Item, !.Array, !:Array) :-
    OldSize = array.size(!.Array),
    NewSize = OldSize + 1,
    array.resize(NewSize, Item, !.Array, !:Array).

:- type result(Ok, Err) ---> ok(Ok); error(Err).

:- type coord ---> coord(x :: int, y :: int).

:- type instruction_type
    --->    turn_on
    ;       toggle
    ;       turn_off.

:- type instruction ---> instruction(instruction_type, coord, coord).

:- type parse_error ---> parse_error(line_no :: int, line :: string).

:- type result_instructions == result(array(instruction), parse_error).

:- pred parse_instructions(array(string)::in, result_instructions::out) is det.
parse_instructions(InstructionsAsLines, Out) :-
    ArrayIn = array.make_empty_array,
    parse_instructions(
        InstructionsAsLines,
        0, array.size(InstructionsAsLines),
        ArrayIn,
        Out
    ).

:- pred parse_instructions(
    array(string)::in,
    int::in, int::in,
    array(instruction)::array_di,
    result_instructions::out
) is det.
parse_instructions(InstructionsAsLines, Index, Length, Array, Out) :-
    (
        if Index < Length then
            array.unsafe_lookup(InstructionsAsLines, Index, InstructionAsString),
            (
                if parse_instruction(InstructionAsString, Instruction) then
                    array_push(Instruction, Array, NewArray),
                    parse_instructions(InstructionsAsLines, Index+1, Length, NewArray, Out)
                else
                    Out = error(parse_error(Index+1, InstructionAsString))
            )
        else
            Out = ok(Array)
    ).

:- pred parse_instruction(string::in, instruction::out) is semidet.
parse_instruction(InstructionAsString, Out) :-
    (
        if
            parse_instruction_type(InstructionAsString, InstructionType, Rest),
            parse_rest(Rest, CoordFrom, CoordTo)
        then
            Out = instruction(InstructionType, CoordFrom, CoordTo)
        else
            false
    ).

:- pred parse_instruction_type(string::in, instruction_type::out, string::out) is semidet.
parse_instruction_type(InstructionAsString, InstructionType, Rest) :-
    (
        if string.prefix(InstructionAsString, "turn on ") then
            string.remove_prefix("turn on ", InstructionAsString, Rest),
            InstructionType = turn_on
        else if string.prefix(InstructionAsString, "toggle ") then
            string.remove_prefix("toggle ", InstructionAsString, Rest),
            InstructionType = toggle
        else if string.prefix(InstructionAsString, "turn off ") then
            string.remove_prefix("turn off ", InstructionAsString, Rest),
            InstructionType = turn_off
        else
            false
    ).

:- pred parse_rest(string::in, coord::out, coord::out) is semidet.
parse_rest(CoordPartOfString, CoordFrom, CoordTo) :-
    SplitString = string.split_at_string(" through ", CoordPartOfString),
    (
        SplitString = [CoordFromString, CoordToString],
        parse_coord(CoordFromString, CoordFrom),
        parse_coord(CoordToString, CoordTo)
    ;
        false
    ).

:- pred parse_coord(string::in, coord::out) is semidet.
parse_coord(CoordString, Coord) :-
    SplitString = string.split_at_string(",", CoordString),
    (
        SplitString = [CoordXString, CoordYString],
        string.to_int(CoordXString, CoordX),
        string.to_int(CoordYString, CoordY),
        Coord = coord(CoordX, CoordY)
    ;
        false
    ).

:- pred int_loop(
    int::in,
    int::in,
    int::in,
    pred(int, S, S)::in(pred(in, in, out) is det),
    S::in,
    S::out
) is det.
int_loop(Frm, To, Step, Body, !.State, !:State) :-
    (
        if Frm =< To then
            Body(Frm, !.State, !:State),
            int_loop(Frm+Step, To, Step, Body, !.State, !:State)
        else
            true
    ).

:- pred solve(
    array(instruction)::in,
    pred(int, int)::in(pred(in, out) is det),
    pred(int, int)::in(pred(in, out) is det),
    pred(int, int)::in(pred(in, out) is det),
    array(int)::array_uo
) is det.
solve(
    Instructions,
    TurnOnBrightnessUpdater,
    ToggleBrightnessUpdater,
    TurnOffBrightnessUpdater,
    ArrayOut
) :-
    array_width(ArrayWidth),
    array_size(ArraySize),
    array.init(ArraySize, 0, ArrayIn),
    array.foldl(
        (
            pred(Instruction::in, !.Array::in, !:Array::out) is det :-
                Instruction = instruction(
                    InstructionType,
                    coord(CoordX1, CoordY1),
                    coord(CoordX2, CoordY2)
                ),
                (
                    InstructionType = turn_on,
                    BrightnessUpdater = TurnOnBrightnessUpdater
                ;
                    InstructionType = toggle,
                    BrightnessUpdater = ToggleBrightnessUpdater
                ;
                    InstructionType = turn_off,
                    BrightnessUpdater = TurnOffBrightnessUpdater
                ),
                int_loop(
                    CoordX1, CoordX2, 1,
                    (
                        pred(CoordX::in, !.Array::in, !:Array::out) is det :-
                            int_loop(
                                CoordY1, CoordY2, 1,
                                (
                                    pred(CoordY::in, !.Array::in, !:Array::out) is det :-
                                        Index = (CoordX * ArrayWidth) + CoordY,
                                        array.unsafe_lookup(!.Array, Index, OldValue),
                                        BrightnessUpdater(OldValue, NewValue),
                                        array.unsafe_set(Index, NewValue, !.Array, !:Array)
                                ),
                                !.Array,
                                !:Array
                            )
                    ),
                    !.Array,
                    !:Array
                )
        ),
        Instructions,
        ArrayIn,
        ArrayOut
    ),
    true.

:- func solve_part1(array(instruction)) = int.
solve_part1(Instructions) = Ret :-
    solve(
        Instructions,
        (pred(_::in, BOut::out) is det :- BOut = 1),
        (pred(BIn::in, BOut::out) is det :- ( if BIn > 0 then BOut = 0 else BOut = 1 )),
        (pred(_::in, BOut::out) is det :- BOut = 0),
        Array
    ),
    array.foldl(
        (
            pred(B::in, PrevCount::in, NextCount::out) is det:-
                ( if B > 0 then NextCount = PrevCount + 1 else NextCount = PrevCount )
        ),
        Array,
        0,
        Ret
    ).

:- func solve_part2(array(instruction)) = int.
solve_part2(Instructions) = Ret :-
    solve(
        Instructions,
        (pred(BIn::in, BOut::out) is det :- BOut = BIn + 1),
        (pred(BIn::in, BOut::out) is det :- BOut = BIn + 2),
        (pred(BIn::in, BOut::out) is det :- ( if BIn > 0 then BOut = BIn - 1 else BOut = BIn )),
        Array
    ),
    array.foldl(
        (
            pred(B::in, Prev::in, Next::out) is det:-
                Next = Prev + B
        ),
        Array,
        0,
        Ret
    ).

main(!IO) :-
    read_input(Lines, !IO),
    parse_instructions(Lines, InstructionsOrErr),
    (
        InstructionsOrErr = ok(Instructions),
        io.format("Part 1: %d\n", [i(solve_part1(Instructions))], !IO),
        io.format("Part 2: %d\n", [i(solve_part2(Instructions))], !IO),
        true
    ;
        InstructionsOrErr = error(parse_error(Line, String)),
        io.format("Could not parse line: %d: ""%s""\n", [i(Line), s(String)], !IO)       
    ),
    true.
