:- module solve.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int, string, list.

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

:- func solve_part_1(string) = int.
solve_part_1(String) = solve_part_1(String, 0, string.length(String), 0).

:- func solve_part_1(string, int, int, int) = int.
solve_part_1(String, Index, Len, Acc) =
    (
        if Index < Len, string.unsafe_index(String, Index, Char) then
            (
                if Char = '(' then
                    solve_part_1(String, Index + 1, Len, Acc + 1)
                else if Char = ')' then
                    solve_part_1(String, Index + 1, Len, Acc - 1)
                else
                    solve_part_1(String, Index + 1, Len, Acc)
            )
        else
            Acc
    ).

:- func solve_part_2(string) = int.
solve_part_2(String) = solve_part_2(String, 0, string.length(String), 0).

:- func solve_part_2(string, int, int, int) = int.
solve_part_2(String, Index, Len, Acc) =
    (
        if Acc < 0 then
            Index
        else if Index < Len, string.unsafe_index(String, Index, Char) then
            (
                if Char = '(' then
                    solve_part_2(String, Index + 1, Len, Acc + 1)
                else if Char = ')' then
                    solve_part_2(String, Index + 1, Len, Acc - 1)
                else
                    solve_part_2(String, Index + 1, Len, Acc)
            )
        else
            -1
    ).

main(!IO) :-
    read_input(Lines, !IO),
    (
        if Lines = [Line] then
            io.format("Part 1: %d\n", [i(solve_part_1(Line))], !IO),
            io.format("Part 2: %d\n", [i(solve_part_2(Line))], !IO)
        else
            io.write_string("Expected file to have only 1 line\n", !IO)
    )
    .
