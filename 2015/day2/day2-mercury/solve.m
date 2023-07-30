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

:- type lwh == {int, int, int}.

:- pred parse_line(string::in, lwh::out) is det.
parse_line(String, Out) :-
    StringSplitted = string.split_at_char('x', String),
    (
        if
            StringSplitted = [LString, WString, HString],
            string.to_int(LString, L),
            string.to_int(WString, W),
            string.to_int(HString, H)
        then
            Out = {L, W, H}
        else
            Out = {0, 0, 0}
    )
    .

:- func solve_part_1_for_single_lwh(lwh) = int.
solve_part_1_for_single_lwh({L, W, H}) = Out :-
    SurfaceAreaOfBox = (W*(L + H) + H*L) << 1,
    (
        if (L >= W), (L >= H) then
            Out = SurfaceAreaOfBox + W*H
        else if (W >= H), (W >= L) then
            Out = SurfaceAreaOfBox + H*L
        else
            Out = SurfaceAreaOfBox + L*W
    )
    .

:- func folder_part_1(lwh, int) = int.
folder_part_1(LWH, Acc) = solve_part_1_for_single_lwh(LWH) + Acc.

:- func solve_part_1(list(lwh)) = int.
solve_part_1(LWHS) = list.foldl(folder_part_1, LWHS, 0).

:- func solve_part_2_for_single_lwh(lwh) = int.
solve_part_2_for_single_lwh({L, W, H}) = Out :-
    Vol = L*W*H,
    (
        if (L >= W), (L >= H) then
            Out = Vol + ((W+H) << 1)
        else if (W >= H), (W >= L) then
            Out = Vol + ((H+L) << 1)
        else
            Out = Vol + ((L+W) << 1)
    )
    .

:- func folder_part_2(lwh, int) = int.
folder_part_2(LWH, Acc) = solve_part_2_for_single_lwh(LWH) + Acc.

:- func solve_part_2(list(lwh)) = int.
solve_part_2(LWHS) = list.foldl(folder_part_2, LWHS, 0).

main(!IO) :-
    read_input(Lines, !IO),
    list.map(parse_line, Lines, ParsedLines),
    io.format("Part 1: %d\n", [i(solve_part_1(ParsedLines))], !IO),
    io.format("Part 2: %d\n", [i(solve_part_2(ParsedLines))], !IO)
    .
