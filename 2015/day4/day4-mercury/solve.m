:- module solve.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module md5, string, int, uint32, list.

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

:- func solve(uint32, string) = uint32.
solve(N, SecretKey) = solve(N, SecretKey, 1u32).

:- func solve(uint32, string, uint32) = uint32.
solve(N, SecretKey, Number) = Ans :-
    SecretKeyAndNumber = string.append(SecretKey, string.uint32_to_string(Number)),
    MD5Digest = md5sum(SecretKeyAndNumber),
    (
        if first_n_zeros(N, MD5Digest) then
            Ans = Number
        else
            Ans = solve(N, SecretKey, Number+1u32)
    ).

:- pred first_n_zeros(uint32::in, string::in) is semidet.
first_n_zeros(N, String) :-
    StrLen = uint32.cast_from_int(string.length(String)),
    (
        if N = 0u32 then
            true
        else if N > StrLen then
            false
        else
            unsafe_first_n_zeros(0, N, String)
    ).

:- pred unsafe_first_n_zeros(int::in, uint32::in, string::in) is semidet.
unsafe_first_n_zeros(Index, N, String) :-
    (
        if N = 0u32 then
            true
        else if string.unsafe_index(String, Index) = '0' then
            unsafe_first_n_zeros(Index+1, N-1u32, String)
        else
            false
    ).

:- func solve_part_1(string) = uint32.
solve_part_1(SecretKey) = solve(5u32, SecretKey).

:- func solve_part_2(string) = uint32.
solve_part_2(SecretKey) = solve(6u32, SecretKey).

main(!IO) :-
    prompt("Enter secret key: ", SecretKeyRes, !IO),
    (
        SecretKeyRes = ok(SecretKey),
        io.format("Part 1: %u\n", [u32(solve_part_1(SecretKey))], !IO),
        io.format("Part 2: %u\n", [u32(solve_part_2(SecretKey))], !IO)
    ;
        SecretKeyRes = eof,
        io.write_string("bye!\n", !IO)
    ;
        SecretKeyRes = error(ErrorCode),
        io.format("%s\n", [s(io.error_message(ErrorCode))], !IO)
    ),
    true.
