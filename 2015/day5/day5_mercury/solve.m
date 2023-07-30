:- module solve.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module string, list, int, char, hash_table.

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

:- type nice_or_naughty ---> nice; naughty.

:- pred atlease_3_vowels(string::in) is semidet.
atlease_3_vowels(String) :- atlease_3_vowels(String, 0, string.length(String), 0).

:- pred atlease_3_vowels(string::in, int::in, int::in, int::in) is semidet.
atlease_3_vowels(String, Index, Length, Cnt) :-
    (
        if Cnt >= 3 then
            true
        else if Index < Length then
            (
                if is_vowel(string.unsafe_index(String, Index)) then
                    atlease_3_vowels(String, Index+1, Length, Cnt+1)
                else
                    atlease_3_vowels(String, Index+1, Length, Cnt)
            )
        else
            false
    ).

:- pred is_vowel(char::in) is semidet.
is_vowel('a').
is_vowel('e').
is_vowel('i').
is_vowel('o').
is_vowel('u').

:- pred a_letter_twice_in_a_row(string::in) is semidet.
a_letter_twice_in_a_row(String) :- a_letter_twice_in_a_row(String, 0, string.length(String)).

:- pred a_letter_twice_in_a_row(string::in, int::in, int::in) is semidet.
a_letter_twice_in_a_row(String, Index, Length) :-
    (
        if Index < (Length - 1) then
            string.unsafe_index(String, Index, C1),
            string.unsafe_index(String, Index+1, C2),
            (
                if C1 = C2 then
                    true
                else
                    a_letter_twice_in_a_row(String, Index+1, Length)
            )
        else
            false
    ).

:- pred does_not_contain(string::in, list(string)::in) is semidet.
does_not_contain(String, Exclusions) :-
    list.all_false(is_substring(String), Exclusions).

:- pred is_substring(string::in, string::in) is semidet.
is_substring(String, X) :- string.sub_string_search(String, X, _).

:- func string_is_nice_or_naughty_part_1(string) = nice_or_naughty.
string_is_nice_or_naughty_part_1(String) =
    (
        if
            atlease_3_vowels(String),
            a_letter_twice_in_a_row(String),
            does_not_contain(String, ["ab", "cd", "pq", "xy"])
        then
            nice
        else
            naughty
    ).

:- pred string_is_nice_part_1(string::in) is semidet.
string_is_nice_part_1(String) :-
    ( if string_is_nice_or_naughty_part_1(String) = nice then true else false ).

:- func solve_part_1(list(string)) = int.
solve_part_1(Strings) =
    list.length(list.filter(string_is_nice_part_1, Strings)).

:- type char_pair ---> char_pair(char, char).

:- pred hash_char_pair(char_pair::in, int::out) is det.
hash_char_pair(char_pair(X, Y), hash(hash(X) + hash(Y))).

:- pred a_pair_of_letters_appear_atleast_twice(string::in) is semidet.
a_pair_of_letters_appear_atleast_twice(String) :-
    a_pair_of_letters_appear_atleast_twice(String, 0, string.length(String), hash_table.init_default(hash_char_pair)).

:- pred a_pair_of_letters_appear_atleast_twice(string::in, int::in, int::in, hash_table(char_pair, int)::hash_table_di) is semidet.
a_pair_of_letters_appear_atleast_twice(String, Index, Length, HashTable0) :-
    (
        if Index < (Length - 1) then
            string.unsafe_index(String, Index, C1),
            string.unsafe_index(String, Index+1, C2),
            CharPair = char_pair(C1, C2),
            (
                if hash_table.search(HashTable0, CharPair, OldestIndex)
                then
                    (
                        if OldestIndex + 1 < Index then
                            true
                        else
                            a_pair_of_letters_appear_atleast_twice(String, Index+1, Length, HashTable0)
                    )
                else
                    HashTable1 = hash_table.set(HashTable0, CharPair, Index),
                    a_pair_of_letters_appear_atleast_twice(String, Index+1, Length, HashTable1)
            )
        else
            false
    ).

:- pred atleast_one_letter_which_repeats_with_exactly_one_letter_between_them(string::in) is semidet.
atleast_one_letter_which_repeats_with_exactly_one_letter_between_them(String) :-
    atleast_one_letter_which_repeats_with_exactly_one_letter_between_them(String, 0, string.length(String)).

:- pred atleast_one_letter_which_repeats_with_exactly_one_letter_between_them(string::in, int::in, int::in) is semidet.
atleast_one_letter_which_repeats_with_exactly_one_letter_between_them(String, Index, Length) :-
    (
        if Index < (Length - 2) then
            string.unsafe_index(String, Index, C1),
            string.unsafe_index(String, Index+2, C2),
            (
                if C1 = C2 then
                    true
                else
                    atleast_one_letter_which_repeats_with_exactly_one_letter_between_them(String, Index+1, Length)
            )
        else
            false
    ).

:- func string_is_nice_or_naughty_part_2(string) = nice_or_naughty.
string_is_nice_or_naughty_part_2(String) =
    (
        if
            a_pair_of_letters_appear_atleast_twice(String),
            atleast_one_letter_which_repeats_with_exactly_one_letter_between_them(String)
        then
            nice
        else
            naughty
    )
    .

:- pred string_is_nice_part_2(string::in) is semidet.
string_is_nice_part_2(String) :-
    ( if string_is_nice_or_naughty_part_2(String) = nice then true else false ).

:- func solve_part_2(list(string)) = int.
solve_part_2(Strings) =
    list.length(list.filter(string_is_nice_part_2, Strings)).

main(!IO) :-
    read_input(Lines, !IO),
    io.format("Part 1: %d\n", [i(solve_part_1(Lines))], !IO),
    io.format("Part 2: %d\n", [i(solve_part_2(Lines))], !IO),
    true.
