
-module(bencode).

-export([bdecode/1, bencode/1]).

-import(string, [substr/2, substr/3, to_integer/1, chr/2]).

%% %% %% %% %% %% %% %% Decoding %% %% %% %% %% %% %% %%

bencode(Val) when is_integer(Val) ->
    "i" ++ integer_to_list(Val) ++ "e".

%% %% %% %% %% %% %% %% Decoding %% %% %% %% %% %% %% %%

%% String
bdecode([Char | _Rest] = Str) when Char >= $0, Char =< $9 ->
    ColIdx = chr(Str, $:),
    IntStr = substr(Str, 1, ColIdx-1),
    {Len, _} = to_integer(IntStr),
    Result = substr(Str, ColIdx+1, Len),
    Rest = substr(Str, ColIdx+Len+1),
    {{string, Result}, Rest};

%% Int
bdecode([Char | Rest]) when Char == $i ->
    End = chr(Rest, $e),
    {Result, _} = to_integer(substr(Rest, 1, End-1)),
    {{int, Result}, substr(Rest, End+1)};

%% List
bdecode([Char | Rest]) when Char == $l ->
    {List, Rem} = declist(Rest),
    {{list, List}, Rem};

%% Dict
bdecode([Char | Rest]) when Char == $d ->
    {Dict, Rem} = decdict(Rest),
    {{dict, Dict}, Rem}.


declist(Str) ->
    declist(Str, []).

declist([Char | Rest], List) when Char == $e ->
    {List, Rest};

declist(Str, List) ->
    {Result, Rest} = bdecode(Str),
    declist(Rest, lists:append(List, [Result])).


decdict(Str) ->
    decdict(Str, dict:new()).

decdict([Char | Rest], Dict) when Char == $e ->
    {Dict, Rest};

decdict(Str, Dict) ->
    {{string, Key}, Rest} = bdecode(Str),
    {Val, Rest2} = bdecode(Rest),
    decdict(Rest2, dict:store(Key, Val, Dict)).
