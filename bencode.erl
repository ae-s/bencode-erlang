
-module(bencode).

-export([bdecode/1]).

-import(string, [substr/2, substr/3, to_integer/1, chr/2]).

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
    {List, Rem} = dolist(Rest),
    {{list, List}, Rem};

%% Dict
bdecode([Char | Rest]) when Char == $d ->
    {Dict, Rem} = dodict(Rest),
    {{dict, Dict}, Rem}.


dolist(Str) ->
    dolist(Str, []).

dolist([Char | Rest], List) when Char == $e ->
    {List, Rest};

dolist(Str, List) ->
    {Result, Rest} = bdecode(Str),
    dolist(Rest, lists:append(List, [Result])).


dodict(Str) ->
    dodict(Str, dict:new()).

dodict([Char | Rest], Dict) when Char == $e ->
    {Dict, Rest};

dodict(Str, Dict) ->
    {{string, Key}, Rest} = bdecode(Str),
    {Val, Rest2} = bdecode(Rest),
    dodict(Rest2, dict:store(Key, Val, Dict)).
    
