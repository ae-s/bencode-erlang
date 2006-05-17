%%
%% Basic Bencode (Bittorrent) encoder.  The details of the encoding
%% are available here: http://bittorrent.org/protocol.html
%% Technically this probably isn't fully compliant as it's not UTF8,
%% but probably won't matter in simple cases.
%%
%% Author: Steve Smith <tarka@internode.on.net>
%% License: LGPL 2.1
%% 

-module(bencode).

-export([bdecode/1, bencode/1]).

-import(string, [substr/2, substr/3, to_integer/1, chr/2]).

%% %% %% %% %% %% %% %% Encoding %% %% %% %% %% %% %% %%

keyenc(Key) when is_list(Key) ->
    bencode({string, Key});
keyenc(Key) when is_atom(Key) ->
    bencode({string, atom_to_list(Key)});
keyenc({string, _} = Key) ->
    bencode(Key).


bencode({int, Val}) ->
    "i" ++ integer_to_list(Val) ++ "e";

bencode({string, Val}) ->
    integer_to_list(length(Val)) ++ ":" ++ Val;

bencode({list, Val}) ->
    "l" ++ lists:flatten(lists:map(fun bencode/1, Val)) ++ "e";

bencode({dict, Dict}) ->
    SFun = fun({Key1, _}, {Key2, _}) ->
		   Key1 < Key2
	   end,
    List = lists:sort(SFun, dict:to_list(Dict)),
    MFun = fun({Key, Val}) ->
		   keyenc(Key) ++ bencode(Val)
	   end,
    "d" ++ lists:flatten(lists:map(MFun, List)) ++ "e".


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
