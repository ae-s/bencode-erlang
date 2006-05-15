
-module(tests).

-include("eunit.hrl").

-compile(export_all).

%% Integer decoding tests
test_enc_int1() ->
    ?match("i999e", bencode:bencode({int, 999})).
test_enc_int2() ->
    ?match("i-999e", bencode:bencode({int, -999})).

%% String encoding tests
test_enc_string1() ->
    ?match("0:", bencode:bencode({string, ""})).
test_enc_string2() ->
    ?match("3:xyz", bencode:bencode({string, "xyz"})).

%% List encoding tests
test_enc_list1() ->
    ?match("li1ei2ei3ee", bencode:bencode({list, [{int, 1},
						  {int, 2},
						  {int, 3}]})).
test_enc_list2() ->
    ?match("li1e3:xyzi3ee", bencode:bencode({list, [{int, 1},
						  {string, "xyz"},
						  {int, 3}]})).
test_enc_list3() ->
    ?match("l3:abcli1ei2ei3ee3:xyze", bencode:bencode({list, [{string, "abc"},
						  {list, [{int, 1},
							  {int, 2},
							  {int, 3}]},
						 {string, "xyz"}]})).

%% Dict encoding tests
test_enc_dict() ->
    Dict = dict:from_list([{"age", {int, 25}}, {"eyes", {string, "blue"}}]),
    ?match("d3:agei25e4:eyes4:bluee", bencode:bencode({dict, Dict})).


%% Integer decoding tests
test_dec_int1() ->
    ?match({{int, 999}, []}, bencode:bdecode("i999e")).
test_dec_int2() ->
    ?match({{int, 0}, []}, bencode:bdecode("i0e")).
test_dec_int3() ->
    ?match({{int, 123}, "abc"}, bencode:bdecode("i123eabc")).
test_dec_int4() ->
    ?match({{int, -10}, []}, bencode:bdecode("i-10e")).

%% String decoding tests
test_dec_string1() ->
    ?match({{string, ""}, []}, bencode:bdecode("0:")).
test_dec_string2() ->
    ?match({{string, "abcde"}, []}, bencode:bdecode("5:abcde")).

%% List decoding tests
test_dec_list1() ->
    ?match({{list, []}, []}, bencode:bdecode("le")).
test_dec_list2() ->
    ?match({{list, [{int,1}]}, []}, bencode:bdecode("li1ee")).
test_dec_list3() ->
    ?match({{list, [{int,1}]}, []}, bencode:bdecode("li1ee")).
test_dec_list4() ->
    ?match({{list, [{int,1}, {int,2}]}, []}, bencode:bdecode("li1ei2ee")).
test_dec_list5() ->
    ?match({{list, [{int,1},
		    {list, [{int, 2}, {int, 3}]}]},
	   []}, 
	   bencode:bdecode("li1eli2ei3eee")).

%% Dict decoding tests
test_dec_dict1() ->
    {{dict, Dict}, []} = bencode:bdecode("d3:agei25e4:eyes4:bluee"),
    ?match([{"age", {int, 25}}, {"eyes", {string, "blue"}}], dict:to_list(Dict)).

%% Entry point
start() ->
    eunit:run([tests], {prefix, "test_"}).
