
-module(tests).

-include("eunit.hrl").

-export([start/0]).
-export([test_dec_int1/0, test_dec_int2/0, test_dec_int3/0, test_dec_int4/0, 
	 test_dec_string1/0, test_dec_string2/0, 
	 test_dec_list1/0, test_dec_list2/0, test_dec_list3/0, test_dec_list4/0, test_dec_list5/0, 
	 test_dec_dict1/0]).
-export([test_enc_int1/0]).

%% Integer decoding tests
test_enc_int1() ->
    ?match("i999e", bencode:bencode(999)).

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
