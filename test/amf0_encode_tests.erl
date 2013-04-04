-module(amf0_encode_tests).

-include("../include/amf.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Auxiliary Function
read_testdata(Name) ->
    SourceFilePath = proplists:get_value(source, ?MODULE:module_info(compile)),
    TestDataDir = filename:dirname(SourceFilePath) ++ "/testdata",
    Path = TestDataDir ++ "/" ++ Name,

    Result = file:read_file(Path),
    ?assertMatch({Path, {ok, _}}, {Path, Result}),
    {ok, Bin} = Result,
    Bin.

%% Assertion wrapper Macros
-define(assertEncodeBin(Expected, Input), 
        begin
            ?assertMatch({ok, _}, amf0_encode:encode(Input)),
            {ok, EncodedIoList} = amf0_encode:encode(Input),
            
            ?assertEqual(Expected, list_to_binary(EncodedIoList))
        end).

-define(assertEncode(Expected, Input), 
        begin
            ?assertMatch({ok, _}, amf0_encode:encode(Input)),
            {ok, EncodedIoList} = amf0_encode:encode(Input),
            
            ExpectedResult = amf0_decode:decode(Expected), 
            ?assertMatch(ExpectedResult, amf0_decode:decode(list_to_binary(EncodedIoList)))
        end).

-define(assertEncodeError(Expected, Input),
        begin
            ?assertMatch({error, Expected}, amf0_encode:encode(Input))
        end).

%% Test Functions
decode_number_test() ->
    Expected = read_testdata("amf0-number.bin"),
    Input = 3.5,
    ?assertEncodeBin(Expected, Input).

encode_boolean_true_test() ->
    Expected = read_testdata("amf0-boolean-true.bin"),
    Input = true,
    ?assertEncodeBin(Expected, Input).

encode_boolean_false_test() ->
    Expected = read_testdata("amf0-boolean-false.bin"),
    Input = false,
    ?assertEncodeBin(Expected, Input).

encode_string_test() ->
    Expected = read_testdata("amf0-string.bin"),
    Input = <<"this is a テスト">>,
    ?assertEncodeBin(Expected, Input).

encode_long_string_test() ->
    Expected = read_testdata("amf0-long-string.bin"),
    Input = << <<97>> || _ <- lists:seq(0, 16#10012)>>,
    ?assertEncodeBin(Expected, Input).

encode_complex_encoded_string_test() ->
    Expected = read_testdata("amf0-complex-encoded-string.bin"),
    Input = amf:object([{<<"utf">>, <<"UTF テスト">>},
                        {<<"zed">>, 5.0},
                        {<<"shift">>, <<"Shift テスト">>}]),
    ?assertEncodeBin(Expected, Input).

encode_object_test() ->
    Expected = read_testdata("amf0-object.bin"),
    Input = amf:object([{<<"foo">>, <<"baz">>},
                        {<<"bar">>, 3.14}]),
    ?assertEncodeBin(Expected, Input).

encode_untyped_object_test() ->
    Expected = read_testdata("amf0-untyped-object.bin"),
    Input = amf:object([{<<"foo">>, <<"bar">>},
                        {<<"baz">>, null}]),
    ?assertEncodeBin(Expected, Input).

encode_composed_object_test() ->
    Expected = read_testdata("amf0-ref-test.bin"),

    Obj = amf:object([{<<"foo">>, <<"baz">>},
                      {<<"bar">>, 3.14}]),
    Input = amf:object([{<<"0">>, Obj},
                        {<<"1">>, Obj}]),

    ?assertEncode(Expected, Input).

encode_null_test() ->
    Expected = read_testdata("amf0-null.bin"),
    Input = null,
    ?assertEncodeBin(Expected, Input).

encode_undefined_test() ->
    Expected = read_testdata("amf0-undefined.bin"),
    Input = undefined,
    ?assertEncodeBin(Expected, Input).

encode_ecma_array_test() ->
    Expected = read_testdata("amf0-ecma-ordinal-array.bin"),
    Input = amf:array(
              [{<<"0">>, <<"a">>},
               {<<"1">>, <<"b">>},
               {<<"2">>, <<"c">>},
               {<<"3">>, <<"d">>}]),
    ?assertEncodeBin(Expected, Input).

encode_hash_test() ->
    Expected = read_testdata("amf0-hash.bin"),
    Input = amf:array(
              [{<<"c">>, <<"d">>},
               {<<"a">>, <<"b">>}]),
    ?assertEncode(Expected, Input).

encode_strict_array_test() ->
    Expected = read_testdata("amf0-strict-array.bin"),
    Input = [1.0, <<"2">>, 3.0],
    ?assertEncodeBin(Expected, Input).

encode_date_test() ->
    Expected = read_testdata("amf0-date.bin"),
    Input = amf:datetime_to_date({{2020,5,30}, {0,0,0}}),
    ?assertEncodeBin(Expected, Input).    

encode_time_test() ->
    Expected = read_testdata("amf0-time.bin"),
    Input = amf:datetime_to_date({{2003,2,13}, {5,0,0}}),
    ?assertEncodeBin(Expected, Input).    

encode_xml_document_test() ->
    Expected = read_testdata("amf0-xml-doc.bin"),
    Input = amf:xml_document(<<"<parent><child prop=\"test\" /></parent>">>),
    ?assertEncodeBin(Expected, Input).    
    
encode_typed_object_test() ->
    Expected = read_testdata("amf0-typed-object.bin"),
    Input = amf:typed_object(<<"org.amf.ASClass">>,
                             [{<<"foo">>, <<"bar">>},
                              {<<"baz">>, null}]),
    ?assertEncodeBin(Expected, Input).

encode_avmplus_object_test_() ->
    {setup,
     fun () -> meck:new(amf3_encode, [no_passthrough_cover]) end,
     fun (_) -> meck:unload(amf3_encode) end,
     [
      fun () ->
              Expected = <<"dummy">>,
              meck:expect(amf3_encode, unsafe_encode, 1, [Expected]),
              
              Input = amf:avmplus_object(1.0),
              ?assertEncodeBin(Expected, Input)
      end
     ]}.

encode_unknown_test() ->
    Input = {1, 2, 3},
    ?assertEncodeError(#amf_exception{type=unsupported, message={value,_}}, Input).

encode_too_large_class_name_test() ->
    Name = << <<97>> || _ <- lists:seq(0, 16#10012)>>,
    Input = amf:typed_object(Name,
                             [{<<"foo">>, <<"bar">>},
                              {<<"baz">>, null}]),
    ?assertEncodeError(#amf_exception{type=invalid, message={class_name,_,_}}, Input).

encode_too_largeg_object_key_test() ->
    Key = << <<97>> || _ <- lists:seq(0, 16#10012)>>,
    Input = amf:object([{Key, <<"bar">>},
                        {<<"baz">>, null}]),
    ?assertEncodeError(#amf_exception{type=invalid, message={key,_,_}}, Input).

%% Performance Test
-ifdef(BENCH).
encode_speed_test() ->
    case cover:is_compiled(amf0_encode) of
        false ->
            Files = ["amf0-complex-encoded-string.bin",
                     "amf0-ref-test.bin",
                     "amf0-ecma-ordinal-array.bin",
                     "amf0-typed-object.bin",
                     "amf0-strict-array.bin"],
            Values = [element(1,amf0_decode:decode(read_testdata(File))) || File <- Files],
            N = lists:seq(1, 50000),
            ?debugTime("amf0_encode",
                       lists:foreach(fun(_) -> 
                                             lists:foreach(fun amf0_encode:encode_to_iolist/1, Values)
                                     end, N));
        _ ->
            skip
    end.
-endif.
