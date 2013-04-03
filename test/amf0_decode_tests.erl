-module(amf0_decode_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/amf.hrl").
-include("../include/internal/amf_internal.hrl").

-define(SOURCE_FILE, proplists:get_value(source, ?MODULE:module_info(compile))).
-define(TESTDATA_DIR, filename:dirname(?SOURCE_FILE) ++ "/testdata").
-define(TESTDATA_PATH(Name), ?TESTDATA_DIR ++ "/" ++ Name).

read_testdata(Name) ->
    Path = ?TESTDATA_PATH(Name),
    Result = file:read_file(Path),
    ?assertMatch({Path, {ok, _}}, {Path, Result}),
    {ok, Bin} = Result,
    Bin.

decode_number_test() ->
    Input = read_testdata("amf0-number.bin"),
    Expected = 3.5,
    ?assertEqual({ok, Expected, <<>>}, amf0_decode:decode(Input)).

decode_number_positive_infinity_test() ->
    Input = read_testdata("amf0-number-positive-infinity.bin"),
    ?assertMatch({error, #amf_exception{type=unsupported, message={number,_}}}, amf0_decode:decode(Input)).

decode_number_negative_infinity_test() ->
    Input = read_testdata("amf0-number-negative-infinity.bin"),
    ?assertMatch({error, #amf_exception{type=unsupported, message={number,_}}}, amf0_decode:decode(Input)).

decode_number_quiet_nan_test() ->
    Input = read_testdata("amf0-number-quiet-nan.bin"),
    ?assertMatch({error, #amf_exception{type=unsupported, message={number,_}}}, amf0_decode:decode(Input)).

decode_number_signaling_nan_test() ->
    Input = read_testdata("amf0-number-signaling-nan.bin"),
    ?assertMatch({error, #amf_exception{type=unsupported, message={number,_}}}, amf0_decode:decode(Input)).

decode_number_partial_test() ->
    Input = read_testdata("amf0-number-partial.bin"),
    ?assertMatch({error, #amf_exception{type=partial, message={number,_}}}, amf0_decode:decode(Input)).

decode_boolean_true_test() ->
    Input = read_testdata("amf0-boolean-true.bin"),
    Expected = true,
    ?assertEqual({ok, Expected, <<>>}, amf0_decode:decode(Input)).

decode_boolean_false_test() ->
    Input = read_testdata("amf0-boolean-false.bin"),
    Expected = false,
    ?assertEqual({ok, Expected, <<>>}, amf0_decode:decode(Input)).

decode_boolean_invalid_test() ->
    Input = read_testdata("amf0-boolean-invalid.bin"),
    ?assertMatch({error, #amf_exception{type=invalid, message={not_a_boolean,_}}}, amf0_decode:decode(Input)).

decode_boolean_partial_test() ->
    Input = read_testdata("amf0-boolean-partial.bin"),
    ?assertMatch({error, #amf_exception{type=partial, message={boolean,_}}}, amf0_decode:decode(Input)).
    
decode_string_test() ->
    Input = read_testdata("amf0-string.bin"),
    Expected = <<"this is a テスト">>,
    ?assertEqual({ok, Expected, <<>>}, amf0_decode:decode(Input)).

decode_complex_encoded_string_test() ->
    Input = read_testdata("amf0-complex-encoded-string.bin"),	
    Expected = amf:object([{<<"utf">>, <<"UTF テスト">>},
                           {<<"zed">>, 5.0},
                           {<<"shift">>, <<"Shift テスト">>}]),
    ?assertEqual({ok, Expected, <<>>}, amf0_decode:decode(Input)).

decode_string_partial_test() ->
    Input = read_testdata("amf0-string-partial.bin"),
    ?assertMatch({error, #amf_exception{type=partial, message={string,_}}}, amf0_decode:decode(Input)).

decode_object_test() ->
    Input = read_testdata("amf0-object.bin"),
    Expected = amf:object([{<<"foo">>, <<"baz">>},
                           {<<"bar">>, 3.14}]),
    ?assertEqual({ok, Expected, <<>>}, amf0_decode:decode(Input)).

decode_untyped_object_test() ->
    Input = read_testdata("amf0-untyped-object.bin"),
    Expected = amf:object([{<<"foo">>, <<"bar">>},
                           {<<"baz">>, null}]),
    ?assertEqual({ok, Expected, <<>>}, amf0_decode:decode(Input)).

decode_movieclip_test() ->
    Input = read_testdata("amf0-movieclip.bin"),
    ?assertMatch({error, #amf_exception{type=unsupported, message=movieclip}}, amf0_decode:decode(Input)).
    
decode_null_test() ->
    Input = read_testdata("amf0-null.bin"),
    Expected = null,
    ?assertEqual({ok, Expected, <<>>}, amf0_decode:decode(Input)).

decode_undefined_test() ->
    Input = read_testdata("amf0-undefined.bin"),
    Expected = undefined,
    ?assertEqual({ok, Expected, <<>>}, amf0_decode:decode(Input)).

decode_reference_test() ->
    Input = read_testdata("amf0-ref-test.bin"),
    Obj = amf:object([{<<"foo">>, <<"baz">>},
                      {<<"bar">>, 3.14}]),
    Expected = amf:object([{<<"0">>, Obj},
                           {<<"1">>, Obj}]),
    ?assertEqual({ok, Expected, <<>>}, amf0_decode:decode(Input)).

decode_bad_reference_test() ->
    Input = read_testdata("amf0-bad-reference.bin"),
    ?assertMatch({error, #amf_exception{type=invalid, message={undefined_object_reference,_}}}, amf0_decode:decode(Input)).

decode_circular_reference_test() ->
    Input = read_testdata("amf0-circular-reference.bin"),
    ?assertMatch({error, #amf_exception{type=unsupported, message={circular_reference,_}}}, amf0_decode:decode(Input)).

decode_reference_partial_test() ->
    Input = read_testdata("amf0-reference-partial.bin"),
    ?assertMatch({error, #amf_exception{type=partial, message={reference,_}}}, amf0_decode:decode(Input)).

decode_ecma_array_test() ->
    Input = read_testdata("amf0-ecma-ordinal-array.bin"),
    Expected = amf:array(
                 [{<<"0">>, <<"a">>},
                  {<<"1">>, <<"b">>},
                  {<<"2">>, <<"c">>},
                  {<<"3">>, <<"d">>}]),
    ?assertEqual({ok, Expected, <<>>}, amf0_decode:decode(Input)).

decode_hash_test() ->
    Input = read_testdata("amf0-hash.bin"),
    Expected = amf:array(
                 [{<<"c">>, <<"d">>},
                  {<<"a">>, <<"b">>}]),
    ?assertEqual({ok, Expected, <<>>}, amf0_decode:decode(Input)).

decode_ecma_array_partial_test() ->
    Input = read_testdata("amf0-ecma-array-partial.bin"),
    ?assertMatch({error, #amf_exception{type=partial, message={ecma_array,_}}}, amf0_decode:decode(Input)).

decode_bad_object_end_test() ->
    Input = read_testdata("amf0-bad-object-end.bin"),
    ?assertMatch({error, #amf_exception{type=invalid, message=unexpected_object_end}}, amf0_decode:decode(Input)).

decode_strict_array_test() ->
    Input = read_testdata("amf0-strict-array.bin"),
    Expected = [1.0, <<"2">>, 3.0],
    ?assertEqual({ok, Expected, <<>>}, amf0_decode:decode(Input)).

decode_strict_array_partial_test() ->
    Input = read_testdata("amf0-strict-array-partial.bin"),
    ?assertMatch({error, #amf_exception{type=partial, message={strict_array,_}}}, amf0_decode:decode(Input)).

decode_date_test() ->
    Input = read_testdata("amf0-date.bin"),
    Expected = amf:datetime_to_date({{2020,5,30}, {0,0,0}}),
    ?assertEqual({ok, Expected, <<>>}, amf0_decode:decode(Input)).

decode_time_test() ->
    Input = read_testdata("amf0-time.bin"),
    Expected = amf:datetime_to_date({{2003,2,13}, {5,0,0}}),
    ?assertEqual({ok, Expected, <<>>}, amf0_decode:decode(Input)).

decode_date_partial_test() ->
    Input = read_testdata("amf0-date-partial.bin"),
    ?assertMatch({error, #amf_exception{type=partial, message={date,_}}}, amf0_decode:decode(Input)).

decode_date_minus_test() ->
    Input = read_testdata("amf0-date-minus.bin"),
    ?assertMatch({error, #amf_exception{type=invalid, message={negative_date,_}}}, amf0_decode:decode(Input)).

decode_date_invalid_test() ->
    Input = read_testdata("amf0-date-invalid.bin"),
    ?assertMatch({error, #amf_exception{type=invalid, message={date,_}}}, amf0_decode:decode(Input)).

decode_long_string_test() ->
    Input = read_testdata("amf0-long-string.bin"),
    Expected = << <<97>> || _ <- lists:seq(0, 16#10012)>>,
    ?assertEqual({ok, Expected, <<>>}, amf0_decode:decode(Input)).

decode_long_string_partial_test() ->
    Input = read_testdata("amf0-long-string-partial.bin"),
    ?assertMatch({error, #amf_exception{type=partial, message={long_string,_}}}, amf0_decode:decode(Input)).

decode_unsupported_test() ->
    Input = read_testdata("amf0-unsupported.bin"),
    ?assertMatch({error, #amf_exception{type=unsupported, message=unsupported}}, amf0_decode:decode(Input)).

decode_recordset_test() ->
    Input = read_testdata("amf0-recordset.bin"),
    ?assertMatch({error, #amf_exception{type=unsupported, message=recordset}}, amf0_decode:decode(Input)).

decode_xml_document_test() ->
    Input = read_testdata("amf0-xml-doc.bin"),
    Expected = amf:xml_document(<<"<parent><child prop=\"test\" /></parent>">>),
    ?assertEqual({ok, Expected, <<>>}, amf0_decode:decode(Input)).

decode_xml_document_partial_test() ->
    Input = read_testdata("amf0-xml-document-partial.bin"),
    ?assertMatch({error, #amf_exception{type=partial, message={xml_document,_}}}, amf0_decode:decode(Input)).
    
decode_typed_object_test() ->
    Input = read_testdata("amf0-typed-object.bin"),
    Expected = amf:typed_object(<<"org.amf.ASClass">>,
                                [{<<"foo">>, <<"bar">>},
                                 {<<"baz">>, null}]),
    ?assertEqual({ok, Expected, <<>>}, amf0_decode:decode(Input)).

decode_typed_object_partial_test() ->
    Input = read_testdata("amf0-typed-object-partial.bin"),
    ?assertMatch({error,#amf_exception{type=partial, message={typed_object,_}}}, amf0_decode:decode(Input)).

decode_avmplus_object_test_() ->
    {setup,
     fun () -> meck:new(amf3_decode, [no_passthrough_cover]) end,
     fun (_) -> meck:unload(amf3_decode) end,
     [
      fun () ->
              meck:expect(amf3_decode, unsafe_decode, 1, {dummy_value, <<"">>}),
              
              Input = read_testdata("amf0-avmplus-object.bin"),
              Expected = dummy_value,
              ?assertEqual({ok, Expected, <<>>}, amf0_decode:decode(Input))
      end
     ]}.

decode_empty_binary_test() ->
    Input = read_testdata("amf0-empty.bin"),
    ?assertMatch({error, #amf_exception{type=partial, message={marker,_}}}, amf0_decode:decode(Input)).

decode_unknown_marker_test() ->
    Input = read_testdata("amf0-unknown-marker.bin"),
    ?assertMatch({error, #amf_exception{type=invalid, message={unknown_marker,_}}}, amf0_decode:decode(Input)).

decode_missing_object_end_test() ->
    Input = read_testdata("amf0-missing-object-end.bin"),
    ?assertMatch({error, #amf_exception{type=invalid, message={missing_object_end,_}}}, amf0_decode:decode(Input)).

decode_object_partial_test() ->
    Input = read_testdata("amf0-object-partial.bin"),
    ?assertMatch({error, #amf_exception{type=partial, message={kv_pairs,_}}}, amf0_decode:decode(Input)).

-ifdef(BENCH).
decode_speed_test() ->
    case cover:is_compiled(amf0_decode) of
        false ->
            Files = ["amf0-complex-encoded-string.bin",
                     "amf0-ref-test.bin",
                     "amf0-ecma-ordinal-array.bin",
                     "amf0-typed-object.bin",
                     "amf0-strict-array.bin"],
            Bins = [read_testdata(File) || File <- Files],
            N = lists:seq(1, 50000),
            ?debugTime("amf0_decode",
                       lists:foreach(fun(_) -> 
                                             lists:foreach(fun amf0_decode:decode/1, Bins)
                                     end, N));
        _ ->
            skip
    end.
-endif.
