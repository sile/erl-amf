-module(amf3_decode_tests).

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

%% Test Functions
decode_undefined_test() ->
    Input = read_testdata("amf3-undefined.bin"),
    Expected = undefined,
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_null_test() ->
    Input = read_testdata("amf3-null.bin"),
    Expected = null,
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_false_test() ->
    Input = read_testdata("amf3-false.bin"),
    Expected = false,
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_true_test() ->
    Input = read_testdata("amf3-true.bin"),
    Expected = true,
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_integer_0_test() ->
    Input = read_testdata("amf3-0.bin"),
    Expected = 0,
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)). 

decode_integer_min_test() ->
    Input = read_testdata("amf3-min.bin"),
    Expected = -16#10000000, 
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)). 

decode_integer_max_test() ->
    Input = read_testdata("amf3-max.bin"),
    Expected = 16#FFFFFFF,
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)). 

decode_double_test() ->    
    Input = read_testdata("amf3-float.bin"),
    Expected = 3.5,
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_double_bignum_test() ->    
    Input = read_testdata("amf3-bignum.bin"),
    Expected = math:pow(2, 1000),
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_double_large_min_test() ->
    Input = read_testdata("amf3-large-min.bin"),
    Expected = -16#10000000 - 1.0,
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_double_large_max_test() ->
    Input = read_testdata("amf3-large-max.bin"),
    Expected = 16#FFFFFFF + 1.0,
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_string_test() ->    
    Input = read_testdata("amf3-string.bin"),
    Expected = <<"String . String">>,
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_string_ref_test() ->    
    Input = read_testdata("amf3-string-ref.bin"),
    Expected = [<<"foo">>,
                <<"str">>,
                <<"foo">>,
                <<"str">>,
                <<"foo">>,
                amf:object([{<<"str">>, <<"foo">>}])],
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_encoded_string_ref_test() ->    
    Input = read_testdata("amf3-encoded-string-ref.bin"),
    Expected = [<<"this is a テスト">>, <<"this is a テスト">>],
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_complex_encoded_string_array_test() ->    
    Input = read_testdata("amf3-complex-encoded-string-array.bin"),
    Expected = [5, <<"Shift テスト">>, <<"UTF テスト">>, 5],
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_empty_string_ref_test() ->    
    Input = read_testdata("amf3-empty-string-ref.bin"),
    Expected = [<<"">>, <<"">>],
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_primitive_array_test() ->
    Input = read_testdata("amf3-primitive-array.bin"),
    Expected = [1,2,3,4,5],
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_empty_array_test() ->
    Input = read_testdata("amf3-empty-array.bin"),
    Expected = [],
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_empty_array_ref_test() ->
    Input = read_testdata("amf3-empty-array-ref.bin"),
    Expected = [[],[],[],[]],
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_array_ref_test() ->
    Input = read_testdata("amf3-array-ref.bin"),
    Expected = [[1,2,3],
                [<<"a">>,<<"b">>,<<"c">>],
                [1,2,3],
                [<<"a">>,<<"b">>,<<"c">>]],
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_associative_array_test() ->
    Input = read_testdata("amf3-associative-array.bin"),
    Expected = amf:array([<<"bar">>, <<"bar1">>, <<"bar2">>],
                         [{<<"2">>, <<"bar3">>},
                          {<<"foo">>, <<"bar">>},
                          {<<"asdf">>, <<"fdsa">>}]),
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_mixed_array_test() ->
    Input = read_testdata("amf3-mixed-array.bin"),
    
    H1 = amf:object([{<<"foo_one">>,<<"bar_one">>}]),
    H2 = amf:object([{<<"foo_two">>,<<>>}]),
    SO1= amf:object([{<<"foo_three">>,42}]),
    Empty = amf:object([]),
    Expected = [H1, H2, SO1, Empty, [H1, H2, SO1], [], 42, <<"">>, [], <<"">>, Empty, <<"bar_one">>, SO1],

    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_object_ref_test() ->
    Input = read_testdata("amf3-object-ref.bin"),
    Obj = amf:object([{<<"foo">>, <<"bar">>}]),
    Expected = [[Obj,Obj], <<"bar">>, [Obj,Obj]],
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_dynamic_object_test() ->
    Input = read_testdata("amf3-dynamic-object.bin"),
    Expected = amf:object([{<<"property_one">>, <<"foo">>},
                           {<<"another_public_property">>, <<"a_public_value">>},
                           {<<"nil_property">>, null}]),
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_typed_object_test() ->
    Input = read_testdata("amf3-typed-object.bin"),
    Expected = amf:typed_object(<<"org.amf.ASClass">>, [{<<"foo">>,<<"bar">>}, {<<"baz">>, null}]),
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_trait_ref_test() ->
    Input = read_testdata("amf3-trait-ref.bin"),
    Expected = [amf:typed_object(<<"org.amf.ASClass">>, [{<<"foo">>,<<"foo">>}, {<<"baz">>,null}]),
                amf:typed_object(<<"org.amf.ASClass">>, [{<<"foo">>,<<"bar">>}, {<<"baz">>,null}])],
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_externalizable_test() ->
    Input = read_testdata("amf3-externalizable.bin"),
    ?assertMatch({error, #amf_exception{type=unsupported, message={object_traits_ext, _, _}}}, amf3_decode:decode(Input)).

decode_symbol_test() ->
    Input = read_testdata("amf3-symbol.bin"),
    Expected = <<"foo">>,
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_hash_test() ->
    Input = read_testdata("amf3-hash.bin"),
    Expected = amf:object([{<<"foo">>, <<"bar">>},
                           {<<"answer">>, 42}]),
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_xml_doc_test() ->
    Input = read_testdata("amf3-xml-doc.bin"),
    Expected = amf:xml_document(<<"<parent><child prop=\"test\" /></parent>">>),
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_date_test() ->
    Input = read_testdata("amf3-date.bin"),
    Expected = amf:date({0,0,0}),
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_date_ref_test() ->
    Input = read_testdata("amf3-date-ref.bin"),
    Expected = [amf:date({0,0,0}), amf:date({0,0,0})],
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_xml_test() ->
    Input= read_testdata("amf3-xml.bin"),
    Expected = amf:xml(<<"<parent><child prop=\"test\"/></parent>">>),
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_xml_ref_test() ->
    Input = read_testdata("amf3-xml-ref.bin"),
    Expected = [amf:xml(<<"<parent><child prop=\"test\"/></parent>">>),
                amf:xml(<<"<parent><child prop=\"test\"/></parent>">>)],
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_byte_array_test() ->    
    Input = read_testdata("amf3-byte-array.bin"),
    Expected = amf:byte_array(<<0,3,"これtest",64>>),
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_byte_array_ref_test() ->    
    Input = read_testdata("amf3-byte-array-ref.bin"),
    Expected = [amf:byte_array(<<"ASDF">>),
                amf:byte_array(<<"ASDF">>)],
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_vector_int_test() ->    
    Input = read_testdata("amf3-vector-int.bin"),
    Expected = amf:vector(int, [4,-20,12]),
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_vector_uint_test() ->    
    Input = read_testdata("amf3-vector-uint.bin"),
    Expected = amf:vector(uint, [4,20,12]),
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_vector_double_test() ->
    Input = read_testdata("amf3-vector-double.bin"),
    Expected = amf:vector(double, [4.3,-20.6]),
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_vector_object_test() ->
    Input = read_testdata("amf3-vector-object.bin"),

    Foo = amf:typed_object(<<"org.amf.ASClass">>, [{<<"foo">>,<<"foo">>}, {<<"baz">>, null}]),
    Bar = amf:typed_object(<<"org.amf.ASClass">>, [{<<"foo">>,<<"bar">>}, {<<"baz">>, null}]),
    Baz = amf:typed_object(<<"org.amf.ASClass">>, [{<<"foo">>,<<"baz">>}, {<<"baz">>, null}]),
    Expected = amf:vector(<<"org.amf.ASClass">>, [Foo, Bar, Baz]),
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_array_collection_test() ->
    Input = read_testdata("amf3-array-collection.bin"),
    ?assertMatch({error, #amf_exception{type=unsupported, message={object_traits_ext, _, _}}}, amf3_decode:decode(Input)).

decode_dictionary_test() ->
    Input = read_testdata("amf3-dictionary.bin"),
    Expected = amf:dictionary(
                 [{<<"bar">>, <<"asdf1">>},
                  {amf:typed_object(<<"org.amf.ASClass">>,[{<<"foo">>,<<"baz">>},{<<"baz">>,null}]),
                   <<"asdf2">>}]),
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_empty_dictionary_test() ->
    Input = read_testdata("amf3-empty-dictionary.bin"),
    Expected = amf:dictionary([]),
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_graph_member_test() ->    
    Input = read_testdata("amf3-graph-member.bin"),
    ?assertMatch({error, #amf_exception{type=unsupported, message={circular_reference,_}}}, amf3_decode:decode(Input)).

decode_bad_object_ref_test() ->
    Input = read_testdata("amf3-bad-object-ref.bin"),
    ?assertMatch({error, #amf_exception{type=invalid, message={undefined_object_reference,_}}}, amf3_decode:decode(Input)).

decode_bad_trait_ref_test() ->
    Input = read_testdata("amf3-bad-trait-ref.bin"),
    ?assertMatch({error, #amf_exception{type=invalid, message={undefined_trait_reference,_}}}, amf3_decode:decode(Input)).

decode_unknown_marker_test() ->
    Input = read_testdata("amf3-unknown-marker.bin"),
    ?assertMatch({error, #amf_exception{type=invalid, message={unknown_marker,_}}}, amf3_decode:decode(Input)).

decode_empty_test() ->
    Input = read_testdata("amf3-empty.bin"),
    ?assertMatch({error, #amf_exception{type=partial, message={marker,_}}}, amf3_decode:decode(Input)).

decode_double_positive_infinity_test() ->
    Input = read_testdata("amf3-double-positive-infinity.bin"),
    ?assertMatch({error, #amf_exception{type=unsupported, message={double,_}}}, amf3_decode:decode(Input)).

decode_double_partial_test() ->
    Input = read_testdata("amf3-double-partial.bin"),
    ?assertMatch({error, #amf_exception{type=partial, message={double,_}}}, amf3_decode:decode(Input)).

decode_date_invalid_millis_test() ->
    Input = read_testdata("amf3-date-invalid-millis.bin"),
    ?assertMatch({error, #amf_exception{type=invalid, message={date,_}}}, amf3_decode:decode(Input)).

decode_date_minus_millis_test() ->
    Input = read_testdata("amf3-date-minus-millis.bin"),
    ?assertMatch({error, #amf_exception{type=invalid, message={negative_date,_}}}, amf3_decode:decode(Input)).

decode_date_partial_test() ->
    Input = read_testdata("amf3-date-partial.bin"),
    ?assertMatch({error, #amf_exception{type=partial, message={date,_}}}, amf3_decode:decode(Input)).

decode_dictionary_partial_test() ->
    Input = read_testdata("amf3-dictionary-partial.bin"),
    ?assertMatch({error, #amf_exception{type=partial, message={dictionary,_,_}}}, amf3_decode:decode(Input)).

decode_vector_partial_test() ->
    Input = read_testdata("amf3-vector-partial.bin"),
    ?assertMatch({error, #amf_exception{type=partial, message={vector,_,_,_}}}, amf3_decode:decode(Input)).

decode_vector_int_partial_test() ->
    Input = read_testdata("amf3-vector-int-partial.bin"),
    ?assertMatch({error, #amf_exception{type=partial, message={int,_}}}, amf3_decode:decode(Input)).

decode_vector_uint_partial_test() ->
    Input = read_testdata("amf3-vector-uint-partial.bin"),
    ?assertMatch({error, #amf_exception{type=partial, message={uint,_}}}, amf3_decode:decode(Input)).

decode_xml_partial_test() ->
    Input = read_testdata("amf3-xml-partial.bin"),
    ?assertMatch({error, #amf_exception{type=partial, message={xml,_,_}}}, amf3_decode:decode(Input)).

decode_string_partial_test() ->
    Input = read_testdata("amf3-string-partial.bin"),
    ?assertMatch({error, #amf_exception{type=partial, message={utf8_vr,_,_}}}, amf3_decode:decode(Input)).

decode_bad_string_ref_test() ->
    Input = read_testdata("amf3-bad-string-ref.bin"),
    ?assertMatch({error, #amf_exception{type=invalid, message={undefined_string_reference,_}}}, amf3_decode:decode(Input)).

decode_u29_partial_test() ->
    Input = read_testdata("amf3-u29-partial.bin"),
    ?assertMatch({error, #amf_exception{type=partial, message={u29,_}}}, amf3_decode:decode(Input)).

decode_integer_2byte_test() ->    
    Input = read_testdata("amf3-integer-2byte.bin"),
    Expected = 2#10000000,
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

decode_integer_3byte_test() ->    
    Input = read_testdata("amf3-integer-3byte.bin"),
    Expected = 2#100000000000000,
    ?assertEqual({ok, Expected, <<>>}, amf3_decode:decode(Input)).

%% Performance Test
-ifdef(BENCH).
decode_speed_test() ->
    case cover:is_compiled(amf3_decode) of
        false ->
            Files = ["amf3-complex-encoded-string-array.bin",
                     "amf3-associative-array.bin",
                     "amf3-mixed-array.bin",
                     "amf3-trait-ref.bin"],
            Bins = [read_testdata(File) || File <- Files],
            N = lists:seq(1, 5000),
            ?debugTime("amf3_decode",
                       lists:foreach(fun(_) -> 
                                             lists:foreach(fun amf3_decode:decode/1, Bins)
                                     end, N));
        _ ->
            skip
    end.
-endif.
