-module(amf3_encode_tests).

-include("../include/amf.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(UTF8(S), unicode:characters_to_binary(S)).

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
            ?assertMatch({ok, _}, amf3_encode:encode(Input)),
            {ok, EncodedIoList} = amf3_encode:encode(Input),

            ?assertEqual(Expected, list_to_binary(EncodedIoList))
        end).

-define(assertEncode(Expected, Input),
        begin
            ?assertMatch({ok, _}, amf3_encode:encode(Input)),
            {ok, EncodedIoList} = amf3_encode:encode(Input),

            ExpectedResult = amf3_decode:decode(Expected),
            ?assertMatch(ExpectedResult, amf3_decode:decode(list_to_binary(EncodedIoList)))
        end).

-define(assertEncodeError(Expected, Input),
        begin
            ?assertMatch({error, Expected}, amf3_encode:encode(Input))
        end).

%% Test Functions
encode_undefined_test() ->
    Expected = read_testdata("amf3-undefined.bin"),
    Input = undefined,
    ?assertEncodeBin(Expected, Input).

encode_null_test() ->
    Expected = read_testdata("amf3-null.bin"),
    Input = null,
    ?assertEncodeBin(Expected, Input).

encode_false_test() ->
    Expected = read_testdata("amf3-false.bin"),
    Input = false,
    ?assertEncodeBin(Expected, Input).

encode_true_test() ->
    Expected = read_testdata("amf3-true.bin"),
    Input = true,
    ?assertEncodeBin(Expected, Input).

encode_integer_0_test() ->
    Expected = read_testdata("amf3-0.bin"),
    Input = 0,
    ?assertEncodeBin(Expected, Input).

encode_integer_2byte_test() ->
    Expected = read_testdata("amf3-integer-2byte.bin"),
    Input = 2#10000000,
    ?assertEncodeBin(Expected, Input).

encode_integer_3byte_test() ->
    Expected = read_testdata("amf3-integer-3byte.bin"),
    Input = 2#100000000000000,
    ?assertEncodeBin(Expected, Input).

encode_integer_min_test() ->
    Expected = read_testdata("amf3-min.bin"),
    Input = -16#10000000,
    ?assertEncodeBin(Expected, Input).

encode_integer_max_test() ->
    Expected = read_testdata("amf3-max.bin"),
    Input = 16#FFFFFFF,
    ?assertEncodeBin(Expected, Input).

encode_double_test() ->
    Expected = read_testdata("amf3-float.bin"),
    Input = 3.5,
    ?assertEncodeBin(Expected, Input).

encode_double_bignum_test() ->
    Expected = read_testdata("amf3-bignum.bin"),
    Input = math:pow(2, 1000),
    ?assertEncodeBin(Expected, Input).

encode_double_large_min_test() ->
    Expected = read_testdata("amf3-large-min.bin"),
    Input = -(16#10000000+1),
    ?assertEncodeBin(Expected, Input).

encode_double_large_max_test() ->
    Expected = read_testdata("amf3-large-max.bin"),
    Input = 16#FFFFFFF+1,
    ?assertEncodeBin(Expected, Input).

encode_string_test() ->
    Expected = read_testdata("amf3-string.bin"),
    Input = <<"String . String">>,
    ?assertEncodeBin(Expected, Input).



encode_string_ref_test() ->
    Expected = read_testdata("amf3-string-ref.bin"),
    Input = [<<"foo">>,
             <<"str">>,
             <<"foo">>,
             <<"str">>,
             <<"foo">>,
             amf:object([{<<"str">>, <<"foo">>}])],
    ?assertEncode(Expected, Input).

encode_encoded_string_ref_test() ->
    Expected = read_testdata("amf3-encoded-string-ref.bin"),
    Input = [?UTF8("this is a テスト"), ?UTF8("this is a テスト")],
    ?assertEncode(Expected, Input).

encode_complex_encoded_string_array_test() ->
    Expected = read_testdata("amf3-complex-encoded-string-array.bin"),
    Input = [5, ?UTF8("Shift テスト"), ?UTF8("UTF テスト"), 5],
    ?assertEncodeBin(Expected, Input).

encode_empty_string_ref_test() ->
    Expected = read_testdata("amf3-empty-string-ref.bin"),
    Input = [<<"">>, <<"">>],
    ?assertEncodeBin(Expected, Input).

encode_primitive_array_test() ->
    Expected = read_testdata("amf3-primitive-array.bin"),
    Input = [1,2,3,4,5],
    ?assertEncodeBin(Expected, Input).

encode_empty_array_test() ->
    Expected = read_testdata("amf3-empty-array.bin"),
    Input = [],
    ?assertEncodeBin(Expected, Input).

encode_empty_array_ref_test() ->
    Expected = read_testdata("amf3-empty-array-ref.bin"),
    Input = [[],[],[],[]],
    ?assertEncode(Expected, Input).

encode_array_ref_test() ->
    Expected = read_testdata("amf3-array-ref.bin"),
    Input = [[1,2,3],
             [<<"a">>,<<"b">>,<<"c">>],
             [1,2,3],
             [<<"a">>,<<"b">>,<<"c">>]],
    ?assertEncode(Expected, Input).

encode_associative_array_test() ->
    Expected = read_testdata("amf3-associative-array.bin"),
    Input = amf:array([<<"bar">>, <<"bar1">>, <<"bar2">>],
                      [{<<"2">>, <<"bar3">>},
                       {<<"foo">>, <<"bar">>},
                       {<<"asdf">>, <<"fdsa">>}]),
    ?assertEncode(Expected, Input).

encode_mixed_array_test() ->
    Expected = read_testdata("amf3-mixed-array.bin"),

    H1 = amf:object([{<<"foo_one">>,<<"bar_one">>}]),
    H2 = amf:object([{<<"foo_two">>,<<>>}]),
    SO1= amf:object([{<<"foo_three">>,42}]),
    Empty = amf:object([]),
    Input = [H1, H2, SO1, Empty, [H1, H2, SO1], [], 42, <<"">>, [], <<"">>, Empty, <<"bar_one">>, SO1],

    ?assertEncode(Expected, Input).

encode_object_ref_test() ->
    Expected = read_testdata("amf3-object-ref.bin"),
    Obj = amf:object([{<<"foo">>, <<"bar">>}]),
    Input = [[Obj,Obj], <<"bar">>, [Obj,Obj]],

    ?assertEncode(Expected, Input).

enode_dynamic_object_test() ->
    Expected = read_testdata("amf3-dynamic-object.bin"),
    Input = amf:object([{<<"property_one">>, <<"foo">>},
                        {<<"another_public_property">>, <<"a_public_value">>},
                        {<<"nil_property">>, null}]),
    ?assertEncodeBin(Expected, Input).

encode_typed_object_test() ->
    Expected = read_testdata("amf3-typed-object.bin"),
    Input = amf:typed_object(<<"org.amf.ASClass">>, [{<<"foo">>,<<"bar">>}, {<<"baz">>, null}]),
    ?assertEncodeBin(Expected, Input).

encode_trait_ref_test() ->
    Expected = read_testdata("amf3-trait-ref.bin"),
    Input = [amf:typed_object(<<"org.amf.ASClass">>, [{<<"foo">>,<<"foo">>}, {<<"baz">>,null}]),
             amf:typed_object(<<"org.amf.ASClass">>, [{<<"foo">>,<<"bar">>}, {<<"baz">>,null}])],
    ?assertEncode(Expected, Input).

enode_symbol_test() ->
    Expected = read_testdata("amf3-symbol.bin"),
    Input = <<"foo">>,
    ?assertEncodeBin(Expected, Input).

encode_hash_test() ->
    Expected = read_testdata("amf3-hash.bin"),
    Input = amf:object([{<<"foo">>, <<"bar">>},
                        {<<"answer">>, 42}]),
    ?assertEncodeBin(Expected, Input).

encode_xml_doc_test() ->
    Expected = read_testdata("amf3-xml-doc.bin"),
    Input = amf:xml_document(<<"<parent><child prop=\"test\" /></parent>">>),
    ?assertEncodeBin(Expected, Input).

encode_xml_test() ->
    Expected = read_testdata("amf3-xml.bin"),
    Input = amf:xml(<<"<parent><child prop=\"test\"/></parent>">>),
    ?assertEncodeBin(Expected, Input).

encode_xml_ref_test() ->
    Expected = read_testdata("amf3-xml-ref.bin"),
    Input = [amf:xml(<<"<parent><child prop=\"test\"/></parent>">>),
             amf:xml(<<"<parent><child prop=\"test\"/></parent>">>)],
    ?assertEncode(Expected, Input).

encode_byte_array_test() ->
    Expected = read_testdata("amf3-byte-array.bin"),
    Input = amf:byte_array(<<0,3,(?UTF8("これtest"))/binary,64>>),
    ?assertEncodeBin(Expected, Input).

encode_byte_array_ref_test() ->
    Expected = read_testdata("amf3-byte-array-ref.bin"),
    Input = [amf:byte_array(<<"ASDF">>),
             amf:byte_array(<<"ASDF">>)],
    ?assertEncode(Expected, Input).

encode_date_test() ->
    Expected = read_testdata("amf3-date.bin"),
    Input = amf:date({0,0,0}),
    ?assertEncodeBin(Expected, Input).

encode_date_ref_test() ->
    Expected = read_testdata("amf3-date-ref.bin"),
    Input = [amf:date({0,0,0}), amf:date({0,0,0})],
    ?assertEncode(Expected, Input).

encode_vector_int_test() ->
    Expected = read_testdata("amf3-vector-int.bin"),
    Input = amf:vector(int, [4,-20,12]),
    ?assertEncodeBin(Expected, Input).

encode_vector_uint_test() ->
    Expected = read_testdata("amf3-vector-uint.bin"),
    Input = amf:vector(uint, [4,20,12]),
    ?assertEncodeBin(Expected, Input).

encode_vector_double_test() ->
    Expected = read_testdata("amf3-vector-double.bin"),
    Input = amf:vector(double, [4.3,-20.6]),
    ?assertEncodeBin(Expected, Input).

encode_vector_object_test() ->
    Expected = read_testdata("amf3-vector-object.bin"),

    Foo = amf:typed_object(<<"org.amf.ASClass">>, [{<<"foo">>,<<"foo">>}, {<<"baz">>, null}]),
    Bar = amf:typed_object(<<"org.amf.ASClass">>, [{<<"foo">>,<<"bar">>}, {<<"baz">>, null}]),
    Baz = amf:typed_object(<<"org.amf.ASClass">>, [{<<"foo">>,<<"baz">>}, {<<"baz">>, null}]),
    Input = amf:vector(<<"org.amf.ASClass">>, [Foo, Bar, Baz]),
    ?assertEncode(Expected, Input).

encode_dictionary_test() ->
    Expected = read_testdata("amf3-dictionary.bin"),
    Input = amf:dictionary(
              [{<<"bar">>, <<"asdf1">>},
               {amf:typed_object(<<"org.amf.ASClass">>,[{<<"foo">>,<<"baz">>},{<<"baz">>,null}]), <<"asdf2">>}]),
    ?assertEncode(Expected, Input).

encode_empty_dictionary_test() ->
    Expected = read_testdata("amf3-empty-dictionary.bin"),
    Input = amf:dictionary([]),
    ?assertEncodeBin(Expected, Input).

encode_unknown_test() ->
    Input = {1,2,3},
    ?assertEncodeError(#amf_exception{type=unsupported, message={value,Input}}, Input).
