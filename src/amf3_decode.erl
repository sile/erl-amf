%%% @doc AMF3 decode API
%%% @reference AMF3 specification: [http://wwwimages.adobe.com/www.adobe.com/content/dam/Adobe/en/devnet/amf/pdf/amf-file-format-spec.pdf]
%%% @private
%%% @end
%%%
%%%
%%% Copyright (c) 2013, Takeru Ohta <phjgt308@gmail.com>
%%%
%%% The MIT License
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%---------------------------------------------------------------------------------------
-module(amf3_decode).
-compile(inline).
-include("../include/amf.hrl").
-include("../include/internal/amf_internal.hrl").

%% Decode API
-export([decode/1, unsafe_decode/1]).

%% Record
-record(refmap,
        {
          str   = ?DECODE_REFERENCE_TABLE_INIT() :: decode_reference_table(), % for string reference
          obj   = ?DECODE_REFERENCE_TABLE_INIT() :: decode_reference_table(), % for object reference
          trait = ?DECODE_REFERENCE_TABLE_INIT() :: decode_reference_table()  % for trait reference
        }).

%% Internal Types
-type refmap_index()               :: decode_reference_table_index().
-type refmap()                     :: #refmap{}.
-type refmap_get_result(ValueType) :: none|uninitialized|{value, ValueType}.

-type decode_fn()        :: fun((binary(), refmap())                                 -> internal_result(amf:amf_value())).
-type decode_object_fn() :: fun((UnsignedInt28::[0..16#FFFFFFF], binary(), refmap()) -> internal_result(amf:amf_value())).

-type object_trait() :: {amf:amf_class_name(), IsDynamic::boolean(), [amf:amf_string()]}.

-type internal_result(ValueType) :: {ValueType, binary(), refmap()}.


%% External Functions

%% @doc Decode AMF3 binary data.
-spec decode(EncodedBytes) -> {ok, DecodedValue, UnconsumedBytes} | {error, Reason} when 
      EncodedBytes    :: binary(),
      DecodedValue    :: amf:amf_value(),
      UnconsumedBytes :: binary(),
      Reason          :: amf:amf_exception().
decode(<<Bin/binary>>) ->
    try unsafe_decode(Bin) of
        {Value, UnconsumedBytes} -> {ok, Value, UnconsumedBytes}
    catch
        throw:#amf_exception{}=Ex -> {error, Ex}
    end.

%% @doc Decode AMF3 binary data (maybe throw exception).
%%
%% This function throws exception if the input data could not be decoded correctly.
%%
%% @throws amf:amf_exception()
-spec unsafe_decode(EncodedBytes) -> {DecodedValue, UnconsumedBytes} when
      EncodedBytes    :: binary(),
      DecodedValue    :: amf:amf_value(),
      UnconsumedBytes :: binary().
unsafe_decode(<<Bin/binary>>) ->
    {Value, UnconsumedBytes, _RefMap} = decode_impl(Bin, refmap_init()),
    {Value, UnconsumedBytes}.

%% Internal Functions for Decode
-spec decode_impl(binary(), refmap()) -> internal_result(amf:amf_value()).
decode_impl(<<Marker:8, Bin/binary>>, RefMap) ->
    case Marker of
        ?AMF3_UNDEFINED_MARKER     -> decode_undefined(Bin, RefMap);
        ?AMF3_NULL_MARKER          -> decode_null(Bin, RefMap);
        ?AMF3_FALSE_MARKER         -> decode_false(Bin, RefMap);
        ?AMF3_TRUE_MARKER          -> decode_true(Bin, RefMap);
        ?AMF3_INTEGER_MARKER       -> decode_integer(Bin, RefMap);
        ?AMF3_DOUBLE_MARKER        -> decode_double(Bin, RefMap);
        ?AMF3_STRING_MARKER        -> decode_string(Bin, RefMap);
        ?AMF3_ARRAY_MARKER         -> decode_array(Bin, RefMap);
        ?AMF3_OBJECT_MARKER        -> decode_object(Bin, RefMap);
        ?AMF3_XML_DOC_MARKER       -> decode_xml_doc(Bin, RefMap);
        ?AMF3_DATE_MARKER          -> decode_date(Bin, RefMap);
        ?AMF3_XML_MARKER           -> decode_xml(Bin, RefMap);
        ?AMF3_BYTE_ARRAY_MARKER    -> decode_byte_array(Bin, RefMap);
        ?AMF3_VECTOR_INT_MARKER    -> decode_vector_int(Bin, RefMap);
        ?AMF3_VECTOR_UINT_MARKER   -> decode_vector_uint(Bin, RefMap);
        ?AMF3_VECTOR_DOUBLE_MARKER -> decode_vector_double(Bin, RefMap);
        ?AMF3_VECTOR_OBJECT_MARKER -> decode_vector_object(Bin, RefMap);
        ?AMF3_DICTIONARY_MARKER    -> decode_dictionary(Bin, RefMap);
        _                          -> decode_unknown(Bin, RefMap, Marker)
    end;
decode_impl(<<Bin/binary>>, _RefMap) -> ?THROW_PARTIAL({marker, Bin}).

-spec decode_unknown(binary(), refmap(), non_neg_integer()) -> no_return().
decode_unknown(_Bin, _RefMap, Marker) -> ?THROW_INVALID({unknown_marker, Marker}).

-spec decode_undefined(binary(), refmap()) -> internal_result(amf:amf_undefined()).
decode_undefined(<<Bin/binary>>, RefMap)   -> {undefined, Bin, RefMap}.

-spec decode_null(binary(), refmap()) -> internal_result(amf:amf_null()).
decode_null(<<Bin/binary>>, RefMap)   -> {null, Bin, RefMap}.

-spec decode_false(binary(), refmap()) -> internal_result(amf:amf_boolean()).
decode_false(<<Bin/binary>>, RefMap)   -> {false, Bin, RefMap}.

-spec decode_true(binary(), refmap()) -> internal_result(amf:amf_boolean()).
decode_true(<<Bin/binary>>, RefMap)   -> {true, Bin, RefMap}.

-spec decode_integer(binary(), refmap()) -> internal_result(amf:amf_number()).
decode_integer(<<Bin/binary>>, RefMap) ->
    {<<Num:29/signed>>, Bin1} = decode_u29(Bin),
    {Num, Bin1, RefMap}.

-spec decode_double(binary(), refmap()) -> internal_result(amf:amf_number()).
decode_double(<<N/float, Bin/binary>>,     RefMap) -> {N, Bin, RefMap};
decode_double(<<N:8/binary,_Bin/binary>>, _RefMap) -> ?THROW_UNSUPPORTED({double, N});
decode_double(<<Bin/binary>>, _)                   -> ?THROW_PARTIAL({double, Bin}).

-spec decode_string(binary(), refmap()) -> internal_result(amf:amf_string()).
decode_string(<<Bin/binary>>, RefMap)   -> decode_utf8_vr(Bin, RefMap).

-spec decode_array(binary(), refmap()) -> internal_result(amf:amf_array()|amf:amf_strict_array()).
decode_array(<<Bin/binary>>, RefMap) ->
    with_object_reference(
      Bin, RefMap,
      fun (DenseElementCount, Bin1, RefMap1) ->
              {Members, Bin2, RefMap2} = decode_kv_pairs(Bin1, RefMap1, []),
              {Values, Bin3, RefMap3} = decode_sequence(Bin2, RefMap2, DenseElementCount, fun decode_impl/2, []),
              case Members of
                  [] -> {Values, Bin3, RefMap3};                    % return amf:amf_strict_array()
                  _  -> {amf:array(Values, Members), Bin3, RefMap3} % return amf:amf_array()
              end
      end).

-spec decode_object(binary(), refmap()) -> internal_result(amf:amf_object()).
decode_object(<<Bin/binary>>, RefMap) ->
    with_object_reference(
      Bin, RefMap,
      fun (U28, Bin1, RefMap1) ->
              {Trait, Bin2, RefMap2} = decode_trait(<<U28:28>>, Bin1, RefMap1),
              {ClassName, IsDynamic, Fields} = Trait,
              {Values, Bin3, RefMap3} = decode_sequence(Bin2, RefMap2, length(Fields), fun decode_impl/2, []),
              {DynamicMembers, Bin4, RefMap4} =
                  case IsDynamic of
                      false -> {[], Bin3, RefMap3};
                      true  -> decode_kv_pairs(Bin3, RefMap3, [])
                  end,
              Members = lists:zip(Fields, Values) ++ DynamicMembers,
              Object = amf:object(Members, [{class,ClassName}, {dynamic,IsDynamic}, {sealed_fields, Fields}]),
              {Object, Bin4, RefMap4}
      end).

-spec decode_date(binary(), refmap()) -> internal_result(amf:amf_date()).
decode_date(<<Bin/binary>>, RefMap) ->
    with_object_reference(
      Bin, RefMap,
      fun (_U28, <<MilliSecs/float, Bin1/binary>>, RefMap1) -> 
              case MilliSecs < 0 of
                  false -> {amf:msec_to_date(trunc(MilliSecs)), Bin1, RefMap1};
                  true  -> ?THROW_INVALID({negative_date, MilliSecs})
              end;
          (_U28, <<DateBin:8/binary,_Bin1/binary>>, _RefMap1) -> ?THROW_INVALID({date, DateBin});
          (_U28, <<Bin1/binary>>, _RefMap1)                   -> ?THROW_PARTIAL({date, Bin1})
      end).

-spec decode_xml_doc(binary(), refmap()) -> internal_result(amf:amf_xml_document()).
decode_xml_doc(<<Bin/binary>>, RefMap)   -> decode_string_wrapper_object(Bin, RefMap, xml_document).

-spec decode_xml(binary(), refmap()) -> internal_result(amf:amf_xml()).
decode_xml(<<Bin/binary>>, RefMap)   -> decode_string_wrapper_object(Bin, RefMap, xml).

-spec decode_byte_array(binary(), refmap()) -> internal_result(amf:amf_byte_array()).
decode_byte_array(<<Bin/binary>>, RefMap)   -> decode_string_wrapper_object(Bin, RefMap, byte_array).

-spec decode_vector_int(binary(), refmap()) -> internal_result(amf:amf_vector()).
decode_vector_int(<<Bin/binary>>, RefMap)   -> decode_vector(Bin, RefMap, int, fun decode_int/2).

-spec decode_vector_uint(binary(), refmap()) -> internal_result(amf:amf_vector()).
decode_vector_uint(<<Bin/binary>>, RefMap)   -> decode_vector(Bin, RefMap, uint, fun decode_uint/2).

-spec decode_vector_double(binary(), refmap()) -> internal_result(amf:amf_vector()).
decode_vector_double(<<Bin/binary>>, RefMap)   -> decode_vector(Bin, RefMap, double, fun decode_double/2).

-spec decode_vector_object(binary(), refmap()) -> internal_result(amf:amf_vector()).
decode_vector_object(<<Bin/binary>>, RefMap)   -> decode_vector(Bin, RefMap, object, fun decode_impl/2).

-spec decode_dictionary(binary(), refmap()) -> internal_result(amf:amf_dictionary()).
decode_dictionary(<<Bin/binary>>, RefMap) ->
    with_object_reference(
      Bin, RefMap,
      fun (Count, <<IsWeak:8, Bin1/binary>>, RefMap1) ->
              {Members, Bin2, RefMap2} = decode_dictionary_entries(Bin1, RefMap1, Count, []),
              {amf:dictionary(Members, [{weak,IsWeak=:=1}]), Bin2, RefMap2};
          (Count, <<Bin1/binary>>, _RefMap1) ->
              ?THROW_PARTIAL({dictionary, Count, Bin1})
      end).

-spec decode_dictionary_entries(binary(), refmap(), non_neg_integer(), [amf:amf_dictionary_entry()]) -> 
                                       internal_result([amf:amf_dictionary_entry()]).
decode_dictionary_entries(<<Bin/binary>>, RefMap, 0, Acc) ->
    {lists:reverse(Acc), Bin, RefMap};
decode_dictionary_entries(<<Bin/binary>>, RefMap, Count, Acc) ->
    {Key, Bin1, RefMap1} = decode_impl(Bin, RefMap),
    {Val, Bin2, RefMap2} = decode_impl(Bin1, RefMap1),
    decode_dictionary_entries(Bin2, RefMap2, Count-1, [{Key,Val}|Acc]).

-spec decode_vector(binary(), refmap(), int|uint|double|object, decode_fn()) -> internal_result(amf:amf_vector()).
decode_vector(<<Bin/binary>>, RefMap, ElementType, DecodeFn) ->
    with_object_reference(
      Bin, RefMap,
      fun (Count, <<IsFixed:8, Bin1/binary>>, RefMap1) ->
              {RealElementType, Bin2, RefMap2} =
                  case ElementType of
                      object -> decode_utf8_vr(Bin1, RefMap1);
                      _      -> {ElementType, Bin1, RefMap1}
                  end,
              {Elements, Bin3, RefMap3} = decode_sequence(Bin2, RefMap2, Count, DecodeFn, []),
              IsVariable = IsFixed =:= 0,
              {amf:vector(RealElementType, Elements, [{variable,IsVariable}]), Bin3, RefMap3};
          (Count, <<Bin1/binary>>, _RefMap1) ->
              ?THROW_PARTIAL({vector, ElementType, Count, Bin1})
      end).

-spec decode_int(binary(), refmap()) -> internal_result(amf:amf_number()).
decode_int(<<N:32/signed, Bin/binary>>, RefMap) -> {N, Bin, RefMap};
decode_int(<<Bin/binary>>, _RefMap)             -> ?THROW_PARTIAL({int, Bin}).

-spec decode_uint(binary(), refmap()) -> internal_result(amf:amf_number()).
decode_uint(<<N:32, Bin/binary>>, RefMap) -> {N, Bin, RefMap};
decode_uint(<<Bin/binary>>, _RefMap)      -> ?THROW_PARTIAL({uint, Bin}).

-spec decode_string_wrapper_object(binary(), refmap(), atom()) -> internal_result(amf:amf_value()).
decode_string_wrapper_object(<<Bin/binary>>, RefMap, Wrapper) ->
    with_object_reference(
      Bin, RefMap,
      fun (Size, Bin1, RefMap1) ->
              case Bin1 of
                  <<Str:Size/binary, Bin2/binary>> -> {amf:Wrapper(Str), Bin2, RefMap1};
                  _                                -> ?THROW_PARTIAL({Wrapper, Size, Bin1})
              end
      end).
              
-spec decode_trait(<<_:28>>, binary(), refmap()) -> internal_result(object_trait()).
decode_trait(<<RefIndex:27, 0:1>>, Bin, RefMap) ->
    case refmap_trait_get(RefMap, RefIndex) of
        {value, Value} -> {Value, Bin, RefMap};
        none           -> ?THROW_INVALID({undefined_trait_reference, RefIndex})
    end;
decode_trait(<<_:26, 1:1, 1:1>>, Bin, RefMap) ->
    {ClassName, Bin1, _RefMap1} = decode_utf8_vr(Bin, RefMap),
    ?THROW_UNSUPPORTED({object_traits_ext, ClassName, Bin1});
decode_trait(<<FieldNum:25, IsDynamic:1, 0:1, 1:1>>, Bin, RefMap) ->
    {ClassName, Bin1, RefMap1} = decode_utf8_vr(Bin, RefMap),
    {Fields, Bin2, RefMap2} = decode_sequence(Bin1, RefMap1, FieldNum, fun decode_utf8_vr/2, []),
    Trait = {case ClassName of 
                 <<"">> -> undefined;
                 _      -> ClassName
             end,
             IsDynamic =:= 1,
             Fields},
    {Trait, Bin2, refmap_trait_add(RefMap2, Trait)}.

-spec decode_u29(binary()) -> {<<_:29>>, binary()}.
decode_u29(<<0:1, B1:7, Bin/binary>>) ->
    {<<B1:29>>, Bin};
decode_u29(<<1:1, B1:7, 0:1, B2:7, Bin/binary>>) ->
    {<<B1:22,B2:7>>, Bin};
decode_u29(<<1:1, B1:7, 1:1, B2:7, 0:1, B3:7, Bin/binary>>) ->
    {<<B1:15,B2:7,B3:7>>, Bin};
decode_u29(<<1:1, B1:7, 1:1, B2:7, 1:1, B3:7, B4:8, Bin/binary>>) ->
    {<<B1:7,B2:7,B3:7,B4:8>>, Bin};
decode_u29(<<Bin/binary>>) ->
    ?THROW_PARTIAL({u29, Bin}).

-spec decode_utf8_vr(binary(), refmap()) -> internal_result(amf:amf_string()).
decode_utf8_vr(<<0:1, 0:6, 1:1, Bin/binary>>, RefMap) ->
    {<<"">>, Bin, RefMap};
decode_utf8_vr(<<Bin/binary>>, RefMap) ->
    {U29, Bin1} = decode_u29(Bin),
    case U29 of
        <<Size:28, 1:1>> ->
            case Bin1 of
                <<Str:Size/binary, Bin2/binary>> -> {Str, Bin2, refmap_str_add(RefMap, Str)};
                _                                -> ?THROW_PARTIAL({utf8_vr, Size, Bin1})
            end;
        <<RefIndex:28, 0:1>> ->
            case refmap_str_get(RefMap, RefIndex) of
                {value, Str} -> {Str, Bin1, RefMap};
                none         -> ?THROW_INVALID({undefined_string_reference, RefIndex})
            end
    end.

-spec with_object_reference(binary(), refmap(), decode_object_fn()) -> internal_result(amf:amf_value()).
with_object_reference(<<Bin/binary>>, RefMap, DecodeFn) ->
    case decode_u29(Bin) of
        {<<RefIndex:28,0:1>>, Bin1} ->
            case refmap_obj_get(RefMap, RefIndex) of
                {value, Value} -> {Value, Bin1, RefMap};
                none           -> ?THROW_INVALID({undefined_object_reference, RefIndex});
                uninitialized  -> ?THROW_UNSUPPORTED({circular_reference, RefIndex})
            end;
        {<<U28:28, 1:1>>, Bin1} ->
            {NextRefIndex, RefMap1} = refmap_obj_reserve(RefMap),
            {Value, Bin2, RefMap2} = DecodeFn(U28, Bin1, RefMap1),
            {Value, Bin2, refmap_obj_set(RefMap2, NextRefIndex, Value)}
    end.

-spec decode_kv_pairs(binary(), refmap(), [amf:amf_kv_pair()]) -> internal_result([amf:amf_kv_pair()]).
decode_kv_pairs(<<Bin/binary>>, RefMap, Acc) ->
    case decode_utf8_vr(Bin, RefMap) of
        {<<"">>, Bin1, RefMap1} ->
            {lists:reverse(Acc), Bin1, RefMap1};
        {Key, Bin1, RefMap1} ->
            {Value, Bin2, RefMap2} = decode_impl(Bin1, RefMap1),
            decode_kv_pairs(Bin2, RefMap2, [{Key,Value}|Acc])
    end.

-spec decode_sequence(binary(), refmap(), non_neg_integer(), decode_fn(), [amf:amf_value()]) -> internal_result([amf:amf_value()]).
decode_sequence(<<Bin/binary>>, RefMap, 0, _DecodeFn, Acc) ->
    {lists:reverse(Acc), Bin, RefMap};
decode_sequence(<<Bin/binary>>, RefMap, Count, DecodeFn, Acc) ->
    {Value, Bin1, RefMap1} = DecodeFn(Bin, RefMap),
    decode_sequence(Bin1, RefMap1, Count-1, DecodeFn, [Value|Acc]).

%% Internal Functions For Reference Map
-spec refmap_init() -> refmap().
refmap_init()       -> #refmap{}.

-spec refmap_str_add(refmap(), amf:amf_string()) -> refmap().
refmap_str_add(#refmap{str=Table}=RefMap, Str)   -> RefMap#refmap{str = ?DECODE_REFERENCE_TABLE_ADD(Table, Str)}.

-spec refmap_str_get(refmap(), refmap_index()) -> refmap_get_result(amf:amf_string()).
refmap_str_get(#refmap{str=Table}, RefIndex)   -> ?DECODE_REFERENCE_TABLE_GET(Table, RefIndex).

-spec refmap_trait_add(refmap(), object_trait())     -> refmap().
refmap_trait_add(#refmap{trait=Table}=RefMap, Trait) -> RefMap#refmap{trait = ?DECODE_REFERENCE_TABLE_ADD(Table, Trait)}.

-spec refmap_trait_get(refmap(), refmap_index()) -> refmap_get_result(object_trait()).
refmap_trait_get(#refmap{trait=Table}, RefIndex) -> ?DECODE_REFERENCE_TABLE_GET(Table, RefIndex).

-spec refmap_obj_reserve(refmap())            -> {refmap_index(), refmap()}.
refmap_obj_reserve(#refmap{obj=Table}=RefMap) ->
    {NextRefIndex, NextTable} = ?DECODE_REFERENCE_TABLE_RESERVE(Table),
    {NextRefIndex, RefMap#refmap{obj=NextTable}}.

-spec refmap_obj_set(refmap(), refmap_index(), amf:amf_value()) -> refmap().
refmap_obj_set(#refmap{obj=Table}=RefMap, RefIndex, Value)      -> RefMap#refmap{obj = ?DECODE_REFERENCE_TABLE_SET(Table, RefIndex, Value)}.

-spec refmap_obj_get(refmap(), refmap_index()) -> refmap_get_result(amf:amf_value()).
refmap_obj_get(#refmap{obj=Table}, RefIndex)   -> ?DECODE_REFERENCE_TABLE_GET(Table, RefIndex).
