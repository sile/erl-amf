%% Copyright (c) 2013, Takeru Ohta <phjgt308@gmail.com>
%%
%% The MIT License
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

%% @doc AMF0 decode API
%% @reference AMF0 specification: [http://download.macromedia.com/pub/labs/amf/amf0_spec_121207.pdf]
%% @private
-module(amf0_decode).
-compile(inline).

%% Decode API
-export([decode/1, unsafe_decode/1]).

-include("../include/amf.hrl").
-include("../include/internal/amf_internal.hrl").

%% Internal Types
-type refmap_index() :: decode_reference_table_index().
-type refmap()       :: decode_reference_table().
-type internal_result(DecodedValueType) :: {DecodedValueType, UnconsumedBytes::binary(), refmap()}.

%% Macro
-define(WITH_REFERENCE(RefMap, RefMapVar, Block),
        begin
            {__RefIndex, RefMapVar} = refmap_reserve(RefMap),
            {__Value, __Bin, __RefMap1} = Block,
            {__Value, __Bin, refmap_set(__RefMap1, __RefIndex, __Value)}
        end).

%% External Functions

%% @doc Decode AMF0 binary data.
-spec decode(EncodedBytes) -> {ok, DecodedValue, UnconsumedBytes} | {error, Reason} when 
      EncodedBytes    :: binary(),
      DecodedValue    :: amf:amf_value(),
      UnconsumedBytes :: binary(),
      Reason          :: amf:amf_exception().
decode(<<Bin/binary>>) ->
    try unsafe_decode(Bin) of
        {Value, UnconsumedBin} -> {ok, Value, UnconsumedBin}
    catch
        throw:#amf_exception{}=Ex -> {error, Ex}
    end.

%% @doc Decode AMF0 binary data (maybe throw exception).
%%
%% This function throws exception if the input data could not be decoded correctly.
%%
%% @throws amf:amf_exception()
-spec unsafe_decode(EncodedBytes) -> {DecodedValue, UnconsumedBytes} when
      EncodedBytes    :: binary(),
      DecodedValue    :: amf:amf_value(),
      UnconsumedBytes :: binary().
unsafe_decode(<<Bin/binary>>) ->
    {Value, UnconsumedBin, _RefMap} = decode_impl(Bin, refmap_init()),
    {Value, UnconsumedBin}.

%% Internal Functions for Decode
-spec decode_impl(binary(), refmap()) -> internal_result(amf:amf_value()).
decode_impl(<<Marker:8, Bin/binary>>, RefMap) ->
    case Marker of
        ?AMF0_NUMBER_MARKER         -> decode_number(Bin, RefMap);
        ?AMF0_BOOLEAN_MARKER        -> decode_boolean(Bin, RefMap);
        ?AMF0_STRING_MARKER         -> decode_string(Bin, RefMap);
        ?AMF0_OBJECT_MARKER         -> decode_object(Bin, RefMap);
        ?AMF0_MOVIECLIP_MARKER      -> decode_movieclip(Bin, RefMap);
        ?AMF0_NULL_MARKER           -> decode_null(Bin, RefMap);
        ?AMF0_UNDEFINED_MARKER      -> decode_undefined(Bin, RefMap);
        ?AMF0_REFERENCE_MARKER      -> decode_reference(Bin, RefMap);
        ?AMF0_ECMA_ARRAY_MARKER     -> decode_ecma_array(Bin, RefMap);
        ?AMF0_OBJECT_END_MARKER     -> decode_object_end(Bin, RefMap);
        ?AMF0_STRICT_ARRAY_MARKER   -> decode_strict_array(Bin, RefMap);
        ?AMF0_DATE_MARKER           -> decode_date(Bin, RefMap);
        ?AMF0_LONG_STRING_MARKER    -> decode_long_string(Bin, RefMap);
        ?AMF0_UNSUPPORTED_MARKER    -> decode_unsupported(Bin, RefMap);
        ?AMF0_RECORDSET_MARKER      -> decode_recordset(Bin, RefMap);
        ?AMF0_XML_DOCUMENT_MARKER   -> decode_xml_document(Bin, RefMap);
        ?AMF0_TYPED_OBJECT_MARKER   -> decode_typed_object(Bin, RefMap);
        ?AMF0_AVMPLUS_OBJECT_MARKER -> decode_avmplus_object(Bin, RefMap);
        _                           -> decode_unknown(Marker, Bin, RefMap)
    end;
decode_impl(<<Bin/binary>>, _RefMap) -> ?THROW_PARTIAL({marker, Bin}).

-spec decode_movieclip(binary(), refmap()) -> no_return().
decode_movieclip(_Bin, _RefMap)  -> ?THROW_UNSUPPORTED(movieclip).

-spec decode_unsupported(binary(), refmap()) -> no_return().
decode_unsupported(_Bin, _RefMap)  -> ?THROW_UNSUPPORTED(unsupported).

-spec decode_recordset(binary(), refmap()) -> no_return().
decode_recordset(_Bin, _RefMap)  -> ?THROW_UNSUPPORTED(recordset).

-spec decode_object_end(binary(), refmap()) -> no_return().
decode_object_end(_Bin, _RefMap)  -> ?THROW_INVALID(unexpected_object_end).

-spec decode_unknown(non_neg_integer(), binary(), refmap()) -> no_return().
decode_unknown(Marker, _Bin, _RefMap) -> ?THROW_INVALID({unknown_marker, Marker}).

-spec decode_null(binary(), refmap()) -> internal_result(amf:amf_null()).
decode_null(<<Bin/binary>>, RefMap)   -> {null, Bin, RefMap}.

-spec decode_undefined(binary(), refmap()) -> internal_result(amf:amf_undefined()).
decode_undefined(<<Bin/binary>>, RefMap)   -> {undefined, Bin, RefMap}.
    
-spec decode_number(binary(), refmap()) -> internal_result(amf:amf_number()).
decode_number(<<Num/float,    Bin/binary>>, RefMap) -> {Num, Bin, RefMap};
decode_number(<<Num:8/binary,_Bin/binary>>,_RefMap) -> ?THROW_UNSUPPORTED({number, Num});
decode_number(<<Bin/binary>>,              _RefMap) -> ?THROW_PARTIAL({number, Bin}).

-spec decode_boolean(binary(), refmap()) -> internal_result(amf:amf_boolean()).
decode_boolean(<<0, Bin/binary>>, RefMap) -> {false, Bin, RefMap};
decode_boolean(<<1, Bin/binary>>, RefMap) -> {true, Bin, RefMap};
decode_boolean(<<N,_Bin/binary>>,_RefMap) -> ?THROW_INVALID({not_a_boolean, N});
decode_boolean(<<Bin/binary>>,   _RefMap) -> ?THROW_PARTIAL({boolean, Bin}).

-spec decode_string(binary(), refmap()) -> internal_result(amf:amf_string()).
decode_string(<<Len:16, Str:Len/binary, Bin/binary>>, RefMap) -> {Str, Bin, RefMap};
decode_string(<<Bin/binary>>,                        _RefMap) -> ?THROW_PARTIAL({string, Bin}).

-spec decode_long_string(binary(), refmap()) -> internal_result(amf:amf_string()).
decode_long_string(<<Len:32, Str:Len/binary, Bin/binary>>, RefMap) -> {Str, Bin, RefMap};
decode_long_string(<<Bin/binary>>,                        _RefMap) -> ?THROW_PARTIAL({long_string, Bin}).

-spec decode_xml_document(binary(), refmap()) -> internal_result(amf:amf_xml_document()).
decode_xml_document(<<Len:32, Str:Len/binary, Bin/binary>>, RefMap) -> {amf:xml_document(Str), Bin, RefMap};
decode_xml_document(<<Bin/binary>>,                        _RefMap) -> ?THROW_PARTIAL({xml_document, Bin}).

-spec decode_date(binary(), refmap()) -> internal_result(amf:amf_date()).
decode_date(<<DateMillis/float, _TimeZone:16/signed, Bin/binary>>, RefMap) ->
    case DateMillis < 0 of
        true  -> ?THROW_INVALID({negative_date, DateMillis});
        false -> {amf:msec_to_date(trunc(DateMillis)), Bin, RefMap}
    end;
decode_date(<<DateBin:10/binary, _Bin/binary>>, _RefMap) ->
    ?THROW_INVALID({date, DateBin});
decode_date(<<Bin/binary>>, _RefMap) -> 
    ?THROW_PARTIAL({date, Bin}).

-spec decode_avmplus_object(binary(), refmap()) -> internal_result(amf:amf_value()).
decode_avmplus_object(<<Bin/binary>>, RefMap) ->
    {Value, Bin1} = amf3_decode:unsafe_decode(Bin),
    {Value, Bin1, RefMap}.

-spec decode_reference(binary(), refmap()) -> internal_result(amf:amf_object()).
decode_reference(<<RefIndex:16, Bin/binary>>, RefMap) ->
    case refmap_get(RefMap, RefIndex) of
        {value, Value} -> {Value, Bin, RefMap};
        none           -> ?THROW_INVALID({undefined_object_reference, RefIndex});
        uninitialized  -> ?THROW_UNSUPPORTED({circular_reference, RefIndex})
    end;
decode_reference(<<Bin/binary>>, _RefMap) -> ?THROW_PARTIAL({reference, Bin}).

-spec decode_object(binary(), refmap()) -> internal_result(amf:amf_object()).
decode_object(<<Bin/binary>>, RefMap) ->
    ?WITH_REFERENCE(RefMap, RefMap1,
                    case decode_kv_pairs(Bin, RefMap1, []) of
                        {Members, Bin1, RefMap2} -> {amf:object(Members), Bin1, RefMap2}
                    end).

-spec decode_typed_object(binary(), refmap()) -> internal_result(amf:amf_object()).
decode_typed_object(<<Len:16, Type:Len/binary, MembersBin/binary>>, RefMap) ->
    ?WITH_REFERENCE(RefMap, RefMap1,
                    case decode_kv_pairs(MembersBin, RefMap1, []) of
                        {Members, Bin1, RefMap2} -> {amf:typed_object(Type, Members), Bin1, RefMap2}
                    end);
decode_typed_object(<<Bin/binary>>, _RefMap) -> ?THROW_PARTIAL({typed_object, Bin}).

-spec decode_ecma_array(binary(), refmap()) -> internal_result(amf:amf_ecma_array()).
decode_ecma_array(<<_Len:32, MembersBin/binary>>, RefMap) ->
    ?WITH_REFERENCE(RefMap, RefMap1,
                    case decode_kv_pairs(MembersBin, RefMap1, []) of
                        {Members, Bin1, RefMap2} -> {amf:array(Members), Bin1, RefMap2}
                    end);
decode_ecma_array(<<Bin/binary>>, _RefMap) -> ?THROW_PARTIAL({ecma_array, Bin}).

-spec decode_strict_array(binary(), refmap()) -> internal_result(amf:amf_strict_array()).
decode_strict_array(<<Count:32, ValuesBin/binary>>, RefMap) ->
    ?WITH_REFERENCE(RefMap, RefMap1, decode_values(ValuesBin, RefMap1, Count, []));
decode_strict_array(<<Bin/binary>>, _RefMap) -> 
    ?THROW_PARTIAL({strict_array, Bin}).

-spec decode_kv_pairs(binary(), refmap(), [amf:amf_kv_pair()]) -> internal_result([amf:amf_kv_pair()]).
decode_kv_pairs(<<0:16, ?AMF0_OBJECT_END_MARKER, Bin/binary>>, RefMap, Acc) -> 
    {lists:reverse(Acc), Bin, RefMap};
decode_kv_pairs(<<0:16, Bin/binary>>, _RefMap, _Acc) ->
    ?THROW_INVALID({missing_object_end, Bin});
decode_kv_pairs(<<Len:16, Key:Len/binary, ValueBin/binary>>, RefMap, Acc) ->
    {Value, Bin1, RefMap2} = decode_impl(ValueBin, RefMap),
    decode_kv_pairs(Bin1, RefMap2, [{Key,Value}|Acc]);
decode_kv_pairs(<<Bin/binary>>, _RefMap, _Acc) ->
    ?THROW_PARTIAL({kv_pairs, Bin}).

-spec decode_values(binary(), refmap(), non_neg_integer(), [amf:amf_value()]) -> internal_result([amf:amf_value()]).
decode_values(<<Bin/binary>>, RefMap, 0, Acc) ->
    {lists:reverse(Acc), Bin, RefMap};
decode_values(<<Bin/binary>>, RefMap, Count, Acc) ->
    {Value, Bin1, RefMap1} = decode_impl(Bin, RefMap),
    decode_values(Bin1, RefMap1, Count-1, [Value|Acc]).

%% Internal Functions For Reference Map
-spec refmap_init() -> refmap().
refmap_init() -> ?DECODE_REFERENCE_TABLE_INIT().

-spec refmap_reserve(refmap()) -> {refmap_index(), refmap()}.
refmap_reserve(RefMap) -> ?DECODE_REFERENCE_TABLE_RESERVE(RefMap).

-spec refmap_set(refmap(), refmap_index(), amf:amf_value()) -> refmap().
refmap_set(RefMap, RefIndex, Value) -> ?DECODE_REFERENCE_TABLE_SET(RefMap, RefIndex, Value).

-spec refmap_get(refmap(), refmap_index()) -> none | uninitialized | {value, amf:amf_value()}.
refmap_get(RefMap, RefIndex) -> ?DECODE_REFERENCE_TABLE_GET(RefMap, RefIndex).
