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

%% @doc AMF0 encode API
%% @reference AMF0 specification: [http://download.macromedia.com/pub/labs/amf/amf0_spec_121207.pdf]
%% @private
-module(amf0_encode).
-compile(inline).

%% Encode API
-export([encode/1, unsafe_encode/1]).

-include("../include/amf.hrl").
-include("../include/internal/amf_internal.hrl").

%% Macro
-define(CHECK_LENGTH(Tag, Str, Length),
        case byte_size(Str) < Length of
            true  -> ok;
            false -> ?THROW_INVALID({Tag, Length, Str})
        end).

%% External Functions

%% @doc Encode AMF0 value.
-spec encode(AmfValue) -> {ok, EncodedData} | {error, Reason} when
      AmfValue    :: amf:amf_value(),
      EncodedData :: iolist(),
      Reason      :: amf:amf_exception().
encode(Value) ->
    try unsafe_encode(Value) of
        EncodedData -> {ok, EncodedData}
    catch
        throw:#amf_exception{}=Ex -> {error, Ex}
    end.

%% @doc Encode AMF0 value (maybe throw exception).
%%
%% This function throws exception if the input value could not be encoded correctly.
%%
%% @throws amf:amf_exception()
-spec unsafe_encode(AmfValue) -> EncodedData when
      AmfValue    :: amf:amf_value(),
      EncodedData :: iolist().
unsafe_encode(Value) ->
    encode_impl(Value).

%% Internal Functions for Encode
-spec encode_impl(amf:amf_value()) -> iolist().
encode_impl(Val) when is_number(Val)  -> encode_number(Val);
encode_impl(Val) when is_boolean(Val) -> encode_boolean(Val);
encode_impl(Val) when is_binary(Val)  -> encode_string(Val);
encode_impl(Val) when is_list(Val)    -> encode_strict_array(Val);
encode_impl(null)                     -> encode_null();
encode_impl(undefined)                -> encode_undefined();
encode_impl(Val=#amf_object{})        -> encode_object(Val);
encode_impl(Val=#amf_array{})         -> encode_ecma_array(Val);
encode_impl(Val=#amf_date{})          -> encode_date(Val);
encode_impl(Val=#amf_xml_document{})  -> encode_xml_document(Val);
encode_impl(Val=#amf_avmplus_object{})-> encode_avmplus_object(Val);
encode_impl(Val)                      -> ?THROW_UNSUPPORTED({value, Val}).

-spec encode_number(amf:amf_number()) -> iolist().
encode_number(Num) -> [?AMF0_NUMBER_MARKER, <<Num/float>>].

-spec encode_boolean(amf:amf_boolean()) -> iolist().
encode_boolean(false) -> [?AMF0_BOOLEAN_MARKER, 0];
encode_boolean(true)  -> [?AMF0_BOOLEAN_MARKER, 1].

-spec encode_string(amf:amf_string()) -> iolist().
encode_string(Str) when byte_size(Str) < 16#10000 ->
    [?AMF0_STRING_MARKER, <<(byte_size(Str)):16>>, Str];
encode_string(Str) ->
    ?CHECK_LENGTH(string, Str, 16#100000000),
    [?AMF0_LONG_STRING_MARKER, <<(byte_size(Str)):32>>, Str].
    
-spec encode_object(amf:amf_object()) -> iolist().
encode_object(#amf_object{class=undefined, members=Members}) ->
    [?AMF0_OBJECT_MARKER, encode_kv_pairs(Members,[]), 0, 0, ?AMF0_OBJECT_END_MARKER];
encode_object(#amf_object{class=Class, members=Members}) ->
    ?CHECK_LENGTH(class_name, Class, 16#10000),
    MembersBin = encode_kv_pairs(Members,[]),
    [?AMF0_TYPED_OBJECT_MARKER, <<(byte_size(Class)):16>>, Class, MembersBin, 0, 0, ?AMF0_OBJECT_END_MARKER].

-spec encode_null() -> iolist().
encode_null()       -> [?AMF0_NULL_MARKER].

-spec encode_undefined() -> iolist().
encode_undefined()       -> [?AMF0_UNDEFINED_MARKER].

-spec encode_ecma_array(amf:amf_ecma_array()) -> iolist().
encode_ecma_array(#amf_array{members=Members}) ->
    MembersBin = (encode_kv_pairs(Members,[])),
    [?AMF0_ECMA_ARRAY_MARKER, <<(length(Members)):32>>, MembersBin, 0, 0, ?AMF0_OBJECT_END_MARKER].

-spec encode_strict_array(amf:amf_strict_array()) -> iolist().
encode_strict_array(Ary) ->
    [?AMF0_STRICT_ARRAY_MARKER, <<(length(Ary)):32>> |  encode_values(Ary,[])].

-spec encode_date(amf:amf_date()) -> iolist().
encode_date(#amf_date{timestamp=Timestamp}) ->
    {MegaSecs, Secs, MicroSecs} = Timestamp,
    MilliSecs = (MegaSecs*1000*1000*1000) + (Secs*1000) + (MicroSecs div 1000),
    TimeZone = 0,
    [?AMF0_DATE_MARKER, <<MilliSecs/float, TimeZone:16>>].

-spec encode_xml_document(amf:amf_xml_document()) -> iolist().
encode_xml_document(#amf_xml_document{data=Data}) ->
    ?CHECK_LENGTH(xml_document, Data, 16#100000000),
    [?AMF0_XML_DOCUMENT_MARKER, <<(byte_size(Data)):32>>, Data].

-spec encode_avmplus_object(amf:amf_avmplus_object())   -> iolist().
encode_avmplus_object(#amf_avmplus_object{value=Value}) -> amf3_encode:unsafe_encode(Value).

-spec encode_kv_pairs([amf:amf_kv_pair()], iolist()) -> iolist().
encode_kv_pairs([], Acc) ->
    lists:reverse(Acc);
encode_kv_pairs([{K,V}|Members], Acc) ->
    ?CHECK_LENGTH(key, K, 16#10000),
    encode_kv_pairs(Members, [encode_impl(V), K, <<(byte_size(K)):16>> | Acc]).

-spec encode_values([amf:amf_value()], iolist()) -> iolist().
encode_values([], Acc) ->
    lists:reverse(Acc);
encode_values([Value|Values], Acc) ->
    encode_values(Values, [encode_impl(Value) | Acc]).
