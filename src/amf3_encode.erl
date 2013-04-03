%%% @doc AMF3 encode API
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
-module(amf3_encode).
-compile(inline).
-include("../include/amf.hrl").
-include("../include/internal/amf_internal.hrl").

%% Encode API
-export([encode/1, unsafe_encode/1]).

%% Internal Types
-type encode_fn() :: fun((amf:amf_value()) -> iolist()).

%% External Functions

%% @doc Encode AMF3 value.
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

%% @doc Encode AMF3 value (maybe throw exception).
%%
%% This function throws exception if the input value could not be encoded correctly.
%%
%% @throws amf:amf_exception()
-spec unsafe_encode(AmfValue) -> EncodedData when
      AmfValue    :: amf:amf_value(),
      EncodedData :: iolist().
unsafe_encode(Value) ->
    encode_impl(Value).

%% Internal Functions
-spec encode_impl(amf:amf_value())    -> iolist().
encode_impl(undefined)                -> encode_undefined();
encode_impl(null)                     -> encode_null();
encode_impl(Val) when is_boolean(Val) -> encode_boolean(Val);
encode_impl(Val) when is_integer(Val) -> encode_integer(Val);
encode_impl(Val) when is_float(Val)   -> encode_double(Val);
encode_impl(Val) when is_binary(Val)  -> encode_string(Val);
encode_impl(Val) when is_list(Val)    -> encode_strict_array(Val);
encode_impl(Val=#amf_object{})        -> encode_object(Val);
encode_impl(Val=#amf_array{})         -> encode_array(Val);
encode_impl(Val=#amf_xml_document{})  -> encode_xml_document(Val);
encode_impl(Val=#amf_xml{})           -> encode_xml(Val);
encode_impl(Val=#amf_byte_array{})    -> encode_byte_array(Val);
encode_impl(Val=#amf_date{})          -> encode_date(Val);
encode_impl(Val=#amf_vector{})        -> encode_vector(Val);
encode_impl(Val=#amf_dictionary{})    -> encode_dictionary(Val);
encode_impl(Val)                      -> ?THROW_UNSUPPORTED({value, Val}).

-spec encode_undefined() -> iolist().
encode_undefined()       -> [?AMF3_UNDEFINED_MARKER].

-spec encode_null() -> iolist().
encode_null()       -> [?AMF3_NULL_MARKER].

-spec encode_boolean(boolean()) -> iolist().
encode_boolean(true)  -> [?AMF3_TRUE_MARKER];
encode_boolean(false) -> [?AMF3_FALSE_MARKER].

-spec encode_integer(integer()) -> iolist().
encode_integer(Val) ->
    case Val of
        N when N >  16#0FFFFFFF -> encode_double(Val+0.0);
        N when N < -16#10000000 -> encode_double(Val+0.0);
        N ->
            Unsigned = case N of
                           _ when N >= 0 -> N;
                           _             -> 16#20000000 + N
                       end,
            [?AMF3_INTEGER_MARKER, encode_u29(Unsigned)]
    end.

-spec encode_double(float()) -> iolist().
encode_double(Val)           -> [?AMF3_DOUBLE_MARKER, <<Val/float>>].

-spec encode_string(binary()) -> iolist().
encode_string(Val)            -> [?AMF3_STRING_MARKER, encode_utf8_vr(Val)].

-spec encode_strict_array([amf:amf_value()]) -> iolist().
encode_strict_array(List) ->
    Count = length(List),
    U29 = (Count bsl 1) + 1,
    [?AMF3_ARRAY_MARKER, encode_u29(U29), encode_utf8_vr(<<"">>), encode_sequence(fun encode_impl/1, List, [])].

-spec encode_object(amf:amf_object()) -> iolist().
encode_object(Obj) ->
    #amf_object{sealed_fields=Fields, members=Members, dynamic=IsDynamic} = Obj,
    Values         = [proplists:get_value(F, Members, null) || F <- Fields],
    DynamicMembers = lists:filter(fun ({K,_}) -> not lists:member(K, Fields) end, Members),

    [?AMF3_OBJECT_MARKER,
     encode_trait(Obj),
     encode_sequence(fun encode_impl/1, Values, []),
     case IsDynamic of
         true  -> encode_kv_pairs(DynamicMembers, []);
         false -> []
     end].

-spec encode_array(amf:amf_array()) -> iolist().
encode_array(Ary) ->
    #amf_array{values=Values, members=Members} = Ary,
    <<U29:29>> = <<(length(Values)):28, 1:1>>,
    [?AMF3_ARRAY_MARKER, encode_u29(U29), encode_kv_pairs(Members,[]), encode_sequence(fun encode_impl/1, Values, [])].

-spec encode_xml_document(amf:amf_xml_document()) -> iolist().
encode_xml_document(#amf_xml_document{data=Data}) ->
    U29 = (byte_size(Data) bsl 1) + 1,
    [?AMF3_XML_DOC_MARKER, encode_u29(U29), Data].

-spec encode_xml(amf:amf_xml()) -> iolist().
encode_xml(#amf_xml{data=Data}) ->
    U29 = (byte_size(Data) bsl 1) + 1,
    [?AMF3_XML_MARKER, encode_u29(U29), Data].

-spec encode_byte_array(amf:amf_byte_array()) -> iolist().
encode_byte_array(#amf_byte_array{data=Data}) ->
    U29 = (byte_size(Data) bsl 1) + 1,
    [?AMF3_BYTE_ARRAY_MARKER, encode_u29(U29), Data].    

-spec encode_date(amf:amf_date()) -> iolist().
encode_date(#amf_date{timestamp=Timestamp}) ->
    {MegaSecs, Secs, MicroSecs} = Timestamp,
    MilliSecs = (MegaSecs*1000*1000*1000) + (Secs*1000) + (MicroSecs div 1000),
    [?AMF3_DATE_MARKER, encode_u29(1), <<MilliSecs/float>>].

-spec encode_vector(amf:amf_vector()) -> iolist().
encode_vector(#amf_vector{type=Type, variable=IsVariable, elements=Elements}) ->
    {Marker, TypeName, EncodeFn} =
        case Type of
            int    -> {?AMF3_VECTOR_INT_MARKER,    [], fun (N) -> <<N:32>> end};
            uint   -> {?AMF3_VECTOR_UINT_MARKER,   [], fun (N) -> <<N:32/signed>> end};
            double -> {?AMF3_VECTOR_DOUBLE_MARKER, [], fun (N) -> <<N/float>> end};
            _      -> {?AMF3_VECTOR_OBJECT_MARKER, encode_utf8_vr(Type), fun encode_impl/1}
        end,
    FixedFlag = case IsVariable of
                    true  -> 0;
                    false -> 1
                end,
    U29 = (length(Elements) bsl 1) + 1,
    [Marker, encode_u29(U29), FixedFlag, TypeName | encode_sequence(EncodeFn, Elements, [])].

-spec encode_dictionary(amf:amf_dictionary()) -> iolist().
encode_dictionary(#amf_dictionary{weak=IsWeak, members=Members}) ->
    WeakFlag = case IsWeak of
                   true  -> 1;
                   false -> 0
               end,
    U29 = (length(Members) bsl 1) + 1,
    [?AMF3_DICTIONARY_MARKER, encode_u29(U29), WeakFlag, encode_dictionary_entries(Members,[])].

-spec encode_trait(amf:amf_object()) -> iolist().
encode_trait(Obj) ->
    #amf_object{class=Class, sealed_fields=Fields, dynamic=IsDynamic} = Obj,
    DynamicFlag = case IsDynamic of
                      true  -> 1;
                      false -> 0
                  end,
    ClassName = case Class of
                    undefined -> <<"">>;
                    _         -> Class
                end,
    FieldCount = length(Fields),
    <<U29:29>> = <<FieldCount:25, DynamicFlag:1, 0:1, 1:1, 1:1>>,
    [encode_u29(U29), encode_utf8_vr(ClassName) | encode_sequence(fun encode_utf8_vr/1, Fields, [])].

-spec encode_u29(non_neg_integer()) -> binary().
encode_u29(U29) ->
    case U29 of
        N when N < 16#80 ->
            <<0:1, N:7>>;
        N when N < 16#4000 ->
            <<B1:7, B2:7>> = <<N:14>>,
            <<1:1, B1:7, 0:1, B2:7>>;
        N when N < 16#200000 ->
            <<B1:7, B2:7, B3:7>> = <<N:21>>,
            <<1:1, B1:7, 1:1, B2:7, 0:1, B3:7>>;
        N when N < 16#40000000 ->
            <<B1:7, B2:7, B3:7, B4:8>> = <<N:29>>,
            <<1:1, B1:7, 1:1, B2:7, 1:1, B3:7, B4:8>>;
        N ->
            ?THROW_INVALID({u29, N})
    end.

-spec encode_utf8_vr(binary()) -> iolist().
encode_utf8_vr(Val) -> 
    Size = byte_size(Val),
    U29 = (Size bsl 1) + 1,
    [encode_u29(U29), Val].

-spec encode_sequence(encode_fn(), [amf:amf_value()], iolist()) -> iolist().
encode_sequence(_EncodeFn, [], Acc) ->
    lists:reverse(Acc);
encode_sequence(EncodeFn, [Val|List], Acc) ->
    encode_sequence(EncodeFn, List, [EncodeFn(Val)|Acc]).

-spec encode_kv_pairs([amf:amf_kv_pair()], iolist()) -> iolist().
encode_kv_pairs([], Acc) ->
    lists:reverse([encode_utf8_vr(<<"">>) | Acc]);
encode_kv_pairs([{K,V}|List], Acc) ->
    encode_kv_pairs(List, [encode_impl(V), encode_utf8_vr(K) | Acc]).

-spec encode_dictionary_entries([amf:amf_dictionary_entry()], iolist()) -> iolist().
encode_dictionary_entries([], Acc) ->
    lists:reverse(Acc);
encode_dictionary_entries([{K,V}|List], Acc) ->
    encode_dictionary_entries(List, [encode_impl(V), encode_impl(K) | Acc]).
