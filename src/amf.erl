%%% @doc AMF encode/decode functions and AMF value constructors
%%% @reference AMF0 specification: [http://download.macromedia.com/pub/labs/amf/amf0_spec_121207.pdf]
%%% @reference AMF3 specification: [http://wwwimages.adobe.com/www.adobe.com/content/dam/Adobe/en/devnet/amf/pdf/amf-file-format-spec.pdf]
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
-module(amf).
-include("../include/amf.hrl").
-include("../include/amf_type.hrl").

%% Encode/Decode API
-export([
         decode/2, 
         encode/2
        ]).

%% AMF Value Construct API
-export([
         object/1, object/2, typed_object/2, 
         array/1, array/2,
         date/1, datetime_to_date/1, msec_to_date/1,
         xml/1, xml_document/1, 
         avmplus_object/1,
         byte_array/1, 
         vector/2, vector/3,
         dictionary/1, dictionary/2
        ]).

%% Exported Types
-export_type([
              amf_version/0,

              amf_exception/0,
              amf_exception_type/0,
              
              amf_value/0,
              amf_number/0,
              amf_boolean/0,
              amf_string/0,
              amf_null/0,
              amf_undefined/0,
              amf_object/0,
              amf_array/0,
              amf_date/0,
              amf_xml_document/0,
              amf_xml/0,
              amf_byte_array/0,
              amf_vector/0,
              amf_vector_element_type/0,
              amf_dictionary/0,
              amf_dictionary_entry/0,
              amf_ecma_array/0,
              amf_strict_array/0,
              amf_avmplus_object/0,
              amf_kv_pair/0,
              amf_class_name/0,
              
              amf_object_option/0,
              amf_vector_option/0,
              amf_dictionary_option/0
             ]).

%% Option Types
-type amf_object_option() :: {class, amf_class_name()} |
                             {dynamic, boolean()} |
                             {sealed_fields, [amf_string()]}.

-type amf_vector_option() :: {variable, boolean()}.

-type amf_dictionary_option() :: {weak, boolean()}.

%% Encode/Decode API

%% @doc Decode AMF0/AMF3 binary data.
-spec decode(AmfVersion, EncodedBytes) -> {ok, DecodedValue, UnconsumedBytes} | {error, Reason} when
      AmfVersion      :: amf_version(),
      EncodedBytes    :: binary(),
      DecodedValue    :: amf_value(),
      UnconsumedBytes :: binary(),
      Reason          :: amf_exception().
decode(amf0, Bin) -> amf0_decode:decode(Bin);
decode(amf3, Bin) -> amf3_decode:decode(Bin).

%% @doc Encode AMF0/AMF3 value.
-spec encode(AmfVersion, AmfValue) -> {ok, EncodedData} | {error, Reason} when
      AmfVersion  :: amf_version(),
      AmfValue    :: amf_value(),
      EncodedData :: iolist(),
      Reason      :: amf_exception().
encode(amf0, Value) -> amf0_encode:encode(Value);
encode(amf3, Value) -> amf3_encode:encode(Value).

%% AMF value Construct API

%% @doc Make anonymous object.
-spec object(Members) -> amf_object() when Members :: [amf_kv_pair()].
object(Members)       -> #amf_object{members = Members}.

%% @doc Make typed-object.
-spec typed_object(TypeName, Members) -> amf_object() when
      TypeName :: amf_class_name(),
      Members  :: [amf_kv_pair()].
typed_object(TypeName, Members) ->
    object(Members, [{class, TypeName},
                     {dynamic, false},
                     {sealed_fields, [Key || {Key,_} <- Members]}]).

%% @doc Make object.
-spec object(Members, Options) -> amf_object() when
      Members :: [amf_kv_pair()], 
      Options :: [amf_object_option()].
object(Members, Options) ->
    #amf_object{members = Members,
                class = proplists:get_value(class, Options, undefined),
                dynamic = proplists:get_value(dynamic, Options, true),
                sealed_fields = proplists:get_value(sealed_fields, Options, [])}.

%% @doc Make associative array.
-spec array(Members) -> amf_array() when Members :: [amf_kv_pair()].
array(Members)       -> #amf_array{members = Members}.
    
%% @doc Make generic array.
-spec array(Values, Members) -> amf_array() when
      Values  :: [amf_value()],
      Members :: [amf_kv_pair()].
array(Values, Members) ->
    #amf_array{values = Values, members = Members}.

%% @doc Make date.
-spec date(Timestamp)   -> amf_date() when Timestamp :: erlang:timestamp().
date({_,_,_}=Timestamp) -> #amf_date{timestamp=Timestamp}.

%% @doc Make date from timestamp in milliseconds
-spec msec_to_date(MilliSeconds) -> amf_date() when MilliSeconds :: non_neg_integer().
msec_to_date(MilliSecs) ->
    MegaSecs  = MilliSecs div (1000*1000*1000),
    Secs      = (MilliSecs rem (1000*1000*1000)) div 1000,
    MicroSecs = (MilliSecs rem 1000) * 1000,
    date({MegaSecs,Secs,MicroSecs}).

%% @doc Make date from datetime.
-spec datetime_to_date(DateTime) -> amf_date() when DateTime :: calendar:datetime().
datetime_to_date(DateTime) ->
    Secs = 
        calendar:datetime_to_gregorian_seconds(DateTime) -
        calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
    case Secs < 0 of
        true  -> error({too_old_datetime, DateTime});
        false -> msec_to_date(Secs*1000)
    end.

%% @doc Make XmlDocument.
-spec xml_document(Xml) -> amf_xml_document() when Xml :: amf_string().
xml_document(Xml)       -> #amf_xml_document{data = Xml}.

%% @doc Make Xml.
-spec xml(Xml) -> amf_xml() when Xml :: amf_string().
xml(Xml)       -> #amf_xml{data = Xml}.

%% @doc Make AvmPlus Object.
-spec avmplus_object(AmfValue) -> amf_avmplus_object() when AmfValue :: amf_value().
avmplus_object(AmfValue)       -> #amf_avmplus_object{value = AmfValue}.
    
%% @doc Make ByteArray.
-spec byte_array(Bytes) -> amf_byte_array() when Bytes :: binary().
byte_array(Bytes)       -> #amf_byte_array{data = Bytes}.

%% @doc Make variable vector.
-spec vector(ElementType, Elements) -> amf_vector() when
      ElementType :: amf_vector_element_type(),
      Elements    :: [amf_value()].
vector(ElementType, Elements) -> vector(ElementType, Elements, []).

%% @doc Make vector.
-spec vector(ElementType, Elements, Options) -> amf_vector() when
      ElementType :: amf_vector_element_type(),
      Elements    :: [amf_value()],
      Options     :: [amf_vector_option()].
vector(ElementType, Elements, Options) ->
    #amf_vector{type = ElementType,
                variable = proplists:get_value(variable, Options, true),
                elements = Elements}.

%% @doc Make dictionary.
-spec dictionary(Entries) -> amf_dictionary() when Entries :: [amf_dictionary_entry()].
dictionary(Members) -> dictionary(Members, []).

%% @doc Make dictionary.    
-spec dictionary(Entries, Options) -> amf_dictionary() when
      Entries :: [amf_dictionary_entry()],
      Options :: [amf_dictionary_option()].
dictionary(Members, Options) ->
    #amf_dictionary{members = Members,
                    weak = proplists:get_value(weak, Options, false)}.
