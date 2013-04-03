-module(amf).

-include("../include/amf.hrl").
-include("../include/amf_type.hrl").

-export([decode/2, encode/2, encode_to_iolist/2]).

-export([
         object/1, object/2, typed_object/2, array/1, array/2,
         date/1, datetime_to_date/1, msec_to_date/1,
         xml/1, xml_document/1, avmplus_object/1,
         byte_array/1, vector/2, vector/3,
         dictionary/1, dictionary/2
        ]).

-export_type([
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
              amf_version/0,

              amf_object_option/0, amf_vector_option/0, amf_dictionary_option/0
             ]).

-type amf_object_option() :: {class, undefined|amf_string()} |
                             {dynamic, boolean()} |
                             {sealed_fields, [amf_string()]}.

-type amf_vector_option() :: {variable, boolean()}.

-type amf_dictionary_option() :: {weak, boolean()}.


-spec decode(amf_version(), binary()) -> {amf_value(), binary()}.
decode(amf0, Bin) -> amf0_decode:decode(Bin);
decode(amf3, Bin) -> amf3_decode:decode(Bin).

-spec encode(amf_version(), amf_value()) -> binary().
encode(amf0, Value) -> amf0_encode:encode(Value);
encode(amf3, Value) -> amf3_encode:encode(Value).

-spec encode_to_iolist(amf_version(), amf_value()) -> iolist().
encode_to_iolist(amf0, Value) -> amf0_encode:encode_to_iolist(Value);
encode_to_iolist(amf3, Value) -> amf3_encode:encode_to_iolist(Value).

-spec object([amf_kv_pair()]) -> amf_object().
object(Members) ->
    #amf_object{members = Members}.

-spec object([amf_kv_pair()], [amf_object_option()]) -> amf_object().
object(Members, Options) ->
    #amf_object{members = Members,
                class = proplists:get_value(class, Options, undefined),
                dynamic = proplists:get_value(dynamic, Options, true),
                sealed_fields = proplists:get_value(sealed_fields, Options, [])}.

-spec typed_object(undefined|amf_string(), [amf_kv_pair()]) -> amf_object().
typed_object(TypeName, Members) ->
    object(Members, [{class, TypeName},
                     {dynamic, false},
                     {sealed_fields, [Key || {Key,_} <- Members]}]).

-spec array([amf_kv_pair()]) -> amf_array().
array(Members) ->
    #amf_array{members = Members}.

-spec array([amf_value()], [amf_kv_pair()]) -> amf_array().
array(Values, Members) ->
    #amf_array{values = Values, members = Members}.

-spec date(erlang:timestamp()) -> amf_date().
date({_,_,_}=Timestamp) -> #amf_date{timestamp=Timestamp}.

-spec msec_to_date(non_neg_integer()) -> amf_date().
msec_to_date(MilliSecs) ->
    MegaSecs  = MilliSecs div (1000*1000*1000),
    Secs      = (MilliSecs rem (1000*1000*1000)) div 1000,
    MicroSecs = (MilliSecs rem 1000) * 1000,
    date({MegaSecs,Secs,MicroSecs}).

-spec datetime_to_date(calendar:datetime()) -> amf_date().
datetime_to_date(DateTime) ->
    Secs = 
        calendar:datetime_to_gregorian_seconds(DateTime) -
        calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
    case Secs < 0 of
        true  -> throw({too_old_datetime, DateTime});
        false -> msec_to_date(Secs*1000)
    end.

-spec xml_document(amf_string()) -> amf_xml_document().
xml_document(Xml) ->
    #amf_xml_document{data = Xml}.

-spec xml(amf_string()) -> amf_xml().
xml(Xml) ->
    #amf_xml{data = Xml}.

-spec avmplus_object(amf_value()) -> amf_avmplus_object().
avmplus_object(Value) ->
    #amf_avmplus_object{value = Value}.

-spec byte_array(binary()) -> amf_byte_array().
byte_array(Bin) ->
    #amf_byte_array{data = Bin}.

-spec vector(amf_vector_element_type(), [amf_value()]) -> amf_vector().
vector(ElementType, Elements) ->
    vector(ElementType, Elements, []).

-spec vector(amf_vector_element_type(), [amf_value()], [amf_vector_option()]) -> amf_vector().
vector(ElementType, Elements, Options) ->
    #amf_vector{type = ElementType,
                variable = proplists:get_value(variable, Options, true),
                elements = Elements}.

-spec dictionary([amf_dictionary_entry()]) -> amf_dictionary().
dictionary(Members) ->
    dictionary(Members, []).

-spec dictionary([amf_dictionary_entry()], [amf_dictionary_option()]) -> amf_dictionary().
dictionary(Members, Options) ->
    #amf_dictionary{members = Members,
                    weak = proplists:get_value(weak, Options, false)}.
