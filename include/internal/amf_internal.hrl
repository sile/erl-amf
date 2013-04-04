%% Throw Exception
-define(THROW(Tag, Msg),        throw(#amf_exception{type=Tag, message=Msg})).
-define(THROW_INVALID(Msg),     ?THROW(invalid, Msg)).
-define(THROW_PARTIAL(Msg),     ?THROW(partial, Msg)).
-define(THROW_UNSUPPORTED(Msg), ?THROW(unsupported, Msg)).

%% AMF0 Value Marker
-define(AMF0_NUMBER_MARKER,         16#00).
-define(AMF0_BOOLEAN_MARKER,        16#01).
-define(AMF0_STRING_MARKER,         16#02).
-define(AMF0_OBJECT_MARKER,         16#03).
-define(AMF0_MOVIECLIP_MARKER,      16#04). % reserved, not supported
-define(AMF0_NULL_MARKER,           16#05).
-define(AMF0_UNDEFINED_MARKER,      16#06).
-define(AMF0_REFERENCE_MARKER,      16#07).
-define(AMF0_ECMA_ARRAY_MARKER,     16#08).
-define(AMF0_OBJECT_END_MARKER,     16#09).
-define(AMF0_STRICT_ARRAY_MARKER,   16#0A).
-define(AMF0_DATE_MARKER,           16#0B).
-define(AMF0_LONG_STRING_MARKER,    16#0C).
-define(AMF0_UNSUPPORTED_MARKER,    16#0D).
-define(AMF0_RECORDSET_MARKER,      16#0E). % reserved, not supported
-define(AMF0_XML_DOCUMENT_MARKER,   16#0F).
-define(AMF0_TYPED_OBJECT_MARKER,   16#10).
-define(AMF0_AVMPLUS_OBJECT_MARKER, 16#11).

%% AMF3 Value Marker
-define(AMF3_UNDEFINED_MARKER,     16#00).
-define(AMF3_NULL_MARKER,          16#01).
-define(AMF3_FALSE_MARKER,         16#02).
-define(AMF3_TRUE_MARKER,          16#03).
-define(AMF3_INTEGER_MARKER,       16#04).
-define(AMF3_DOUBLE_MARKER,        16#05).
-define(AMF3_STRING_MARKER,        16#06).
-define(AMF3_XML_DOC_MARKER,       16#07).
-define(AMF3_DATE_MARKER,          16#08).
-define(AMF3_ARRAY_MARKER,         16#09).
-define(AMF3_OBJECT_MARKER,        16#0A).
-define(AMF3_XML_MARKER,           16#0B).
-define(AMF3_BYTE_ARRAY_MARKER,    16#0C).
-define(AMF3_VECTOR_INT_MARKER,    16#0D).
-define(AMF3_VECTOR_UINT_MARKER,   16#0E).
-define(AMF3_VECTOR_DOUBLE_MARKER, 16#0F).
-define(AMF3_VECTOR_OBJECT_MARKER, 16#10).
-define(AMF3_DICTIONARY_MARKER,    16#11).

%% Reference Map Template for Decoding Process
-type decode_reference_table_index() :: non_neg_integer().
-type decode_reference_table_entry() :: {Key::decode_reference_table_index(), Value::term()}.
-type decode_reference_table()       :: {NextKey::decode_reference_table_index(), [decode_reference_table_entry()]}.

-define(DECODE_REFERENCE_TABLE_INIT(), {0, []}).

-define(DECODE_REFERENCE_TABLE_RESERVE(RefMap),
        begin
            {NextRefIndex, RefMapInternal} = RefMap,
            {NextRefIndex, {NextRefIndex+1, RefMapInternal}}
        end).

-define(DECODE_REFERENCE_TABLE_SET(RefMap, RefIndex, Value),
        begin
            {NextRefIndex, RefMapInternal} = RefMap,
            {NextRefIndex, [{RefIndex,Value} | RefMapInternal]}
        end).

-define(DECODE_REFERENCE_TABLE_GET(RefMap, RefIndex),
        begin
            {NextRefIndex, RefMapInternal} = RefMap,
            case RefIndex < NextRefIndex of
                false -> none;
                true ->
                    case lists:keyfind(RefIndex, 1, RefMapInternal) of
                        false      -> uninitialized;
                        {_, Value} -> {value, Value}
                    end
            end
        end).

-define(DECODE_REFERENCE_TABLE_ADD(RefMap, Value),
        begin
            {NextRefIndex, RefMapInternal} = RefMap,
            {NextRefIndex+1, [{NextRefIndex,Value} | RefMapInternal]}
        end).
