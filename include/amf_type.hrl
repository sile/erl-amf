%% -include("amf.hrl").

-type amf_version() :: amf0 | amf3. 

-type amf_number() :: number().
-type amf_boolean() :: boolean().
-type amf_string() :: binary().
-type amf_null() :: null.
-type amf_undefined() :: undefined.

-type amf_value() :: amf_number() |
                     amf_boolean() |
                     amf_string() | 
                     amf_object() |
                     amf_null() |
                     amf_undefined() |
                     amf_ecma_array() |
                     amf_strict_array() |
                     amf_avmplus_object() |
                     amf_array() |
                     amf_vector() |
                     amf_xml_document() |
                     amf_xml() |
                     amf_byte_array() |
                     amf_dictionary() |
                     amf_date().

-type amf_object() :: #amf_object{}.

-type amf_array() :: #amf_array{}.

-type amf_date() :: #amf_date{}.

-type amf_xml_document() :: #amf_xml_document{}.

-type amf_xml() :: #amf_xml{}.

-type amf_ecma_array() :: #amf_array{values :: []}.
-type amf_strict_array() :: [amf_value()].
-type amf_avmplus_object() :: #amf_avmplus_object{}.

-type amf_vector_element_type() :: int|uint|double|amf_string().
-type amf_vector() :: #amf_vector{}.

-type amf_dictionary_entry() :: {Key::amf_value(), Value::amf_value()}.
-type amf_dictionary() :: #amf_dictionary{}.

-type amf_byte_array() :: #amf_byte_array{}.

-type amf_kv_pair() :: {Key::amf_string(), Value::amf_value()}.

-type amf_class_name() :: undefined | amf_string(). % 'undefined' indicates an anonymous class

-type amf_exception_type() :: invalid | partial | unsupported.

-type amf_exception() :: #amf_exception{}.
