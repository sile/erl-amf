-record(amf_object,
        {
          class         = undefined :: undefined | amf:amf_string(),
          dynamic       = true      :: amf:amf_boolean(),
          sealed_fields = []        :: [amf:amf_string()],
          members       = []        :: [amf:amf_kv_pair()]
        }).

-record(amf_array,
        {
          values  = [] :: [amf:amf_value()],
          members = [] :: [amf:amf_kv_pair()]
        }).

-record(amf_date,
        {
          timestamp = {0,0,0} :: erlang:timestamp()
        }).

-record(amf_xml_document,
        {
          data = <<"">> :: amf:amf_string()
        }).

-record(amf_xml,
        {
          data = <<"">> :: amf:amf_string()
        }).

-record(amf_avmplus_object,
        {
          value = undefined :: amf:amf_value()
        }).

-record(amf_byte_array,
        {
          data = <<"">> :: binary()
        }).

-record(amf_vector,
        {
          type = <<"*">>  :: amf:amf_vector_element_type(),
          variable = true :: amf:amf_boolean(),
          elements = []   :: [amf:amf_value()]
        }).

-record(amf_dictionary,
        {
          members = [] :: [amf:amf_dictionary_entry()],
          weak = false :: amf:amf_boolean()
        }).

-record(amf_exception,
        {
          type    = invalid :: amf:amf_exception_type(),
          message = undefined :: any()
        }).
