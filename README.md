# ErlAMF
ErlAMF is an AMF(Action Message Format) encoding/deconding library written in Erlang.

## Version
0.1.0

## Usage
### Install

    $ git clone git://github.com/sile/erl-amf.git
    $ cd erl-amf
    $ rebar get-deps compile

### Example

    $ erl -pa ebin
    %%% Encode and Decode
    %% AMF3 encode
    1> {ok, IoList} = amf:encode(amf3, [1,2,3]).
    {ok,[9,<<7>>,[<<1>>,<<>>],[[4,<<1>>],[4,<<2>>],[4,<<3>>]]]}
    
    %% AMF3 decode
    2> amf:decode(amf3, list_to_binary(IoList)).
    {ok,[1,2,3],<<>>}
    
    %%% AMF values
    
    %% Load records
    %% # They are defined in "erl-amf/include/amf.hrl"
    3> rr(amf).
    [amf_array,amf_avmplus_object,amf_byte_array,amf_date,
     amf_dictionary,amf_exception,amf_object,amf_vector,amf_xml,
     amf_xml_document]
    
    %%% Error

## API
### Decode/Encode Functions

### AMF value Construct Functions

### Exception (Error Reason)

## Mapping between Erlang values and AMF values

## Limitations
