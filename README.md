# ErlAMF

ErlAMF is an AMF(Action Message Format) encoding/deconding library written in Erlang.

## Version
0.1.0

## Usage
### Build

ErlAMF uses [Rebar](https://github.com/basho/rebar/) as build tool.

```sh
# build
$ git clone git://github.com/sile/erl-amf.git
$ cd erl-amf
$ rebar get-deps compile

# run unit tests
$ rebar eunit

# generate document
$ rebar edoc
```

### Examples

```erlang
$ erl -pa ebin

%%%
%%% Encode and Decode
%%%
%%% AMF3 encode
1> {ok, IoList} = amf:encode(amf3, [1,2,3]).
{ok,[9,7,[1],[[4,1],[4,2],[4,3]]]}
    
%% AMF3 decode
2> amf:decode(amf3, list_to_binary(IoList)).
{ok,[1,2,3],<<>>}
    
%%%
%%% AMF values
%%%
%% Load records
%% (Records are defined in "erl-amf/include/amf.hrl")
3> rr(amf).
[amf_array,amf_avmplus_object,amf_byte_array,amf_date,
 amf_dictionary,amf_exception,amf_object,amf_vector,amf_xml,
 amf_xml_document]
    
%% Anonymous object
4> amf:object([{<<"one">>, 1}, {<<"two">>, 2}]).
#amf_object{class = undefined,dynamic = true,
            sealed_fields = [],
            members = [{<<"one">>,1},{<<"two">>,2}]}

5> amf:encode(amf0, v(4)).
{ok,[3,
     [<<0,3>>,
      <<"one">>,
      [0,<<63,240,0,0,0,0,0,0>>],
      <<0,3>>,
      <<"two">>,
      [0,<<64,0,0,0,0,0,0,0>>]],
      0,0,9]}

%% Date
6> amf:encode(amf0, amf:date(now())).
{ok,[11,<<66,115,221,102,242,183,208,0,0,0>>]}

%%%
%%% Error
%%%
%% The encode/2(and decode/2) function returns an amf_exception() instance as error reason
7> amf:encode(amf0, can_not_encode).
{error,#amf_exception{type = unsupported,
                      message = {value,can_not_encode}}}
    
8> amf:decode(amf0, <<>>).
{error,#amf_exception{type = partial,
                      message = {marker,<<>>}}}
```

## API

See: EDoc document

## Mapping between Erlang values and AMF values

TODO

## Limitations
ErlAMF doesn't support following features:
* Circular reference objects
* flash.utils.IExternalizable interface
* No Object/Trait/String reference is used in encoding process
* Special double value (e.g. NaN, Infinity)

## Reference
* [AMF0 Specification](http://download.macromedia.com/pub/labs/amf/amf0_spec_121207.pdf)
* [AMF3 Specification](http://wwwimages.adobe.com/www.adobe.com/content/dam/Adobe/en/devnet/amf/pdf/amf-file-format-spec.pdf)
