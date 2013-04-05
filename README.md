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
     [<<0,3>>, <<"one">>,
      [0,<<63,240,0,0,0,0,0,0>>],
      <<0,3>>, <<"two">>,
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

## Mapping between Erlang Values and AMF types(markers)

<table>
<tr><th>Erlang Value</th><th>AMF0 Marker</th><th>AMF3 Marker</th></tr>
<tr><td>10</td><td>number</td><td>integer</td></tr>
<tr><td>10.5</td><td>number</td><td>double</td></tr>
<tr><td>true</td><td>boolean</td><td>true</td></tr>
<tr><td>false</td><td>boolean</td><td>false</td></tr>
<tr><td>null</td><td>null</td><td>null</td></tr>
<tr><td>undefined</td><td>undefined</td><td>undefined</td></tr>
<tr><td>&lt;&lt;"string"&gt;&gt;</td><td>string | long-string</td><td>string</td></tr>
<tr><td></td><td>movieclip</td><td></td></tr>
<tr><td></td><td>recordset</td><td></td></tr>
<tr><td></td><td>unsupported</td><td></td></tr>
<tr><td>amf:date(now())</td><td>date</td><td>date</td></tr>
<tr><td>amf:xml(&lt;&lt;"xml"&gt;&gt;)</td><td></td><td>xml</td></tr>
<tr><td>amf:xml_document(&lt;&lt;"xml"&gt;&gt;)</td><td>xml-document</td><td>xml-doc</td></tr>
<tr><td>amf:byte_array(&lt;&lt;"bytes"&gt;&gt;)</td><td></td><td>byte-array</td></tr>
<tr><td>amf:vector(int, [-1,2,-3])</td><td></td><td>vector-int</td></tr>
<tr><td>amf:vector(uint, [1,2,3])</td><td></td><td>vector-uint</td></tr>
<tr><td>amf:vector(double, [-1.0,2.0,-3.0])</td><td></td><td>vector-double</td></tr>
<tr><td>amf:vector(<<"type_name">>, [1,&lt;&lt;"2"&gt;&gt;,[3]])</td><td></td><td>vector-object</td></tr>
<tr><td>[1,2,3]</td><td>strict-array</td><td>array (only dense element)</td></tr>
<tr><td>amf:array([{&lt;&lt;"key"&gt;&gt;, &lt;&lt;"value"&gt;&gt;}])</td><td>ecma-array</td><td>array (only associative element)</td></tr>
<tr><td>amf:array([1,2,3], [{&lt;&lt;"key"&gt;&gt;, &lt;&lt;"value"&gt;&gt;}])</td><td></td><td>array (mixed)</td></tr>
<tr><td>amf:object([{&lt;&lt;"key"&gt;&gt;, &lt;&lt;"value"&gt;&gt;}])</td><td>object</td><td>object (anonymous)</td></tr>
<tr><td>amf:typed_object(&lt;&lt;"type"&gt;&gt;, [{&lt;&lt;"key"&gt;&gt;, &lt;&lt;"value"&gt;&gt;}])</td><td>typed-object</td><td>object (typed)</td></tr>
<tr><td>amf:dictionary([{1,2}, {true, false}])</td><td></td><td>dictionary</td></tr>
<tr><td>amf:avmplus_object([1,2,3])</td><td>avmplus-object</td><td></td></tr>
</table>

## Limitations
ErlAMF doesn't support following features:
* Circular reference objects
* flash.utils.IExternalizable interface
* No Object/Trait/String reference is used in encoding process
* Special double value (e.g. NaN, Infinity)

## Reference
* [AMF0 Specification](http://download.macromedia.com/pub/labs/amf/amf0_spec_121207.pdf)
* [AMF3 Specification](http://wwwimages.adobe.com/www.adobe.com/content/dam/Adobe/en/devnet/amf/pdf/amf-file-format-spec.pdf)
