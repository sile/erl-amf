

# Module amf #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

AMF encode/decode functions and AMF value constructors.

__References__* AMF0 specification: [`http://download.macromedia.com/pub/labs/amf/amf0_spec_121207.pdf`](http://download.macromedia.com/pub/labs/amf/amf0_spec_121207.pdf)
* AMF3 specification: [`http://wwwimages.adobe.com/www.adobe.com/content/dam/Adobe/en/devnet/amf/pdf/amf-file-format-spec.pdf`](http://wwwimages.adobe.com/www.adobe.com/content/dam/Adobe/en/devnet/amf/pdf/amf-file-format-spec.pdf)

<a name="types"></a>

## Data Types ##




### <a name="type-amf_array">amf_array()</a> ###


<pre><code>
amf_array() = #amf_array{values = [<a href="amf.md#type-amf_value">amf:amf_value()</a>], members = [<a href="amf.md#type-amf_kv_pair">amf:amf_kv_pair()</a>]}
</code></pre>




### <a name="type-amf_avmplus_object">amf_avmplus_object()</a> ###


<pre><code>
amf_avmplus_object() = #amf_avmplus_object{value = <a href="amf.md#type-amf_value">amf:amf_value()</a>}
</code></pre>




### <a name="type-amf_boolean">amf_boolean()</a> ###


<pre><code>
amf_boolean() = boolean()
</code></pre>




### <a name="type-amf_byte_array">amf_byte_array()</a> ###


<pre><code>
amf_byte_array() = #amf_byte_array{data = binary()}
</code></pre>




### <a name="type-amf_class_name">amf_class_name()</a> ###


<pre><code>
amf_class_name() = undefined | <a href="#type-amf_string">amf_string()</a>
</code></pre>


```
    'undefined' indicates an anonymous class
```



### <a name="type-amf_date">amf_date()</a> ###


<pre><code>
amf_date() = #amf_date{timestamp = <a href="erlang.md#type-timestamp">erlang:timestamp()</a>}
</code></pre>




### <a name="type-amf_dictionary">amf_dictionary()</a> ###


<pre><code>
amf_dictionary() = #amf_dictionary{members = [<a href="amf.md#type-amf_dictionary_entry">amf:amf_dictionary_entry()</a>], weak = <a href="amf.md#type-amf_boolean">amf:amf_boolean()</a>}
</code></pre>




### <a name="type-amf_dictionary_entry">amf_dictionary_entry()</a> ###


<pre><code>
amf_dictionary_entry() = {Key::<a href="#type-amf_value">amf_value()</a>, Value::<a href="#type-amf_value">amf_value()</a>}
</code></pre>




### <a name="type-amf_dictionary_option">amf_dictionary_option()</a> ###


<pre><code>
amf_dictionary_option() = {weak, boolean()}
</code></pre>




### <a name="type-amf_ecma_array">amf_ecma_array()</a> ###


<pre><code>
amf_ecma_array() = #amf_array{values = [], members = [<a href="amf.md#type-amf_kv_pair">amf:amf_kv_pair()</a>]}
</code></pre>




### <a name="type-amf_exception">amf_exception()</a> ###


<pre><code>
amf_exception() = #amf_exception{type = <a href="amf.md#type-amf_exception_type">amf:amf_exception_type()</a>, message = any()}
</code></pre>




### <a name="type-amf_exception_type">amf_exception_type()</a> ###


<pre><code>
amf_exception_type() = invalid | partial | unsupported
</code></pre>

```
  invalid:     Input is something wrong.
  partial:     Input binary is too small to complete decoding.
  unsupported: Some unsupported feature is needed to proceed.
```



### <a name="type-amf_kv_pair">amf_kv_pair()</a> ###


<pre><code>
amf_kv_pair() = {Key::<a href="#type-amf_string">amf_string()</a>, Value::<a href="#type-amf_value">amf_value()</a>}
</code></pre>




### <a name="type-amf_null">amf_null()</a> ###


<pre><code>
amf_null() = null
</code></pre>




### <a name="type-amf_number">amf_number()</a> ###


<pre><code>
amf_number() = number()
</code></pre>




### <a name="type-amf_object">amf_object()</a> ###


<pre><code>
amf_object() = #amf_object{class = <a href="amf.md#type-amf_class_name">amf:amf_class_name()</a>, dynamic = <a href="amf.md#type-amf_boolean">amf:amf_boolean()</a>, sealed_fields = [<a href="amf.md#type-amf_string">amf:amf_string()</a>], members = [<a href="amf.md#type-amf_kv_pair">amf:amf_kv_pair()</a>]}
</code></pre>




### <a name="type-amf_object_option">amf_object_option()</a> ###


<pre><code>
amf_object_option() = {class, <a href="#type-amf_class_name">amf_class_name()</a>} | {dynamic, boolean()} | {sealed_fields, [<a href="#type-amf_string">amf_string()</a>]}
</code></pre>




### <a name="type-amf_strict_array">amf_strict_array()</a> ###


<pre><code>
amf_strict_array() = [<a href="#type-amf_value">amf_value()</a>]
</code></pre>




### <a name="type-amf_string">amf_string()</a> ###


<pre><code>
amf_string() = binary()
</code></pre>




### <a name="type-amf_undefined">amf_undefined()</a> ###


<pre><code>
amf_undefined() = undefined
</code></pre>




### <a name="type-amf_value">amf_value()</a> ###


<pre><code>
amf_value() = <a href="#type-amf_number">amf_number()</a> | <a href="#type-amf_boolean">amf_boolean()</a> | <a href="#type-amf_string">amf_string()</a> | <a href="#type-amf_object">amf_object()</a> | <a href="#type-amf_null">amf_null()</a> | <a href="#type-amf_undefined">amf_undefined()</a> | <a href="#type-amf_ecma_array">amf_ecma_array()</a> | <a href="#type-amf_strict_array">amf_strict_array()</a> | <a href="#type-amf_avmplus_object">amf_avmplus_object()</a> | <a href="#type-amf_array">amf_array()</a> | <a href="#type-amf_vector">amf_vector()</a> | <a href="#type-amf_xml_document">amf_xml_document()</a> | <a href="#type-amf_xml">amf_xml()</a> | <a href="#type-amf_byte_array">amf_byte_array()</a> | <a href="#type-amf_dictionary">amf_dictionary()</a> | <a href="#type-amf_date">amf_date()</a>
</code></pre>




### <a name="type-amf_vector">amf_vector()</a> ###


<pre><code>
amf_vector() = #amf_vector{type = <a href="amf.md#type-amf_vector_element_type">amf:amf_vector_element_type()</a>, variable = <a href="amf.md#type-amf_boolean">amf:amf_boolean()</a>, elements = [<a href="amf.md#type-amf_value">amf:amf_value()</a>]}
</code></pre>




### <a name="type-amf_vector_element_type">amf_vector_element_type()</a> ###


<pre><code>
amf_vector_element_type() = int | uint | double | (ClassName::<a href="#type-amf_string">amf_string()</a>)
</code></pre>




### <a name="type-amf_vector_option">amf_vector_option()</a> ###


<pre><code>
amf_vector_option() = {variable, boolean()}
</code></pre>




### <a name="type-amf_version">amf_version()</a> ###


<pre><code>
amf_version() = amf0 | amf3
</code></pre>




### <a name="type-amf_xml">amf_xml()</a> ###


<pre><code>
amf_xml() = #amf_xml{data = <a href="amf.md#type-amf_string">amf:amf_string()</a>}
</code></pre>




### <a name="type-amf_xml_document">amf_xml_document()</a> ###


<pre><code>
amf_xml_document() = #amf_xml_document{data = <a href="amf.md#type-amf_string">amf:amf_string()</a>}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#array-1">array/1</a></td><td>Make associative array.</td></tr><tr><td valign="top"><a href="#array-2">array/2</a></td><td>Make generic array.</td></tr><tr><td valign="top"><a href="#avmplus_object-1">avmplus_object/1</a></td><td>Make AvmPlus Object.</td></tr><tr><td valign="top"><a href="#byte_array-1">byte_array/1</a></td><td>Make ByteArray.</td></tr><tr><td valign="top"><a href="#date-1">date/1</a></td><td>Make date.</td></tr><tr><td valign="top"><a href="#datetime_to_date-1">datetime_to_date/1</a></td><td>Make date from datetime.</td></tr><tr><td valign="top"><a href="#decode-2">decode/2</a></td><td>Decode AMF0/AMF3 binary data.</td></tr><tr><td valign="top"><a href="#dictionary-1">dictionary/1</a></td><td>Make dictionary.</td></tr><tr><td valign="top"><a href="#dictionary-2">dictionary/2</a></td><td>Make dictionary.</td></tr><tr><td valign="top"><a href="#encode-2">encode/2</a></td><td>Encode AMF0/AMF3 value.</td></tr><tr><td valign="top"><a href="#msec_to_date-1">msec_to_date/1</a></td><td>Make date from timestamp in milliseconds.</td></tr><tr><td valign="top"><a href="#object-1">object/1</a></td><td>Make anonymous object.</td></tr><tr><td valign="top"><a href="#object-2">object/2</a></td><td>Make object.</td></tr><tr><td valign="top"><a href="#typed_object-2">typed_object/2</a></td><td>Make typed-object.</td></tr><tr><td valign="top"><a href="#vector-2">vector/2</a></td><td>Make variable vector.</td></tr><tr><td valign="top"><a href="#vector-3">vector/3</a></td><td>Make vector.</td></tr><tr><td valign="top"><a href="#xml-1">xml/1</a></td><td>Make XML.</td></tr><tr><td valign="top"><a href="#xml_document-1">xml_document/1</a></td><td>Make XML Document.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="array-1"></a>

### array/1 ###

<pre><code>
array(Members) -&gt; <a href="#type-amf_array">amf_array()</a>
</code></pre>

<ul class="definitions"><li><code>Members = [<a href="#type-amf_kv_pair">amf_kv_pair()</a>]</code></li></ul>

Make associative array.

<a name="array-2"></a>

### array/2 ###

<pre><code>
array(Values, Members) -&gt; <a href="#type-amf_array">amf_array()</a>
</code></pre>

<ul class="definitions"><li><code>Values = [<a href="#type-amf_value">amf_value()</a>]</code></li><li><code>Members = [<a href="#type-amf_kv_pair">amf_kv_pair()</a>]</code></li></ul>

Make generic array.

<a name="avmplus_object-1"></a>

### avmplus_object/1 ###

<pre><code>
avmplus_object(AmfValue) -&gt; <a href="#type-amf_avmplus_object">amf_avmplus_object()</a>
</code></pre>

<ul class="definitions"><li><code>AmfValue = <a href="#type-amf_value">amf_value()</a></code></li></ul>

Make AvmPlus Object.

<a name="byte_array-1"></a>

### byte_array/1 ###

<pre><code>
byte_array(Bytes) -&gt; <a href="#type-amf_byte_array">amf_byte_array()</a>
</code></pre>

<ul class="definitions"><li><code>Bytes = binary()</code></li></ul>

Make ByteArray.

<a name="date-1"></a>

### date/1 ###

<pre><code>
date(Timestamp) -&gt; <a href="#type-amf_date">amf_date()</a>
</code></pre>

<ul class="definitions"><li><code>Timestamp = <a href="erlang.md#type-timestamp">erlang:timestamp()</a></code></li></ul>

Make date.

<a name="datetime_to_date-1"></a>

### datetime_to_date/1 ###

<pre><code>
datetime_to_date(DateTime) -&gt; <a href="#type-amf_date">amf_date()</a>
</code></pre>

<ul class="definitions"><li><code>DateTime = <a href="calendar.md#type-datetime">calendar:datetime()</a></code></li></ul>

Make date from datetime.

<a name="decode-2"></a>

### decode/2 ###

<pre><code>
decode(AmfVersion, EncodedBytes) -&gt; {ok, DecodedValue, UnconsumedBytes} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>AmfVersion = <a href="#type-amf_version">amf_version()</a></code></li><li><code>EncodedBytes = binary()</code></li><li><code>DecodedValue = <a href="#type-amf_value">amf_value()</a></code></li><li><code>UnconsumedBytes = binary()</code></li><li><code>Reason = <a href="#type-amf_exception">amf_exception()</a></code></li></ul>

Decode AMF0/AMF3 binary data.

<a name="dictionary-1"></a>

### dictionary/1 ###

<pre><code>
dictionary(Entries) -&gt; <a href="#type-amf_dictionary">amf_dictionary()</a>
</code></pre>

<ul class="definitions"><li><code>Entries = [<a href="#type-amf_dictionary_entry">amf_dictionary_entry()</a>]</code></li></ul>

Make dictionary.

<a name="dictionary-2"></a>

### dictionary/2 ###

<pre><code>
dictionary(Entries, Options) -&gt; <a href="#type-amf_dictionary">amf_dictionary()</a>
</code></pre>

<ul class="definitions"><li><code>Entries = [<a href="#type-amf_dictionary_entry">amf_dictionary_entry()</a>]</code></li><li><code>Options = [<a href="#type-amf_dictionary_option">amf_dictionary_option()</a>]</code></li></ul>

Make dictionary.

<a name="encode-2"></a>

### encode/2 ###

<pre><code>
encode(AmfVersion, AmfValue) -&gt; {ok, EncodedData} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>AmfVersion = <a href="#type-amf_version">amf_version()</a></code></li><li><code>AmfValue = <a href="#type-amf_value">amf_value()</a></code></li><li><code>EncodedData = iolist()</code></li><li><code>Reason = <a href="#type-amf_exception">amf_exception()</a></code></li></ul>

Encode AMF0/AMF3 value.

<a name="msec_to_date-1"></a>

### msec_to_date/1 ###

<pre><code>
msec_to_date(MilliSeconds) -&gt; <a href="#type-amf_date">amf_date()</a>
</code></pre>

<ul class="definitions"><li><code>MilliSeconds = non_neg_integer()</code></li></ul>

Make date from timestamp in milliseconds

<a name="object-1"></a>

### object/1 ###

<pre><code>
object(Members) -&gt; <a href="#type-amf_object">amf_object()</a>
</code></pre>

<ul class="definitions"><li><code>Members = [<a href="#type-amf_kv_pair">amf_kv_pair()</a>]</code></li></ul>

Make anonymous object.

<a name="object-2"></a>

### object/2 ###

<pre><code>
object(Members, Options) -&gt; <a href="#type-amf_object">amf_object()</a>
</code></pre>

<ul class="definitions"><li><code>Members = [<a href="#type-amf_kv_pair">amf_kv_pair()</a>]</code></li><li><code>Options = [<a href="#type-amf_object_option">amf_object_option()</a>]</code></li></ul>

Make object.

<a name="typed_object-2"></a>

### typed_object/2 ###

<pre><code>
typed_object(TypeName, Members) -&gt; <a href="#type-amf_object">amf_object()</a>
</code></pre>

<ul class="definitions"><li><code>TypeName = <a href="#type-amf_class_name">amf_class_name()</a></code></li><li><code>Members = [<a href="#type-amf_kv_pair">amf_kv_pair()</a>]</code></li></ul>

Make typed-object.

<a name="vector-2"></a>

### vector/2 ###

<pre><code>
vector(ElementType, Elements) -&gt; <a href="#type-amf_vector">amf_vector()</a>
</code></pre>

<ul class="definitions"><li><code>ElementType = <a href="#type-amf_vector_element_type">amf_vector_element_type()</a></code></li><li><code>Elements = [<a href="#type-amf_value">amf_value()</a>]</code></li></ul>

Make variable vector.

<a name="vector-3"></a>

### vector/3 ###

<pre><code>
vector(ElementType, Elements, Options) -&gt; <a href="#type-amf_vector">amf_vector()</a>
</code></pre>

<ul class="definitions"><li><code>ElementType = <a href="#type-amf_vector_element_type">amf_vector_element_type()</a></code></li><li><code>Elements = [<a href="#type-amf_value">amf_value()</a>]</code></li><li><code>Options = [<a href="#type-amf_vector_option">amf_vector_option()</a>]</code></li></ul>

Make vector.

<a name="xml-1"></a>

### xml/1 ###

<pre><code>
xml(Xml) -&gt; <a href="#type-amf_xml">amf_xml()</a>
</code></pre>

<ul class="definitions"><li><code>Xml = <a href="#type-amf_string">amf_string()</a></code></li></ul>

Make XML.

<a name="xml_document-1"></a>

### xml_document/1 ###

<pre><code>
xml_document(Xml) -&gt; <a href="#type-amf_xml_document">amf_xml_document()</a>
</code></pre>

<ul class="definitions"><li><code>Xml = <a href="#type-amf_string">amf_string()</a></code></li></ul>

Make XML Document.

