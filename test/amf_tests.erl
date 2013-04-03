-module(amf_tests).

-include_lib("eunit/include/eunit.hrl").

decode_test_() ->
    {setup,
     fun () ->
             meck:new(amf0_decode, [no_passthrough_cover]),
             meck:new(amf3_decode, [no_passthrough_cover])
     end,
     fun (_) -> 
             meck:unload(amf0_decode),
             meck:unload(amf3_decode)
     end,
     [
      fun () ->
              meck:expect(amf0_decode, decode, 1, {amf0_dummy_value, <<"">>}),
              meck:expect(amf3_decode, decode, 1, {amf3_dummy_value, <<"">>}),
              Input = <<"">>,

              ?assertMatch({amf0_dummy_value, <<"">>}, amf:decode(amf0, Input)),
              ?assertMatch({amf3_dummy_value, <<"">>}, amf:decode(amf3, Input))
      end
     ]}.

encode_test_() ->
    {setup,
     fun () ->
             meck:new(amf0_encode, [no_passthrough_cover]),
             meck:new(amf3_encode, [no_passthrough_cover])
     end,
     fun (_) -> 
             meck:unload(amf0_encode),
             meck:unload(amf3_encode)
     end,
     [
      fun () ->
              meck:expect(amf0_encode, encode, 1, <<"amf0_dummy_value">>),
              meck:expect(amf3_encode, encode, 1, <<"amf3_dummy_value">>),
              Input = 10,

              ?assertMatch(<<"amf0_dummy_value">>, amf:encode(amf0, Input)),
              ?assertMatch(<<"amf3_dummy_value">>, amf:encode(amf3, Input))
      end
     ]}.

encode_to_iolist_test_() ->
    {setup,
     fun () ->
             meck:new(amf0_encode, [no_passthrough_cover]),
             meck:new(amf3_encode, [no_passthrough_cover])
     end,
     fun (_) -> 
             meck:unload(amf0_encode),
             meck:unload(amf3_encode)
     end,
     [
      fun () ->
              meck:expect(amf0_encode, encode_to_iolist, 1, <<"amf0_dummy_value">>),
              meck:expect(amf3_encode, encode_to_iolist, 1, <<"amf3_dummy_value">>),
              Input = 10,

              ?assertMatch(<<"amf0_dummy_value">>, amf:encode_to_iolist(amf0, Input)),
              ?assertMatch(<<"amf3_dummy_value">>, amf:encode_to_iolist(amf3, Input))
      end
     ]}.
