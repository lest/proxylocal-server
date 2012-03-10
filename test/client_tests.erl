-module(client_tests).

-include_lib("eunit/include/eunit.hrl").

splithost_test_() ->
    [?_assertEqual({<<"localhost">>, <<>>}, client:splithost("localhost")),
     ?_assertEqual({<<"localhost">>, <<>>}, client:splithost(<<"localhost">>)),
     ?_assertEqual({<<"test">>, <<"local">>}, client:splithost(<<"test.local">>)),
     ?_assertEqual({<<"test">>, <<"test.local">>}, client:splithost(<<"test.test.local">>))].
