%%% -*-mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
%%% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%

-define(CONSOLE(Str, Args), io:format(Str, Args)).

-ifdef(noerlangnow).
%-compile({nowarn_deprecated_function, {erlang,now,0}}).
-define(now(), erlang:timestamp()).
-else.
-define(now(), erlang:now()).
-endif.
-undef(noerlangnow).


