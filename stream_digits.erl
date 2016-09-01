#!/usr/bin/env escript
%%!
%% -*- coding: utf-8 -*-
%% Copyright © 2016 Pierre Fenoll ‹pierrefenoll@gmail.com›
%% See LICENSE for licensing information.
-module(stream_digits).
-mode('compile').
-compile([debug_info]).

%% stream_digits: produce binary data on stdout.

-export([main/1]).

-type data() :: any().
-type state() :: #{yielder => fun((data()) -> data())
                  ,data => data()
                  ,succ => fun((state()) -> state())
                  }.


%% API

main ([]) ->
    io:format(standard_error, "Usage: ./stream_digits  <sqrt | rand | file/name.txt>\n", []),
    halt(1);
main ([Arg]) ->
    loop(init(Arg)).

%% Internals

init ("sqrt") ->
    #{yielder => fun sqrt/1
     ,data => 1
     ,succ => fun sqrt_succ/1
     };
init ("rand") ->
    _ = rand:seed(exs1024, {123, 123534, 345345}),
    #{yielder => fun (_) -> rand:uniform() end
     ,data => undefined
     ,succ => fun id/1
     };
init (File) ->
    case file:open(File, [read, binary, raw]) of
        {ok, Dev} ->
            #{yielder => fun translate/1
             ,data => Dev
             ,succ => fun id/1
             };
        _Error -> fail("~s: ~p", [File, _Error])
    end.

loop (State=#{yielder := Yield
             ,succ := Succ
             }) ->
    emit(Yield(State)),
    loop(Succ(State)).

emit (Float) when is_float(Float) ->
    Bin = term_to_binary(Float),
    io:format("~s", [Bin]);
emit (Byte) when is_integer(Byte), Byte =< 0 ->
    Byte + 256;
emit (Byte) when is_integer(Byte), Byte > 255 ->
    Byte - 256;
emit (Byte) when is_integer(Byte), Byte < 256 ->
    io:format("~s", [[Byte]]).


id (A) -> A.

sqrt (#{data := N}) ->
    math:sqrt(N).
sqrt_succ (State) ->
    State#{data => 1 + maps:get(data, State)}.

translate (#{data := Device}) ->
    case file:read(Device, 2) of
        {ok, <<High, Low>>}
          when $0 =< High, High =< $9,
               $0 =< Low, Low =< $9 ->
            coalesce($9, $0, High, Low);
        {ok, <<High, Low>>}
          when $a =< High, High =< $z,
               $a =< Low, Low =< $z ->
            coalesce($z, $a, High, Low);
        _ -> halt(0)
    end.

coalesce (Top, Bottom, High, Low) ->
    (Top + 1 - Bottom) * (High - Bottom) + Low - Bottom.

fail (Fmt, Args) ->
    io:format(standard_error, Fmt++"\n", Args),
    halt(1).

%% End of Module.
