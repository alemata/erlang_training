% This file is licensed to you under the Apache License,
% Version 2.0 (the "License"); you may not use this file
% except in compliance with the License.  You may obtain
% a copy of the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing,
% software distributed under the License is distributed on an
% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
% KIND, either express or implied.  See the License for the
% specific language governing permissions and limitations
% under the License.

%% @doc First excercise: Message Passing.
%% 		== Required Knowledge ==
%%		<ul>
%%		<li>Modules and functions</li>
%%		<li>Start, register and link processes</li>
%%		<li>Send messages</li>
%%		<li>Get, send and use pids</li>
%%		</ul>
%%
%%		== Problem Statement ==
%%		Create a module called lazy_server, with the following functions:
%%		<dl>
%%		<dt>start() -> ok.</dt><dd>Starts the server</dd>
%%		<dt>stop() -> ok.</dt><dd>Stops the server</dd>
%%		<dt>wait(Ms) -> ok.</dt><dd>Sends a message to the server and returns 'ok', Ms milliseconds later</dd>
%%		</dl>
%%		The lazy_server should accept messages, and for each message it should create another process that waits
%%		for the desired number of milliseconds (the use of timer:sleep/1 is disallowed here) and then notify the
%%		original caller, which must have been blocked waiting for that message.
%%		<b>Important:</b> lazy_server should allow any number of concurrent callers.
-module(excercise_001_SUITE).
-author('elbrujohalcon@inaka.net').

-type config() :: [{atom(), term()}].
-export([all/0]).
-export([basic/1, concurrent/1]).

%% @private
-spec all() -> [atom()].
all() -> [Fun || {Fun, 1} <- module_info(exports), Fun =/= module_info].

%% @doc Basic tests. i.e. no concurrency
-spec basic(config()) -> ok.
basic(_Config) ->
	try lazy_server:wait(10) of
		_ -> throw(it_should_have_failed)
	catch
		_ -> ok
	end,
	ok = lazy_server:start(),
	{Time, ok} = timer:tc(lazy_server, wait, 100),
	true = Time > 100,
	ok = lazy_server:stop(),
	try lazy_server:wait(10) of
		_ -> throw(it_should_have_failed)
	catch
		_ -> ok
	end,
	ok.

%% @doc Concurrency tests
-spec concurrent(config()) -> ok.
concurrent(_Config) ->
	ok = lazy_server:start(),
	Self = self(),
	lists:foreach(
		fun(I) ->
			spawn_link(
				fun() ->
					{Time, ok} = timer:tc(lazy_server, wait, I),
					Self ! {Time, I}
				end)
		end, lists:seq(100, 1000, 100)),
	{FullTime, ok} =
		timer:tc(
			fun() ->
				lists:foreach(
					fun(_) ->
						receive
							{T, I} when I > T -> throw({with, I, returned_too_early, T});
							{_, _} -> ok
						end
					end, lists:seq(100, 1000, 100))
			end),
	%%NOTE: Since they all run in parallel, we should've compared the value against 1000, but we give the VM scheduler and what-not a 50% extra margin
	true = FullTime < 1500,
	ok = lazy_server:stop().