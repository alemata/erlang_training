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
%%		<li>supervisor and gen_server OTP behaviors</li>
%%		</ul>
%%
%%		== Problem Statement ==
%%		Create the required process structure and modules to provide a very simple in-memory database.
%%		The db interface will be provided by the dumb_db module, with the following functions:
%%		<dl>
%%		<dt>start() -> ok.</dt><dd>Starts the db server</dd>
%%		<dt>stop() -> ok.</dt><dd>Stops the db server</dd>
%%		<dt>add(Key::atom(), Value::term()) -> ok | {error, already_present}.</dt><dd>Adds an object to the database</dd>
%%		<dt>del(Key) -> ok.</dt><dd>Removes an object from the database, if it was already deleted no error should be reported</dd>
%%		<dt>upd(Key::atom(), Fun::fun((Value::term()) -> NewValue)) -> {ok, NewValue} | {error, not_found}</dt>
%%			<dd>Updates the value stored at Key by running the function Fun on it and returns the resulting value.
%%				Note that exceptions in the function *must* be propagated to the caller but the item should be kept in the db</dd>
%%		<dt>get(Key::atom()) -> {ok, term()} | {error, not_found}</dt><dd>Retrieves the value stored at Key</dd>
%%		</dl>
%%		There're many many ways to implement dumb_db. The only off-tests requirements are the use of at least one supervisor and one gen_server, but no ets or other external db tools.
%%		The <i>recommended</i> way to implement it is using one gen_server per stored value.
-module(excercise_002_SUITE).
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
	try dumb_db:add(x, 1) of
		_ -> throw(it_should_have_failed)
	catch
		_:_ -> ok
	end,
	ok = dumb_db:start(),
	try dumb_db:start() of
		_ -> throw(it_should_have_failed)
	catch
		_:_ -> ok
	end,
	{error, not_found} = dumb_db:get(x),
	ok = dumb_db:add(x, 0),
	{error, already_present} = dumb_db:add(x, 0),
	{ok, 0} = dumb_db:get(x),
	{ok, 1} = dumb_db:upd(x, fun(X) -> X + 1 end),
	ok = dumb_db:del(x),
	timer:sleep(100), %% dumb_db is not required to delete the value right away... a brief delay is ok
	{error, not_found} = dumb_db:get(x),
	{error, not_found} = dumb_db:upd(x, fun(X) -> X - 1 end),

	ok = dumb_db:add(y, an_atom),
	{ok, an_atom} = dumb_db:get(y),
	try dumb_db:upd(y, fun(X) -> X + 1 end) of
		_ -> throw(it_should_have_failed)
	catch
		_:E0 -> badarg = E0
	end,
	{ok, an_atom} = dumb_db:get(y),
	ok = dumb_db:stop(),
	timer:sleep(100), %% dumb_db is not required to stop right away... a brief delay is ok
	try dumb_db:add(x, 1) of
		_ -> throw(it_should_have_failed)
	catch
		_:_ -> ok
	end,
	ok.

%% @doc Concurrency tests
-spec concurrent(config()) -> ok.
concurrent(_Config) ->
	ok = dumb_db:start(),
	Self = self(),
	lists:foreach(
		fun(C) ->
			spawn_link(
				fun() ->
					K = list_to_atom([C]),
					ok = dumb_db:add(K, C),
					Self ! dumb_db:upd(K, fun(X) -> timer:sleep(1000), {f, X} end)
				end)
		end, lists:seq($a, $j)),
	lists:foreach(
		fun(C) ->
			spawn_link(
				fun() ->
					K = list_to_atom([C]),
					ok = dumb_db:add(K, C),
					Self ! dumb_db:upd(K, fun({f, X}) -> timer:sleep(1000), {g, X} end)
				end)
		end, lists:seq($a, $j)),
	%%NOTE: We run the same thing twice, to ensure that each key has 2 requests that are applied consecutively
	{FullTime, ok} =
		timer:tc(
			fun() ->
				lists:foreach(
					fun(_) ->
						receive
							{ok, {f, _}} -> ok;
							{ok, {g, _}} -> ok;
							Error -> throw(Error)
						end
					end, lists:seq(1, 20))
			end),
	%%NOTE: Since they all run in parallel, we should've compared the value against 2000ms, but we give the VM scheduler and what-not a 50% extra margin
	true = FullTime < 3000000,
	ok = dumb_db:stop().