%% Licensed to the Apache Software Foundation (ASF) under one
%% or more contributor license agreements.  See the NOTICE file
%% distributed with this work for additional information
%% regarding copyright ownership.  The ASF licenses this file
%% to you under the Apache License, Version 2.0 (the
%% "License"); you may not use this file except in compliance
%% with the License.  You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.

%% @author: fangbo.su@outlook.com

Definitions.

%% Characters for keys
INTEGER = [0-9]+
VARIABLE = [A-Z_][0-9a-zA-Z_]*
ATOM = [a-z][0-9a-zA-Z_]*
BINARY = [0-9a-zA-Z_\s]*

%% Spaces, breaks and comment delimiters
SPACE = [\s\t]
BREAK = (\n|\r|\n\r)
COMMENT = [;#%]

%% Quotation mark
%% Equal to QUOTA = [\'\"]
QUOTA = (\'|\")

Rules.

%% skip empty or blank lines or lines with space/tab chars
%% {BREAK}({SPACE}*{BREAK})+    : {token, {break,   TokenLine}}.
{BREAK}({SPACE}*{BREAK})+       : skip_token.

%% mark line break by token 'break' in order to use as delimiters
%% {BREAK}                      : {token, {break,   TokenLine}}.
{BREAK}                         : skip_token.

%% Just chars
=                               : {token, {'=',     TokenLine}}.
\[                              : {token, {'[',     TokenLine}}.
\]                              : {token, {']',     TokenLine}}.
\.                              : {token, {'.',     TokenLine}}.

%% word-like tokens
{INTEGER}                       : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
{ATOM}                          : {token, {atom,    TokenLine, list_to_atom(TokenChars)}}.
%% {SPACE}+                     : {token, {blank,   TokenLine, TokenChars}}.
{SPACE}+                        : skip_token.
{QUOTA}{BINARY}{QUOTA}          : String = strip(TokenChars, TokenLen),
                                  {token, {binary,  TokenLine, list_to_binary(String)}}.
{VARIABLE}                      : {token, {variable,TokenLine, TokenChars}}.

%% comment-like token, but may be a part of value depending on the location
%% {COMMENT}.*                  : {token, {comment, TokenLine, TokenChars}}.
{COMMENT}.*                     : skip_token.

Erlang code.
strip(TokenChars, TokenLen) ->
    lists:sublist(TokenChars, 2, TokenLen - 2).