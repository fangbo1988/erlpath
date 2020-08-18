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

Nonterminals
list elements element index value attr.

Terminals
'.' '[' ']' '=' atom integer binary.

Rootsymbol list.

list -> elements : '$1'.

elements -> element : [{elem, '$1'}].
elements -> element index : [{elem, '$1'}] ++ '$2'.
elements -> element '.' elements : [{elem, '$1'}] ++ '$3'.

index -> '[' attr '=' value ']' : [{attr, {'$2', '$4'}}].
index -> '[' attr '=' value ']' '.' elements : [{attr, {'$2', '$4'}}] ++ '$7'.

element -> atom : unwrap('$1').
attr -> atom : unwrap('$1').
value -> integer : unwrap('$1').
value -> atom : unwrap('$1').
value -> binary : unwrap('$1').

Erlang code.
unwrap({_,_,V}) -> V.