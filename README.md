# erlpath - An Erlang Path parser

[![Build Status](https://travis-ci.org/fangbo1988/erlpath.svg?branch=master)](https://travis-ci.org/fangbo1988/erlpath)
![CI](https://github.com/fangbo1988/erlpath/workflows/CI/badge.svg?branch=master)

An erlang path parser, do lookup/update/delete on a erlang list.

# Example
See test suite for more details.

## Parse
```
{ok, Result} = erlpath:parse("a.b[i=1].c").
```
Result should be
```
[{elem,a},{elem,b},{attr,{i,1}},{elem,c}]
```

## Lookup
```
get(a, 1) -> [{b,[[{i,1},{v,100},{s,2}],[{i,2},{v,200},{s,4}]]}, {c,[[{i,1},{v,300},{s,5}],[{i,2},{v,400},{s,7}]]}].
    
{ok, Result} = erlpath:lookup("a.b[i=1].v", {?MODULE, get, [1]}).
```
Result should be
```
100
```

## Update
```
get(a, 2) -> [{b,[[{i,1},c,{c,3}],[{i,2},c,{c,3}]]}].

{ok, a, Result} = update("a.b[i=1].c", 103, {?MODULE, get, [2]}).
```
Result should be
```
[{b,[[{i,1},c,{c,103}],[{i,2},c,{c,3}]]}]
```

## Delete
```
get(a, 3) -> [{b,[[{i,1},c,{c,3}],[{i,2},c,{c,3}]]},c].

{ok, a, Result} = delete("a.b[i=1].c", {?MODULE, get, [3]}).
```
Result should be
```
[{b,[[{i,1}],[{i,2},c,{c,3}]]},c]
```

# License
Apache License v2. See LICENSE file for detail.
