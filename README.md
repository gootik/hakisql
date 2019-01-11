# HakiSQL

An in-memory datastore for Erlang that uses [hakicache][1] in the background. This
allows for querying the data with no copy.

HakiSQL allows for very basic SQL-like queries. 

HakiSQL uses a wide bitmap to index the data, therefore this datastore is only
useful for datasets with low cardinality for indexed fields. This also means loading
the data is slow but querying is fast. 

This library is by no means "production" ready. Even if it is, I do not recommend
using it. Especially if you load/insert data often. 

### WHY
To learn about Bitmap indexes and fast datastore access path problems.

### Persistence
Does not exist. Restart the VM and your data is gone :)

### TODO
1. Range query use range bitmap index 
2. Custom filtering funs?! like ets:fun2ms()
3. Inserting rows need to recalculate index
4. Create views

### Example 
```erlang
Erlang/OTP 20 [erts-9.1] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V9.1  (abort with ^G)
1>  hakisql:create(test, #{
1>      a => [index, atom],
1>      b => [index, number],
1>      c => [number],
1>      name => [string]
1>  }).
ok

2> hakisql:insert(test, [
2>     #{a => test, b => 2, c => 3.1, name => "A"},
2>     #{a => test2, b => 24, c => 12.1, name => "B"},
2>     #{a => test, b => 4, c => 12.1, name => "C"},
2>     #{a => test2, b => 24, c => 12.1, name => "D"},
2>     #{a => test, b => 8, c => 12.1, name => "E"},
2>     #{a => test4, b => 24, c => 12.1, name => "F"}
2> ]).
ok

3> hakisql:q(test, "(b = 2 OR b = 8) AND a = test").
[#{a => test,b => 2,c => 3.1,name => "A"},
 #{a => test,b => 8,c => 12.1,name => "E"}]
```

### Benchmarks
TODO.

For small tables, ETS always wins. Bigger tables HakiSQL is much faster and more consistent in timing.

Look at [this perf test][3] for more info

### References
The Lexer and Parser code were originally copied/modified from [swirl][2].


[1]: https://github.com/gootik/hakicache
[2]: https://github.com/lpgauth/swirl
[3]: https://github.com/gootik/hakisql/blob/master/test/hakisql_perf_test.erl#L13
