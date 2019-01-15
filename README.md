# HakiSQL

An in-memory datastore for Erlang that uses [hakicache][1] in the background. This
allows for querying the data with no copy. 

**NOTE**: On OTP 21+, hakicache is not used and instead data is stoed in [persistent term][5] 
storage built into Erlang.

HakiSQL allows for very basic SQL-like queries.

HakiSQL uses a wide bitmap to index the data, therefore this datastore is only
useful for datasets with low cardinality for indexed fields. This also means loading
the data is slow but querying is fast.

This library is by no means "production" ready. Even if it is, I do not recommend
using it. Especially if you load/insert data often.

### WHY
To learn about Bitmap indexes and fast datastore access path problems.

### Persistence
:warning: Does not exist. Restart the VM and your data is gone :)

### TODO
1. Range query use range bitmap index
2. Inserting rows need to recalculate index
3. Persistence?
4. More tests for queries

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

### Query Language
The language is a basic version of popular SQL:

### Operations
#### And
`expresion AND expression`
Return rows where both expressions are true.

#### Or
`expression OR expression`
Return rows where either expression is true.

### =
`field = values`
Return rows where field value is equal to value.

### !=
`field = values`
Return rows where field value is not equal to value.

### > / < / <= / =>
:warning: Not Implemented.

`field (>/</<=/=>) values`
Return rows where field equality is tested against value.
Comparison follows [Erlang standard](http://erlang.org/doc/reference_manual/expressions.html#term-comparisons).

#### In
`field in (values)`
Return rows where field value is one of values.

#### Not
`field not in (values)`
Return rows where expression value is not any of values.

### Value Representation
#### Atoms
`"field = atom"`
#### String
`"field = 'string'"`
#### Number
`"field = 23 OR field = 2.3"`
#### Binary
`"field = <<'binary'>>"`
#### Tuple
:warning: Has bugs.

`"field = {a,b,23}"`
#### Map
:warning: Not implemented.
#### Record
:warning: Not implemented.

### Benchmarks
Using [eministat][4] to see if there is a significant
difference between [1000 searches in 10000 "complex" records][6]
using hakisql (persistent term storage) or ETS:
```
Dataset: x (ETS) N=1000 CI=95.0000
Statistic     Value     [         Bias] (Bootstrapped LB‥UB)
Min:            783.000
1st Qu.         826.000
Median:         861.000
3rd Qu.         913.000
Max:            3413.00
Average:        910.550 [    -0.127774] (      900.227 ‥       924.029)
Std. Dev:       191.568 [     -1.55408] (      161.391 ‥       249.685)

Outliers: 0/103 = 103 (μ=910.422, σ=190.014)
        Outlier variance:      0.994651 (severe, the data set is probably unusable)

------

Dataset: + (HakiSQL) N=1000 CI=95.0000
Statistic     Value     [         Bias] (Bootstrapped LB‥UB)
Min:            152.000
1st Qu.         153.000
Median:         154.000
3rd Qu.         160.000
Max:            264.000
Average:        160.469 [   3.14420e-3] (      159.617 ‥       161.479)
Std. Dev:       14.9246 [  -3.03244e-2] (      13.1941 ‥       16.9747)

Outliers: 0/140 = 140 (μ=160.472, σ=14.8943)
        Outlier variance:      0.970293 (severe, the data set is probably unusable)

Difference at 95.0% confidence
        -750.081 ± 11.9095
        -82.3767% ± 1.30795%
        (Student's t, pooled s = 135.870)
------
```


For small tables, ETS always wins. Bigger tables HakiSQL is much faster and more consistent in timing.

Look at [this perf test][3] for more info

### References
The Lexer and Parser code were originally copied/modified from [swirl][2].


[1]: https://github.com/gootik/hakicache
[2]: https://github.com/lpgauth/swirl
[3]: https://github.com/gootik/hakisql/blob/master/test/hakisql_perf_test.erl#L13
[4]: https://github.com/jlouis/eministat
[5]: http://erlang.org/doc/man/persistent_term.html
[6]: https://github.com/gootik/hakisql/blob/master/benchmark/bechmark.erl
