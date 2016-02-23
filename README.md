Simple Binary Interval Tree (Fenwick Tree). Support range sum queries with updates.

Random tests (vector size = 1000, update/query size = 1000):

    benchmarking array update & query test/naive method
    time                 3.336 ms   (3.301 ms .. 3.368 ms)
                         0.999 R²   (0.999 R² .. 0.999 R²)
    mean                 3.405 ms   (3.374 ms .. 3.444 ms)
    std dev              108.6 μs   (73.43 μs .. 142.3 μs)
    variance introduced by outliers: 16% (moderately inflated)
    
    benchmarking array update & query test/with fenwick
    time                 1.551 ms   (1.539 ms .. 1.564 ms)
                         0.999 R²   (0.998 R² .. 0.999 R²)
    mean                 1.554 ms   (1.538 ms .. 1.571 ms)
    std dev              55.81 μs   (46.74 μs .. 68.89 μs)
    variance introduced by outliers: 23% (moderately inflated)

