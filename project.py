import time
import os
import numpy as np
from itertools import combinations, permutations
import theano.tensor as ts
import theano
# from theano import pp


def get_0_1_matrix_combos(dim, n, dtype=np.int32):
    assert n <= dim * dim

    for idx in combinations(range(dim * dim), n):
        m = np.zeros(dim * dim, dtype=dtype)
        if idx:
            m[np.array(idx)] = 1
        yield m.reshape(dim, dim)

x = ts.imatrix('x')
y = ts.imatrix('y')
sub = x - y
f = theano.function([x, y], sub)

d = 5
i = 2
matches = 0
t_start = time.time()
for a in get_0_1_matrix_combos(d, i):
    for b in get_0_1_matrix_combos(d, i):
        if len(a) == 0:
            os.sys.exit()

        if len(a) != len(a[0]):
            os.sys.exit()

        for p in permutations(range(len(a))):
            a_perm = a[list(p)].T[list(p)].T
            var = np.count_nonzero(f(a_perm, b))
            if var == 0:
                matches += 1
                # print(a)
                # print(b)
                # print({p[i]+1: i+1 for i in range(len(p))})
t_end = time.time()
print(matches)
print(t_end - t_start)

