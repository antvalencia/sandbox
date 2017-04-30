import time
import numpy as np
import itertools
import os
import theano
import theano.tensor as ts
from theano import pp

dim = 5
print(np.zeros((dim, dim), dtype=np.int))

a = np.array(
    [
        [0, 1, 0],
        [0, 0, 1],
        [0, 0, 0]
    ], dtype=np.int32
)
b = np.array(
    [
        [0, 0, 1],
        [1, 0, 0],
        [0, 0, 0]
    ], dtype=np.int32
)

print(np.reshape(a, (3, 3)))

x = ts.imatrix('x')
y = ts.imatrix('y')
sub = x - y
f = theano.function([x, y], sub)

if len(a) == 0:
    os.sys.exit()

if len(a) != len(a[0]):
    os.sys.exit()

for p in itertools.permutations(range(len(a))):
    a_perm = a[list(p)].T[list(p)].T
    t_start = time.time()
    var = np.count_nonzero(f(a_perm, b))
    t_end = time.time()
    if var == 0:
        print(a)
        print(b)
        print({p[i]+1: i+1 for i in range(len(p))})
    print(t_end - t_start)

