from theano import tensor as T
from theano import function as t_fn
import numpy as np
import time


M = 512
N = 512
PRINT_MAT = False
x = T.dmatrix('x')
y = T.dmatrix('y')
z = x * y
mat_mult = t_fn([x, y], z)
m1 = np.mat(np.ones((M, N)))
m2 = np.mat(np.ones((M, N)))

s_t = time.time()
prod = mat_mult(m1, m2)
e_t = time.time()

print("Theano----dim: " + str(M) + "x" + str(N) + "; exec time: {0:.5f}ms".format(round(((e_t - s_t) * 1000),2)))
if PRINT_MAT:
    print(prod)
