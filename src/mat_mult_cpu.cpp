#include <stdio.h>
#include <iostream>
#include <cstdlib>
#include <ctime>
#include <sys/time.h>


using namespace std;

const int M = 512;
const int N = 512;
const bool PRINT_RESULT = false;
 
void mat_mult() {
	int mat_1[M][N];
	int mat_2[M][N];
	int mat_prod[M][N];
	struct timespec s_t, e_t;

	srand(time(NULL));
	for(int m=0; m<M; m++) {
		for(int n=0; n<N; n++) {
			mat_1[m][n] = rand() % 10;
			mat_2[m][n] = rand() % 10;
			mat_prod[m][n] = 0;
		}
	}

	clock_gettime(CLOCK_MONOTONIC_RAW, &s_t);
	for (int i=0; i<M; i++)
		for (int j=0; j<N; j++)
			for (int k=0; k<N; k++)
				mat_prod[i][j] += mat_1[i][k] * mat_2[k][j];
	clock_gettime(CLOCK_MONOTONIC_RAW, &e_t);
	double delta_t = double((e_t.tv_sec - s_t.tv_sec) * 1000000 + (e_t.tv_nsec - s_t.tv_nsec) / 1000) / 1000.0;

	if (PRINT_RESULT) {
		for (int i=0; i<M; i++) {
			for (int j=0; j<N; j++)
				cout << mat_prod[i][j] << " ";
			cout << endl;
		}
	}
	cout << "CPU-------dim: " << M << "X" << N << "; exec time: ";
	printf("%.5lf", delta_t);
	cout << "ms" << endl;
}

int main() {
	mat_mult();
	return 0;
}

