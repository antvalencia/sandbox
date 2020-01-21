#include <stdio.h>
#include <math.h>
#include <time.h>
#include <stdlib.h>

#define TILES 2


__global__ void
mat_mult(float *m1d, float *m2d, float *mpd, const int DIM) {
	__shared__ float m1s [TILES][TILES];
	__shared__ float m2s [TILES][TILES];

	unsigned int col = (TILES * blockIdx.x) + threadIdx.x;
	unsigned int row = (TILES * blockIdx.y) + threadIdx.y;

	for (int m=0; m<(DIM/TILES); m++) {
		m1s[threadIdx.y][threadIdx.x] =  m1d[(row * DIM) + ((m * TILES) + threadIdx.x)];
		m2s[threadIdx.y][threadIdx.x] =  m2d[(((m * TILES) + threadIdx.y) * DIM) + col];
		__syncthreads();

		for (int k=0; k<TILES; k++)
			mpd[(row * DIM) + col] += (m1s[threadIdx.x][k] * m2s[k][threadIdx.y]);
		__syncthreads();
	}
}

int main () {
	const int DIM = 512;
	const bool PRINT_RESULT = false;
	float m1_h[DIM][DIM], m2_h[DIM][DIM], m_prod_h[DIM][DIM];
	float *m1_d, *m2_d, *m_prod_d;
	int i, j;
	srand(time(NULL));

	// initialize matrices on host
	for (i=0; i<DIM; i++) {
		for (j=0; j<DIM; j++) {
			m1_h[i][j] = rand() % 10;
			m2_h[i][j] = rand() % 10;
		}
	}

	// allocate memory for device matrices
	cudaMalloc((void **) &m1_d, DIM * DIM * sizeof(int));
	cudaMalloc((void **) &m2_d, DIM * DIM * sizeof(int));

	// copy host matrix to device
	cudaMemcpy(m1_d, m1_h, DIM * DIM * sizeof(int), cudaMemcpyHostToDevice);
	cudaMemcpy(m2_d, m2_h, DIM * DIM * sizeof(int), cudaMemcpyHostToDevice);

	// allocate memory for product matrix
	cudaMalloc((void **) &m_prod_d, DIM * DIM * sizeof(int));

	// call kernel
	dim3 dimGrid(DIM/TILES, DIM/TILES, 1);
	dim3 dimBlock(TILES, TILES, 1);

        float exec_t;
        cudaEvent_t s_t, e_t;
        cudaEventCreate(&s_t);
        cudaEventCreate(&e_t);
        cudaEventRecord(s_t, 0);
	mat_mult <<<dimGrid,dimBlock>>> (m1_d, m2_d, m_prod_d, DIM);

	cudaMemcpy(m_prod_h, m_prod_d, DIM * DIM * sizeof(int), cudaMemcpyDeviceToHost);

        cudaEventRecord(e_t, 0);
        cudaEventSynchronize(e_t);
        cudaEventElapsedTime(&exec_t, s_t, e_t);

        if (PRINT_RESULT) {
		for (i=0; i<DIM; i++) {
			for (j=0; j<DIM; j++) {
				printf("%f   ", m_prod_h[i][j]);
			}
			printf("\n");
		}
	}
	printf("GPU-(shr)-dim: %iX%i; exec time: %3.5fms \n", DIM, DIM, exec_t);
}
