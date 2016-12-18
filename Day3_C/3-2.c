#include "stdio.h"

#define SIDES 3

int isTriangle(int, int, int);

int main(void) {
	FILE *f = fopen("input.txt", "r");
	int x[SIDES], y[SIDES], z[SIDES];
	int counter = 0, i = 0;

	while (fscanf(f, "%d %d %d %d %d %d %d %d %d",
		      &x[0], &x[1], &x[2],
		      &y[0], &y[1], &y[2],
		      &z[0], &z[1], &z[2]) != -1) {
		for (i = 0; i < SIDES; i++)
			if (isTriangle(x[i], y[i], z[i]))
				counter++;
	}

	printf("%d\n", counter);
	return 0;
}

int isTriangle(int x, int y, int z) {
	if (x + y > z &&
	    y + z > x &&
	    x + z > y) {
		return 1;
	}
	return 0;
}
