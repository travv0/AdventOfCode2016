#include "stdio.h"

int isTriangle(int, int, int);

int main(void) {
	FILE *f = fopen("input.txt", "r");
	int x1, x2, x3,	y1, y2, y3, z1, z2, z3,	i = 0;

	while (fscanf(f, "%d %d %d %d %d %d %d %d %d",
		      &x1, &x2, &x3, &y1, &y2, &y3, &z1, &z2, &z3) != -1) {
		if (isTriangle(x1, y1, z1))
			i++;
		if (isTriangle(x2, y2, z2))
			i++;
		if (isTriangle(x3, y3, z3))
			i++;
	}

	printf("%d\n", i);
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
