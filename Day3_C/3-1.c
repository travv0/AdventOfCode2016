#include "stdio.h"

int isTriangle(int, int, int);

int main(void) {
	FILE *f = fopen("input.txt", "r");
	int x, y, z, i = 0;

	while (fscanf(f, "%d %d %d", &x, &y, &z) != -1)
		if (isTriangle(x, y, z))
			i++;

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
