#include <stdio.h>
#include <string.h>
#include <math.h>

int main(int argc, char *argv[]) {
  char *digits;
  int size, i, j, number, shift;
  number = 0;

  digits = argv[1];
  size = strlen(digits);

  for (i = size-1, j = 0; i >= 0; --i, ++j) {
    shift = pow(10, j);
    number = number + (shift * (digits[i] - '0'));
  }

  printf("int: %d, char: %c\n", number, number);
}
