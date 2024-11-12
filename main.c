#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include <string.h>

#define POLY_SIZE 72
const int poly[POLY_SIZE] = {
    -6, 3, -6, 12, -4, 7, -7, 1, 0, 5, -2, -4, -12, 2, 7, 12, -7, -10,
    -4, 3, 9, -7, 0, -8, 14, -3, 9, 2, -3, -10, -2, -6, 1, 10, -3, 1,
    7, -7, 7, -12, -5, 8, 6, 10, -8, -8, -7, -3, 9, 1, 6, 6, -2, -3,
    -10, -2, 3, 5, 2, -1, -1, -1, -1, -1, 1, 2, 2, -1, -2, -1, 0, 1
};

#define MAX_DIGITS 5100  // Support up to 1000 digits

typedef struct {
    int digits[MAX_DIGITS];
    int size;
} BigInt;

void bigint_init(BigInt *a, int value) {
    memset(a->digits, 0, sizeof(a->digits));
    a->size = 0;
    while (value > 0) {
        a->digits[a->size++] = value % 10;
        value /= 10;
    }
    if (a->size == 0) a->size = 1;  // Ensure zero has size 1
}

void bigint_from_string(BigInt *a, const char *str) {
    int len = strlen(str);
    a->size = len;
    for (int i = 0; i < len; i++) {
        a->digits[i] = str[len - i - 1] - '0';
    }
}

void bigint_add(const BigInt *a, const BigInt *b, BigInt *res) {
    int carry = 0;
    res->size = (a->size > b->size ? a->size : b->size);
    for (int i = 0; i < res->size || carry; i++) {
        if (i == res->size) res->size++;
        int sum = carry;
        if (i < a->size) sum += a->digits[i];
        if (i < b->size) sum += b->digits[i];
        res->digits[i] = sum % 10;
        carry = sum / 10;
    }
}

void bigint_print(const BigInt *a) {
    for (int i = a->size - 1; i >= 0; i--) {
        printf("%d", a->digits[i]);
    }
    printf("\n");
	printf("size: %d\n", a->size);
}

int bigint_compare(const BigInt *a, const BigInt *b) {
    if (a->size != b->size) return (a->size > b->size) ? 1 : -1;
    for (int i = a->size - 1; i >= 0; i--) {
        if (a->digits[i] != b->digits[i])
            return (a->digits[i] > b->digits[i]) ? 1 : -1;
    }
    return 0;
}

void bigint_subtract(const BigInt *a, const BigInt *b, BigInt *res) {
    int borrow = 0;
    res->size = a->size;
    for (int i = 0; i < res->size; i++) {
        int diff = a->digits[i] - borrow;
        if (i < b->size) diff -= b->digits[i];
        if (diff < 0) {
            diff += 10;
            borrow = 1;
        } else {
            borrow = 0;
        }
        res->digits[i] = diff;
    }
    while (res->size > 1 && res->digits[res->size - 1] == 0) {
        res->size--;
    }
}

void bigint_multiply_int(const BigInt *a, int b, BigInt *res) {
    long long carry = 0;
    res->size = a->size;
    for (int i = 0; i < a->size || carry; i++) {
        if (i == res->size) res->size++;
        long long prod = (i < a->size ? a->digits[i] : 0) * (long long)b + carry;
        res->digits[i] = prod % 10;
        carry = prod / 10;
    }
    while (res->size > 1 && res->digits[res->size - 1] == 0) {
        res->size--;  // Remove leading zeros
    }
}

void bigint_multiply(const BigInt *a, const BigInt *b, BigInt *res) {
    memset(res->digits, 0, sizeof(res->digits));
    res->size = a->size + b->size;
	int alen = a->size;
	int blen = b->size;
    for (int i = 0; i < alen; i++) {
        int carry = 0;
        for (int j = 0; ; ) {
			if (i+j>=MAX_DIGITS) break;
			if (j>=MAX_DIGITS) break;
			if (j>= blen && !carry) break;
            long long prod = res->digits[i + j] + carry + (long long)a->digits[i] * (j < b->size ? b->digits[j] : 0);
            res->digits[i + j] = prod % 10;
            carry = prod / 10;
			j++;
			if (j < blen) continue;
			if (prod/10) continue;
			break;
        }
    }
	if (res->size > MAX_DIGITS) {
		printf("asdf\n");
	}
    while (res->size > 1 && res->digits[res->size - 1] == 0) {
        res->size--;  // Remove leading zeros
    }
}

void bigint_divide(const BigInt *a, const BigInt *b, BigInt *quotient) {
    BigInt current, temp;
    memset(quotient->digits, 0, sizeof(quotient->digits));
    quotient->size = 0;

    bigint_init(&current, 0);

    for (int i = a->size - 1; i >= 0; i--) {
        // Shift current left by one digit
        for (int j = current.size; j > 0; j--) {
            current.digits[j] = current.digits[j - 1];
        }
        current.digits[0] = a->digits[i];
        if (current.size == 0 || current.digits[current.size] != 0) {
            current.size++;
        }

        // Find the maximum number x such that b * x <= current
        int x = 0;
        int low = 0, high = 9;
        while (low <= high) {
            int mid = (low + high) / 2;
            bigint_multiply_int(b, mid, &temp);
            if (bigint_compare(&temp, &current) <= 0) {
                x = mid;
                low = mid + 1;
            } else {
                high = mid - 1;
            }
        }

        // Set quotient digit
        quotient->digits[i] = x;
        if (i >= quotient->size) quotient->size = i + 1;

        // Subtract b * x from current
        bigint_multiply_int(b, x, &temp);
        bigint_subtract(&current, &temp, &current);
    }

    // Remove leading zeros in quotient
    while (quotient->size > 1 && quotient->digits[quotient->size - 1] == 0) {
        quotient->size--;
    }
}

void evalpoly(const BigInt *x, const int *coeffs, const BigInt *scale, int deriv, BigInt *result) {
    BigInt term = {};
	BigInt temp = {};
   	BigInt sink = {};

    bigint_init(result, 0);
    bigint_init(&temp, 0);
    bigint_init(&term, 0);
    bigint_init(&sink, 0);

    BigInt current_x = *x;

	int isNeg = 0;

	int j = 0;

	int f = 1;

    for (int i = POLY_SIZE - 1; i >= deriv; i--, ++j) {
        int coefficient = coeffs[i] * (deriv ? i : 1);

		BigInt mulRes, divRes;

        bigint_init(&term, abs(coefficient));
        bigint_multiply(&term, scale, &mulRes);

        bigint_multiply(&current_x, result, &temp);
        bigint_divide(&temp, scale, &divRes);

		// mul
		int sgn_a = coefficient >= 0;
		// div
		int sgn_b = f;

        if (sgn_a == sgn_b) {
            bigint_add(&mulRes, &divRes, result);
        } else {
			if (bigint_compare(&divRes, &mulRes) == 1) {
            	bigint_subtract(&divRes, &mulRes, result);
				f = !sgn_a;
			} else {
            	bigint_subtract(&mulRes, &divRes, result);
				f = !sgn_b;
			}
		}
    }
}

void newton_method(BigInt *x, const BigInt *scale) {
    BigInt f_x, f_prime_x, temp;
    bigint_init(&f_x, 0);
    bigint_init(&f_prime_x, 0);
    bigint_init(&temp, 0);

    for (int i = 0; i < 14; i++) {
        // Evaluate f(x) and f'(x)
        evalpoly(x, poly, scale, 0, &f_x);
        evalpoly(x, poly, scale, 1, &f_prime_x);

        // temp = f(x) * scale
        bigint_multiply(&f_x, scale, &temp);


        // temp = f(x) * scale / f'(x)
		BigInt res;
    	bigint_init(&res, 0);
        bigint_divide(&temp, &f_prime_x, &res);
		temp = res;

        // x = x - temp
        bigint_subtract(x, &temp, &res);
		*x = res;
		bigint_print(x);
    }
}

int main() {
    BigInt scale, x;
    bigint_init(&scale, 0);
    bigint_init(&x, 0);

	memset(scale.digits, 0, sizeof(scale.digits));

    // Initialize scale = 10^1000
    bigint_from_string(&scale, "1");
    for (int i = 0; i < 1000; i++) {
        bigint_multiply_int(&scale, 10, &scale);
    }

    // Initialize x = scale * 4 / 3
    BigInt four, three, res;
    bigint_init(&four, 4);
    bigint_init(&three, 3);
    bigint_multiply(&scale, &four, &x);
    bigint_divide(&x, &three, &res);

    // Perform Newton's method to approximate root
    newton_method(&res, &scale);

	printf("1.");
	for (int i = 1000; i >= 3; i--) {
		printf("%d", res.digits[i-1]);
	}
	puts("49");

    return 0;
}

