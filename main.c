#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include <string.h>

#define MAX_ITERATIONS 5

#define BASE 16

#define MAX(a,b) ((a) >= (b) ? (a) : (b))
#define MIN(a,b) ((a) <= (b) ? (a) : (b))

#define POLY_SIZE 72
const int poly[POLY_SIZE] = {
    -6, 3, -6, 12, -4, 7, -7, 1, 0, 5, -2, -4, -12, 2, 7, 12, -7, -10,
    -4, 3, 9, -7, 0, -8, 14, -3, 9, 2, -3, -10, -2, -6, 1, 10, -3, 1,
    7, -7, 7, -12, -5, 8, 6, 10, -8, -8, -7, -3, 9, 1, 6, 6, -2, -3,
    -10, -2, 3, 5, 2, -1, -1, -1, -1, -1, 1, 2, 2, -1, -2, -1, 0, 1
};

#define MAX_DIGITS 2100
typedef struct {
    int digits[MAX_DIGITS];
    int size;
} BigInt;

BigInt one, two, three;
BigInt scale;

void bigint_init(BigInt *a, int value) {
    memset(a->digits, 0, sizeof(a->digits));
    a->size = 0;
    while (value > 0) {
        a->digits[a->size++] = value & 15;
        value >>= 4;
    }
    if (a->size == 0) a->size = 1;
}

double bigint_to_double(const BigInt *bigint) {
	double result = 0.0;
	double base_power = 1.0; // Represents the positional value of each digit.

	for (int i = 0; i < bigint->size; i++) {
		result += bigint->digits[i] * base_power;
		base_power *= BASE; // Scale the base power for the next digit.
	}

	return result;
}

void bigint_from_double(BigInt *bigint, double value) {
	// Clear the BigInt and initialize
	bigint_init(bigint, 0);

	if (value < 0) {
		printf("Error: can not handle negative values!\n");
		exit(1);
	}

	// Extract the integer part of the double
	unsigned long long int_part = (unsigned long long)value;

	// Extract digits from the integer part
	while (int_part > 0) {
	    bigint->digits[bigint->size++] = int_part % BASE;
	    int_part /= BASE;
	}

	// If the value was a pure fractional number (e.g., 0.5), handle it as 0
	if (bigint->size == 0) {
	    bigint->digits[bigint->size++] = 0;
	}
}

void bigint_add(const BigInt *a, const BigInt *b, BigInt *res) {
    int carry = 0;
    res->size = MAX(a->size, b->size);
	int i = 0;
    for (int min = MIN(a->size, b->size); i < min; i++) {
        int sum = carry + a->digits[i] + b->digits[i];
        res->digits[i] = sum & 0xf;
        carry = sum >> 4;
    }
    for (; i < res->size; i++) {
        int sum = carry;
        if (i < a->size) sum += a->digits[i];
        if (i < b->size) sum += b->digits[i];
        res->digits[i] = sum & 0xf;
        carry = sum >> 4;
    }
	if (carry) {
    	res->size++;
    	res->digits[i] = carry & 0xf;
	}
}

void bigint_print(const BigInt *bigint) {
	    char buffer[MAX_DIGITS * 4]; // Each base-16 digit can contribute up to 4 decimal digits
	    int buffer_index = 0;

	    // Temporary array to store the number in base 10
	    unsigned int temp[MAX_DIGITS * 4] = {0};
	    int temp_size = 0;

	    // Initialize temp with the BigInt in base 16
	    for (int i = bigint->size - 1; i >= 0; i--) {
	        unsigned int carry = bigint->digits[i]; // Current base-16 digit to process

	        // Multiply current temp by BASE16 (shifting left in decimal space)
	        for (int j = 0; j < temp_size || carry > 0; j++) {
	            if (j == temp_size) temp_size++; // Expand the size if needed

	            unsigned long long value = (unsigned long long)temp[j] * BASE + carry;
	            temp[j] = value % 10;   // Store the current digit (mod 10)
	            carry = value / 10;     // Carry forward the remaining value
	        }
	    }

	    // Convert the number from temp array to a string buffer in reverse order
	    for (int i = temp_size - 1; i >= 0; i--) {
	        buffer[buffer_index++] = '0' + temp[i];
	    }

	    // Print the final result
	    if (buffer_index == 0) {
	        putchar('0'); // If no digits, the number is zero
	    } else {
	        for (int i = 0; i < buffer_index; i++) {
	            putchar(buffer[i]);
	        }
	    }

	    putchar('\n'); // End with a newline
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
            diff += BASE;
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
        res->digits[i] = prod & 0xf;
        carry = prod >> 4;
    }
    while (res->size > 1 && res->digits[res->size - 1] == 0) {
        res->size--;
    }
}

void bigint_multiply(const BigInt *a, const BigInt *b, BigInt *res) {
    memset(res->digits, 0, sizeof(res->digits));
    res->size = a->size + b->size;
	int alen = a->size;
	int blen = b->size;
    for (int i = 0; i < a->size; i++) {
        int carry = 0;
        for (int j = 0; j < b->size; j++) {
            carry += res->digits[i + j];
			carry += a->digits[i] * b->digits[j];
            res->digits[i + j] = carry & 0xf;
            carry >>= 4;
        }
        carry += res->digits[i + b->size];
        res->digits[i + b->size] = carry & 0xf;
    }
    while (res->size > 1 && res->digits[res->size - 1] == 0) {
        res->size--;
    }
}

void bigint_divide(const BigInt *a, const BigInt *b, BigInt *quotient) {
    BigInt current, temp;
    bigint_init(&current, 0);

    quotient->size = 0;

    for (int i = a->size - 1; i >= 0; i--) {
        for (int j = current.size; j > 0; j--) {
            current.digits[j] = current.digits[j - 1];
        }
        current.digits[0] = a->digits[i];
        if (current.size == 0 || current.digits[current.size] != 0) {
            current.size++;
        }

        // Find the maximum number x such that b * x <= current
        int x = 0;
        int low = 0, high = BASE - 1;
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

void compute_reciprocal(const BigInt *b, BigInt *reciprocal) {
	BigInt guess, temp, temp2, temp3;
	bigint_init(&guess, 10);
	bigint_init(&temp, 0);

	BigInt y;
	bigint_init(&y, 10);

	// Iterative refinement (Newton's Method).
	for (int i = 0; i < MAX_ITERATIONS; i++) {
		bigint_init(&temp2, 0);
		bigint_init(&temp3, 0);
		// Compute: guess = guess * (2 - b * guess)
		bigint_multiply(b, &guess, &temp);          // temp = b * guess
		bigint_subtract(&y, &temp, &temp3);        // temp = 2 - temp
		bigint_multiply(&guess, &temp3, &temp2);    // guess = guess * temp
		guess = temp2;
	}

	*reciprocal = guess;
}

void truncate_precision(BigInt *value, int precision) {
	// Truncate value to the required number of digits.
	if (value->size > precision) {
		value->size = precision;
	}
}

void bigint_divide2(const BigInt *a, const BigInt *b, BigInt *quotient) {
	BigInt reciprocal, temp;
	bigint_init(&reciprocal, 0);
	bigint_init(&temp, 0);

	// Step 1: Compute reciprocal of b using Newton's method.
	compute_reciprocal(b, &reciprocal);

	// Step 2: Multiply a by reciprocal to compute the quotient.
	bigint_multiply(a, &reciprocal, quotient);

	// Step 3: Truncate extra precision (if needed).
	truncate_precision(quotient, a->size - b->size);

	// Step 4: Remove any residual error by correction.
	BigInt correction;
	bigint_init(&correction, 0);
	bigint_multiply(b, quotient, &correction);

	if (bigint_compare(&correction, a) > 0) {
		bigint_subtract(quotient, &one, quotient);
	}
}

void evalpoly(const BigInt *x, const int *coeffs, int deriv, BigInt *result) {
	BigInt temp = {};

	int f = 1;
    for (int i = POLY_SIZE - 1; i >= deriv; i--) {
        int coefficient = coeffs[i] * (deriv ? i : 1);
		BigInt mulRes, divRes;

        bigint_multiply_int(&scale, abs(coefficient), &mulRes);
        bigint_multiply(x, result, &temp);
        bigint_divide(&temp, &scale, &divRes);

		int sgn_a = coefficient >= 0;
		int sgn_b = f;

        if (sgn_a == sgn_b) {
            bigint_add(&mulRes, &divRes, result);
			continue;
        }

		if (bigint_compare(&divRes, &mulRes) == 1) {
            bigint_subtract(&divRes, &mulRes, result);
			f = !sgn_a;
		} else {
            bigint_subtract(&mulRes, &divRes, result);
			f = !sgn_b;
		}
    }
}

void newton_method(BigInt *x) {
    for (int i = 0; i < 14; i++) {
    	BigInt f_x, f_prime_x, temp;
    	bigint_init(&temp, 0);
    	bigint_init(&f_x, 0);
    	bigint_init(&f_prime_x, 0);

        evalpoly(x, poly, 0, &f_x);
        evalpoly(x, poly, 1, &f_prime_x);

        bigint_multiply(&f_x, &scale, &temp);

		BigInt res;
        bigint_divide(&temp, &f_prime_x, &res);
		temp = res;

        bigint_subtract(x, &temp, &res);
		*x = res;
		printf("%d\n", x->size);
    }
}

int test() {
	BigInt five, six;
	bigint_init(&five, 5);
	bigint_init(&six, 6);

	BigInt res;
	bigint_multiply(&five, &six, &res);
	bigint_print(&res);

	bigint_multiply_int(&six, 10, &six);
	bigint_divide2(&six, &five, &res);
	bigint_print(&res);

	BigInt a, b;
	bigint_init(&a, 100);
	bigint_init(&b, 99);
	bigint_divide2(&a, &b, &res);
	bigint_print(&res);
}

int main() {
	bigint_init(&one, 1);
	bigint_init(&two, 2);
	bigint_init(&three, 3);

	test();
	return 0;

    BigInt x;
    bigint_init(&scale, 0);
    bigint_init(&x, 0);

	memset(scale.digits, 0, sizeof(scale.digits));

    bigint_init(&scale, 1);
    for (int i = 0; i < 1000; i++) {
        bigint_multiply_int(&scale, 10, &scale);
    }



    BigInt res;
    bigint_multiply_int(&scale, 4, &x);
    bigint_divide(&x, &three, &res);

    newton_method(&res);

	printf("1.");

	bigint_subtract(&res, &scale, &res); 
	bigint_subtract(&res, &two, &res); 
	bigint_print(&res);

    return 0;
}

