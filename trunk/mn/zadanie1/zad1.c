// Przyblizanie calki metoda Gaussa-Czebyszewa

#include <stdio.h>
#include <math.h>

#define MAX_N 100000

/* Fn - funkcje, ktore calkujemy */

// przyklad 1: rozklad wykladniczy
double F1(double x) {
  return exp(-x); // wynik: 1
}

// przyklad 2
double F2(double x) {
  return sin(x)*exp(-x); // wynik: 1/2
}

// przyklad 3
double F3(double x) {
  return 1.0/(pow(x,2)+1); // wynik: PI/2
}

// przyklad 3
double F4(double x) {
  return pow(x,2)/pow(2,x);
}


double f1(double x) {
  return 2*F1((1+x)/(1-x)) * pow((1-x), -2) * sqrt(1-pow(x, 2));
}

double f2(double x) {
  return 2*F2((1+x)/(1-x)) * pow((1-x), -2) * sqrt(1-pow(x, 2));
}

double f3(double x) {
  return 2*F3((1+x)/(1-x)) * pow((1-x), -2) * sqrt(1-pow(x, 2));
}

double f4(double x) {
  return 2*F4((1+x)/(1-x)) * pow((1-x), -2) * sqrt(1-pow(x, 2));
}


double x(int i, int n) {
  return cos(M_PI*(2*i-1)/(2*n));
}


int main() {
  int i, n;
  double sum;
  double W_i;

  printf("Calka oznaczona z F(x)=exp(-x) od 0 do niesk. = 1\n");
  // liczymy sume
  for (n=10; n < MAX_N; n*=10) {
    sum = 0;
    W_i = M_PI/n;
    for (i=0; i < n; i++) {
      sum += W_i * f1(x(i, n));
    }
    printf("\tdla n=%d\t%f\n", n, sum);
  }

  printf("Calka oznaczona z F(x)=sin(x)*exp(-x) od 0 do niesk. = 1/2\n");
  // liczymy sume
  for (n=10; n < MAX_N; n*=10) {
    sum = 0;
    W_i = M_PI/n;
    for (i=0; i < n; i++) {
      sum += W_i * f2(x(i, n));
    }
    printf("\tdla n=%d\t%f\n", n, sum);
  }

  printf("Calka oznaczona z F(x)=1/(x^2+1) od 0 do niesk. = PI/2");
  printf(" ~= 1.5707963\n");
  // liczymy sume
  for (n=10; n < MAX_N; n*=10) {
    sum = 0;
    W_i = M_PI/n;
    for (i=0; i < n; i++) {
      sum += W_i * f3(x(i, n));
    }
    printf("\tdla n=%d\t%f\n", n, sum);
  }

  printf("Calka oznaczona z F(x)=x^2/2^x od 0 do niesk. ~= 6.0055614\n");
  // liczymy sume
  for (n=10; n < MAX_N; n*=10) {
    sum = 0;
    W_i = M_PI/n;
    for (i=0; i < n; i++) {
      sum += W_i * f4(x(i, n));
    }
    printf("\tdla n=%d\t%f\n", n, sum);
  }

  return 0;
}
