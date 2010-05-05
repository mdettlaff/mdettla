#include <stdio.h>
#include <stdlib.h>
#include <math.h>


/*
 * Prosi użytkownika o podanie macierzy z klawiatury i zwraca wskaźnik
 * do niej, a do zmiennej matrix_dimension przypisuje wymiar macierzy.
 */
double **read_matrix(int *matrix_dimension) {
    double **matrix;
    int i, j;
    printf("Podaj wymiar macierzy:\n");
    scanf("%d", matrix_dimension);
    printf("Podaj zawartość macierzy:\n");
    matrix = malloc(*matrix_dimension * sizeof (*matrix));
    for (i = 0; i < *matrix_dimension; i++) {
        matrix[i] = malloc(*matrix_dimension * sizeof (**matrix));
        for (j = 0; j < *matrix_dimension; j++) {
            scanf("%lf", &matrix[i][j]);
        }
    }
    return matrix;
}

void print_matrix(double **matrix, int dimension) {
    int i, j;
    for (i = 0; i < dimension; i++) {
        for (j = 0; j < dimension; j++) {
            printf("%.1lf ", matrix[i][j]);
        }
        printf("\n");
    }
}

/*
 * Oblicza wyznacznik podanej macierzy, jeśli jej rozmiar (dimension) jest
 * równy 2 lub 1 (trywialne przypadki).
 */
double det_trivial(double **matrix, int dimension) {
    if (dimension == 1) {
        return matrix[0][0];
    }
    return matrix[0][0] * matrix[1][1] - matrix[1][0] * matrix[0][1];
}

/*
 * Oblicza wyznacznik podanej macierzy 3x3 metodą Sarrusa.
 */
double det_sarrus(double **matrix) {
    int i;
    double det = 0;
    for (i = 0; i < 3; i++) {
        det +=
            matrix[0][i % 3] * matrix[1][(i + 1) % 3] * matrix[2][(i + 2) % 3];
    }
    for (i = 0; i < 3; i++) {
        det -=
            matrix[2][i % 3] * matrix[1][(i + 1) % 3] * matrix[0][(i + 2) % 3];
    }
    return det;
}

/*
 * Zwraca macierz powstałą z macierzy matrix poprzez skreślenie z niej
 * wiersza row oraz kolumny column. Macierz matrix ma rozmiar
 * dimension * dimension, a zwracana macierz (dimension - 1) * (dimension - 1).
 */
double **create_minor(double **matrix, int dimension, int row, int column) {
    int i, j, ii, jj;
    double **minor = malloc((dimension - 1) * sizeof (*minor));
    for (i = 0; i < dimension - 1; i++) {
        minor[i] = malloc((dimension - 1) * sizeof (**minor));
        ii = i < row ? i : i + 1;
        for (j = 0; j < dimension - 1; j++) {
            jj = j < column ? j : j + 1;
            minor[i][j] = matrix[ii][jj];
        }
    }
    return minor;
}

/*
 * Oblicza rekurencyjnie wyznacznik podanej macierzy z rozwinięcia Laplace'a.
 * Jeśli macierz ma rozmiar 3x3 lub mniejszy, do obliczenia wyznacznika
 * stosowane są prostsze metody (np. reguła Sarrusa).
 */
double det_laplace(double **matrix, int dimension) {
    int k;
    if (dimension > 3) {
        double det = 0;
        // rozwinięcie według pierwszego wiersza
        for (k = 0; k < dimension; k++) {
            double **minor = create_minor(matrix, dimension, 0, k);
            double cofactor =
                pow(-1, 0 + k) * det_laplace(minor, dimension - 1);
            det += matrix[0][k] * cofactor;
        }
        return det;
    } else if (dimension == 3) {
        return det_sarrus(matrix);
    } else {
        return det_trivial(matrix, dimension);
    }
}

int main() {
    double **matrix;
    int matrix_dimension;

    matrix = read_matrix(&matrix_dimension);
    printf("A =\n");
    print_matrix(matrix, matrix_dimension);

    printf("det(A) = %.1lf\n", det_laplace(matrix, matrix_dimension));

    free(matrix);
    return EXIT_SUCCESS;
}
