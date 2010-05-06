/*
 * Program obliczający wyznacznik podanej przez użytkownika macierzy.
 * Wyznacznik jest obliczany z rozwinięcia Laplace'a, rekurencyjnie
 * i współbieżnie z wykorzystaniem wątków POSIX.
 */

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>


typedef struct matrix_struct {
    double **a; // elementy
    int dim; // wymiar
    double det; // wyznacznik
} matrix_t;


/*
 * Prosi użytkownika o podanie macierzy z klawiatury i zwraca wskaźnik
 * do niej.
 */
matrix_t *read_matrix() {
    matrix_t *matrix;
    int i, j;
    matrix = malloc(sizeof (matrix_t));
    printf("podaj wymiar macierzy:\n");
    scanf("%d", &matrix->dim);
    printf("podaj zawartość macierzy:\n");
    matrix->a = malloc(matrix->dim * sizeof (*matrix->a));
    for (i = 0; i < matrix->dim; i++) {
        matrix->a[i] = malloc(matrix->dim * sizeof (**matrix->a));
        for (j = 0; j < matrix->dim; j++) {
            scanf("%lf", &matrix->a[i][j]);
        }
    }
    return matrix;
}

void print_matrix(matrix_t *matrix) {
    int i, j;
    for (i = 0; i < matrix->dim; i++) {
        for (j = 0; j < matrix->dim; j++) {
            printf("%.1lf ", matrix->a[i][j]);
        }
        printf("\n");
    }
}

/*
 * Zwraca wyznacznik podanej macierzy o wymiarze 2x2.
 */
double det_2d(matrix_t *A) {
    return A->a[0][0] * A->a[1][1] - A->a[1][0] * A->a[0][1];
}

/*
 * Zwraca wyznacznik podanej macierzy o wymiarze 3x3 obliczony metodą Sarrusa.
 */
double det_sarrus(matrix_t *A) {
    int i;
    double det = 0;
    for (i = 0; i < 3; i++) {
        det += A->a[0][i % 3] * A->a[1][(i + 1) % 3] * A->a[2][(i + 2) % 3];
    }
    for (i = 0; i < 3; i++) {
        det -= A->a[2][i % 3] * A->a[1][(i + 1) % 3] * A->a[0][(i + 2) % 3];
    }
    return det;
}

/*
 * Tworzy macierz powstałą z macierzy matrix poprzez skreślenie z niej
 * wiersza i oraz kolumny j, po czym zwraca wskaźnik do niej. Jeśli macierz
 * matrix jest wymiaru n x n, to zwracana macierz jest wymiaru n-1 x n-1.
 */
matrix_t *create_minor(matrix_t *matrix, int i, int j) {
    matrix_t *minor;
    int i_minor, j_minor, i_matrix, j_matrix;
    minor = malloc(sizeof (matrix_t));
    minor->dim = matrix->dim - 1;
    minor->a = malloc(minor->dim * sizeof (*minor->a));
    for (i_minor = 0; i_minor < minor->dim; i_minor++) {
        minor->a[i_minor] = malloc(minor->dim * sizeof (**minor->a));
        i_matrix = i_minor < i ? i_minor : i_minor + 1;
        for (j_minor = 0; j_minor < minor->dim; j_minor++) {
            j_matrix = j_minor < j ? j_minor : j_minor + 1;
            minor->a[i_minor][j_minor] = matrix->a[i_matrix][j_matrix];
        }
    }
    return minor;
}

/*
 * Wątek, który bierze jako argument wejściowy wskaźnik do macierzy i wpisuje
 * w jej polu ->det wartość wyznacznika tej macierzy obliczoną z rozwinięcia
 * Laplace'a. Jeśli macierz ma rozmiar 3x3 lub mniejszy, do obliczenia
 * wyznacznika stosowane są prostsze metody (np. reguła Sarrusa).
 */
void *det_laplace_thread(void *arg) {
    matrix_t *matrix;
    int k;
    int res;
    matrix = (matrix_t *)arg;
    if (matrix->dim == 1) {
        matrix->det = matrix->a[0][0];
    } else if (matrix->dim == 2) {
        matrix->det = det_2d(matrix);
    } else if (matrix->dim == 3) {
        matrix->det = det_sarrus(matrix);
    } else {
        // tworzymy minory macierzy potrzebne do obliczenia rozwinięcia
        // Laplace'a według pierwszego wiersza
        matrix_t *minors[matrix->dim];
        for (k = 0; k < matrix->dim; k++) {
            minors[k] = create_minor(matrix, 0, k);
        }

        /*
         * Obliczamy rekurencyjnie wyznaczniki minorów danej macierzy.
         * Wyznacznik każdego minora obliczany jest przez osobny wątek
         * (nową instancję tego wątku).
         */
        pthread_t threads[matrix->dim];
        for (k = 0; k < matrix->dim; k++) {
            res = pthread_create(
                    &threads[k], NULL, det_laplace_thread, (void *)minors[k]);
            if (res != 0) {
                perror("Thread creation failed");
                exit(EXIT_FAILURE);
            }
        }
        for (k = 0; k < matrix->dim; k++) {
            void *thread_result;
            res = pthread_join(threads[k], &thread_result);
            if (res != 0) {
                perror("Thread join failed");
                exit(EXIT_FAILURE);
            }
        }
        // po tym jak wątki zakończyły działanie, każdy minor ma wpisany
        // swój wyznacznik w polu ->det

        matrix->det = 0;
        for (k = 0; k < matrix->dim; k++) {
            double cofactor = pow(-1, 0 + k) * minors[k]->det;
            // stosujemy rozwinięcie według pierwszego wiersza
            matrix->det += matrix->a[0][k] * cofactor;
        }
    }
    return NULL;
}

/*
 * Oblicza wyznacznik danej macierzy z rozwinięcia Laplace'a i wpisuje go do
 * pola ->det tej macierzy.
 */
void det_laplace(matrix_t *matrix) {
    det_laplace_thread(matrix);
}

int main() {
    matrix_t *matrix;

    matrix = read_matrix();
    det_laplace(matrix);
    printf("wyznacznik macierzy jest równy %.1lf\n", matrix->det);

    return EXIT_SUCCESS;
}
