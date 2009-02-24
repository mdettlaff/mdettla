# include <stdio.h>
# include <stdlib.h>
# include <time.h>

/* kompiluj $gcc -lrt xyz.c */

int main(int argc, char *argv[]){
	int i, n;

	if (argc < 2)
		printf("Sposob uzycia: generate [liczba_dat] > [plikdocelowy]\n");
	else{
		srandom(time(NULL));

		n=atoi(argv[1]);
		for (i=0; i < n; i++)
			printf("%02d-%02d-%02d\n", rand()%30+1, rand()%12+1, rand()%220+1800);
	}

	return 0;
}

