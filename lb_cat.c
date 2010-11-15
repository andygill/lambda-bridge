#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

main(int argc,char **argv){
	char buf[1024];
	int sz, i;
	int wt_hd, rd_hd;

	for(i = 0;i < argc;i++) {
		printf("echo: (%s)\n",argv[i]);
	}

	i = 0;
	while(i < argc) {
		if (strcmp(argv[i],"--write-fds") == 0) {
			i++;
			if (i < argc) {
				wt_hd = atoi(argv[i]);
			}
	} else if (strcmp(argv[i],"--read-fds") == 0) {
			i++;
			printf("%d %d\n",i,argc);
			if (i < argc) {
				rd_hd = atoi(argv[i]);
			}
		} else {
			// ignore, for now
		}
		i++;
	}

	
	printf(">> %d\n",rd_hd);

	i = 0;
	char *str = malloc(100);
	for(i=0;i<100000;i++) {
		printf("loop cat : %d\n",i);
		sz = read(wt_hd,str,1);
		if (sz == -1) {
			printf(">> %s\n",strerror(errno));
			exit(-1);
		}
		printf("wt_hd sz=%d\n",sz);
//		sprintf(str,"%d\n",i);
//		printf("%s\n",str);
		sz = write(rd_hd,str,1);
		if (sz == -1) {
			printf(">> %s\n",strerror(errno));
			exit(-1);
		}
		printf("=>%d\n",sz);
	}

	exit(0);
}
