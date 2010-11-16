/*
 * Copyright (c) 2010 The University of Kansas
 * All rights reserved.
 * 
 * Author: Andy Gill (andygill@ku.edu)
 *
 * Trivial driver with two FIFOs than just reflects back everything sent.
 * Nominal error checking.
 */

#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <limits.h>

main(int argc,char **argv){
	char buf[1024];
	int sz, i;
	int wt_hd, rd_hd;
	int DEBUG = 0;

//	for(i = 0;i < argc;i++) {
//		printf("echo: %d (%s)\n",i,argv[i]);
//	}
	
	if (strcmp(argv[1],"1") != 0) {
		fprintf(stderr,"lb_cat failed: more than one writing fifo\n");
		exit(-1);
	}
	wt_hd = atoi(argv[2]);
	if (strcmp(argv[3],"1") != 0) {
		fprintf(stderr,"lb_cat failed: more than one reading fifo\n");
		exit(-1);
	}
	rd_hd = atoi(argv[4]);

	if (argc == 7) {
		if (strcmp(argv[6],"--debug") == 0) {
			DEBUG = 1;
			fprintf(stderr,"lb_cat: DEBUG mode on for lb_cat\n");
		}
	}

	char *str = malloc(100);
	while(1) {
		if (DEBUG) { fprintf(stderr,"lb_cat waiting\n"); }
		sz = read(wt_hd,str,1);
		if (sz == -1) {
			fprintf(stderr,"lb_cat: terminated: %s\n",strerror(errno));
			exit(-1);
		}
		if (sz == 0) {
			fprintf(stderr,"lb_cat: EOF found, terminating\n");
			exit(-1);
		}		
		if (DEBUG) { fprintf(stderr,"lb_cat: cat found 0x%x\n",str[0]); }
		sz = write(rd_hd,str,1);
		if (sz == -1) {
			fprintf(stderr,"lb_cat: terminated: %s\n",strerror(errno));
			exit(-1);
		}
		if (sz == 0) {
			fprintf(stderr,"lb_cat: failed to write; terminating\n");
			exit(-1);
		}		
	}

	exit(0);
}
