/*
 * Copyright (c) 2010 The University of Kansas
 * All rights reserved.
 * 
 * Author: Andy Gill (andygill@ku.edu)
 *
 * invoke the driver, please. Nominal error checking.
 */

#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lb_board_connect.h"

#define DEBUG 0

char *lb_allocint(int value);

int lb_board_connect(int argc,char **argv, int sends, int recvs, int *hds) {
	int wt_hd = sends;
	int rd_hd = recvs;		
	int i = 1;
	int c = 0;
	
	char *lb_path = getenv("LB_PATH");
	if (DEBUG) { fprintf(stderr,"lb_board_connect: LB_PATH=%s\n",lb_path); }
	if (lb_path == NULL) {
		return -1;
	}

	if (argc < 2) {
		return -1;
	}

	for(i = 0;i < argc;i++) {
		if (DEBUG) { fprintf(stderr,"lb_board_connect: argv[%d] = '%s'\n",i,argv[i]); }
	}


	int *wr_fds = malloc(sizeof(int) * (wt_hd * 2));
	for(i = 0;i < wt_hd;i++) {
		pipe(&wr_fds[i*2]);
	}

	int *rd_fds = malloc(sizeof(int) * (rd_hd * 2));
	for(i = 0;i < rd_hd;i++) {
		pipe(&rd_fds[i*2]);
	}


	pid_t pid;
	pid = fork();
	if (pid == 0) {
		// Close end of the pipes we are not going to use
		for(i = 0;i < wt_hd;i++) {
			close(wr_fds[i*2+1]);
		}
		for(i = 0;i < rd_hd;i++) {
			close(rd_fds[i*2]);
		}
		
		char *preamble = ""; // no preamble now "lb_";
		char *driver = malloc(sizeof(char) * (strlen(argv[1]) + strlen(preamble)));
	
		strcpy(&driver[0],preamble);
		strcpy(&driver[strlen(preamble)],argv[1]);

		char **argv2 = malloc(sizeof(char *) * (argc + wt_hd + rd_hd + 3));
		int c = 0;

		argv2[c++] = &driver[0];
		argv2[c++] = lb_allocint(wt_hd);
		for(i = 0;i < wt_hd;i++) {
			argv2[c++] = lb_allocint(wr_fds[i*2]);
		}
		argv2[c++] = lb_allocint(rd_hd);
		for(i = 0;i < rd_hd;i++) {
			argv2[c++] = lb_allocint(rd_fds[i*2+1]);
		}		
		for(i = 1;i < argc;i++) {
			argv2[c++] = argv[i];
		}
		argv2[c] = NULL;


		if (DEBUG) { fprintf(stderr,"lb_board_connect: driver = '%s'\n",driver); }
		for(i = 0;i < c;i++) {
			if (DEBUG) { fprintf(stderr,"lb_board_connect: argv2[%d] = '%s'\n",i,argv2[i]); }
		}

		execvP(driver,lb_path,argv2);

		fprintf(stderr,"The `%s' driver (%s) was not found\n",argv[1],driver);
		exit(-1);
	}

	// Close end of the pipes we are not going to use
	c = 0;
	for(i = 0;i < wt_hd;i++) {
		close(wr_fds[i*2]);
		hds[c++] = wr_fds[i*2+1];
	}
	for(i = 0;i < rd_hd;i++) {
		hds[c++] = rd_fds[i*2];
		close(rd_fds[i*2+1]);
	}

	return 0;
}

char *lb_allocint(int value) {
	char *str = malloc(16);
	sprintf(str,"%d",value);
	return str;
}
	
