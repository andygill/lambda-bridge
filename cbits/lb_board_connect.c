#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lb_board_connect.h"

char *lb_allocint(int value);

int lb_board_connect(int argc,char **argv, int sends, int recvs, int *hds) {
	int wt_hd = sends;
	int rd_hd = recvs;		
	int i = 1;
	int c = 0;
	
	char *lb_path = getenv("LB_PATH");
	printf("(%s)\n",lb_path);
	if (lb_path == NULL) {
		return -1;
	}

	if (argc < 2) {
		return -1;
	}

	for(i = 0;i < argc;i++) {
		printf("%d(%s)\n",i,argv[i]);
	}

/*
	char **argv2 = malloc(sizeof(char *) * argc);
	int c2 = 0;
		
	while(i < argc) {
		printf("%d => <<%s>>\n",i,argv[i]);
		if (strcmp(argv[i],"-w") == 0) {
			i++;
			if (i < argc) {
				wt_hd = atoi(argv[i]);
			}
	} else if (strcmp(argv[i],"-r") == 0) {
			i++;
			if (i < argc) {
				rd_hd = atoi(argv[i]);
			}
		} else {
			argv2[c2++] = argv[i];
		}
		i++;
	}
*/

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
		
		char *preamble = "lb_";
		char *driver = malloc(sizeof(char) * (strlen(argv[1]) + strlen(preamble)));
	
		strcpy(&driver[0],preamble);
		strcpy(&driver[strlen(preamble)],argv[1]);

		char **argv3 = malloc(sizeof(char *) * (argc + wt_hd + rd_hd + 3));
		int c3 = 0;

		argv3[c3++] = &driver[0];
		argv3[c3++] = lb_allocint(wt_hd);
		for(i = 0;i < wt_hd;i++) {
			argv3[c3++] = lb_allocint(wr_fds[i*2]);
		}
		argv3[c3++] = lb_allocint(rd_hd);
		for(i = 0;i < rd_hd;i++) {
			argv3[c3++] = lb_allocint(rd_fds[i*2+1]);
		}		
		for(i = 1;i < argc;i++) {
			argv3[c3++] = argv[i];
		}
		argv3[c3] = NULL;

		for(i = 0;i <= c3;i++) {
			printf(":: (%s)\n",argv3[i]);
		}
		
		printf("(%s)::\n",driver);

		execvP(driver,lb_path,argv3);

		fprintf(stderr,"The `%s' driver (%s) was not found\n",argv[1],driver);

		exit(0);
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

	printf("%d %d\n",wt_hd,rd_hd);

	return 0;
}

char *lb_allocint(int value) {
	char *str = malloc(16);
	sprintf(str,"%d",value);
	return str;
}
	
