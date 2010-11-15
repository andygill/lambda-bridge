#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int lb_board_connect(int argc,char **argv,int *hds);
// <cmd> 
// -w <number of writing fifos> [default 1]
// -r <number of reading fifos> [default 1]


// Two arguments are added
//  * a comma seperated list of writing channels
//  * a comma seperated list of reading channels

// In Haskell
// [String] -> IO [Handle]



main(int argc,char **argv){
	printf("Hello (%d)\n",argc);
	int conn = lb_board_connect(argc,argv,NULL);
	if (conn == -1) {
		exit(0);
	}
	char *str = malloc(100);
	int i;
	int sz;
	for(i=0;i<10000;i++) {
		printf("loop: %d\n",i);
		sz = write(4,"Hello World",11);
		printf("1 (%d)\n",sz);
		sz = write(4,"This is a test",14);
		printf("1 (%d)\n",sz);
	}
	exit(0);
//	while(1) {
//		sz = read(5,str,1);
//		printf("(%d)%c\n",sz,str[0]);
//	}
}


int lb_board_connect(int argc,char **argv,int *hds) {
	int wt_hd = 1;
	int rd_hd = 1;		
	int i = 1;
	
	char *lb_path = getenv("LB_PATH");
	printf("(%s)\n",lb_path);
	if (lb_path == NULL) {
		return -1;
	}

	if (argc < 2) {
		return -1;
	}


	char **argv2 = malloc(sizeof(char *) * argc);
	int c2 = 0;
		
	while(i < argc) {
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


	int *wr_fds = malloc(sizeof(int) * (wt_hd * 2));
	for(i = 0;i < wt_hd;i++) {
		pipe(&wr_fds[i*2]);
	}

	int *rd_fds = malloc(sizeof(int) * (rd_hd * 2));
	for(i = 0;i < rd_hd;i++) {
		pipe(&rd_fds[i*2]);
	}


	for(i = 0;i < c2;i++) {
		printf("(%s)\n",argv2[i]);
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
		
		char **argv3 = malloc(sizeof(char *) * (c2 + wt_hd + rd_hd + 2));
		int c3 = 0;
		for(i = 1;i < argc;i++) {
			argv3[c3++] = argv[i];
		}
		argv3[c3++] = "--write-fds";
		for(i = 0;i < wt_hd;i++) {
			char *str = malloc(5);
			sprintf(str,"%d",wr_fds[i*2]);
			argv3[c3++] = str;
		}
		argv3[c3++] = "--read-fds";
		for(i = 0;i < rd_hd;i++) {
			char *str = malloc(5);
			sprintf(str,"%d",rd_fds[i*2+1]);
			argv3[c3++] = str;
		}		
		argv3[c3] = NULL;

		for(i = 0;i <= c3;i++) {
			printf(":: (%s)\n",argv3[i]);
		}
		
		char *preamble = "lb_";
		char *driver = malloc(sizeof(char) * (strlen(argv[1]) + strlen(preamble)));
	
		strcpy(&driver[0],preamble);
		strcpy(&driver[strlen(preamble)],argv[1]);

		printf("(%s)::\n",driver);

		execvP(driver,lb_path,argv3);

		fprintf(stderr,"The `%s' driver (%s) was not found\n",argv[1],driver);

		exit(0);
	}

	// Close end of the pipes we are not going to use
	for(i = 0;i < wt_hd;i++) {
		close(wr_fds[i*2]);
	}
	for(i = 0;i < rd_hd;i++) {
		close(rd_fds[i*2+1]);
	}


	printf("%d %d\n",wt_hd,rd_hd);


}
	
	
