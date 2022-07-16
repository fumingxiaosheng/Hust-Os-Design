#include<stdio.h>
#include<stdlib.h>
#include<sys/types.h>
#include<string.h>
#include <fcntl.h> // for open
#include <unistd.h> // for close

#define BUFFER_SIZE 32
char inputBuf[32], outputBuf[32];
int main(){
	int fd, m, n;
	char c;
	fd = open("/dev/FIFOWithBlock", O_RDWR);
	
	if(fd < 0){
		printf("open /dev/FIFOWithBlock failed\n");
		//exit(-1);
	}
	inputBuf[1] = '\0';
	char str[100] = {0};
	while(1){
		printf("input a sentence,input \"quit\" to end\n");
		fgets(inputBuf, sizeof(inputBuf), stdin);
		if(!strcmp(inputBuf,"quit\n"))
			break;
		inputBuf[strlen(inputBuf)+1]='\0';
		n = write(fd, inputBuf, strlen(inputBuf)*sizeof(char));
		printf("write done %d words!\n",n);

	}
	close(fd);
	return 0;
}
