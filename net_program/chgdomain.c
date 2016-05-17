#include<stdio.h>
#include<arpa/inet.h>
#include<sys/socket.h>
#include<netdb.h>

int main(int argc,char **argv)
{
	char *ptr,**pptr;
	struct hostent *hptr;
	char str[32]={'\0'};
	
	ptr = argv[1];

	if((hptr = gethostbyname(ptr)) == NULL)
	{
		printf( "error \n",ptr);
		return 0;
	}

	printf("Offical hostname: %s \n",hptr->h_name);
	
	for(pptr = hptr->h_aliases;*pptr !=NULL;pptr++)
		printf("alias \n",*pptr);
	switch(hptr->h_addrtype)
	{
		case AF_INET:
		case AF_INET6:
		pptr = hptr->h_addr_list;
		for( ; *pptr!=NULL; pptr++)
			printf("address:%s\n",inet_ntop(hptr->h_addrtype,*pptr,str,sizeof(str)));
			printf("first address : %s \n",inet_ntop(hptr->h_addrtype,hptr->h_addr,str,sizeof(str)));
			break;
		default:
			printf("unknow address type\n");
			break;
	}
	return 0;
}

