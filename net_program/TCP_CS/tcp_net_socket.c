#include "tcp_net_socket.h"

/* init 初始化 */

int tcp_init(const char* ip,int port)
{
	int sfd = socket(AF_INET,SOCK_STREAM,0);

	if(sfd == -1)
	{
		perror("socket");
		exit(-1);
	}

	struct sockaddr_in serveraddr;
	memset(&serveraddr,0,sizeof(struct sockaddr));
	serveraddr.sin_family = AF_INET;
	serveraddr.sin_port = htons(port);
	serveraddr.sin_addr.s_addr = inet_addr(ip);

	/*bind 绑定*/
	if(bind(sfd, (struct sockaddr*)&serveraddr,sizeof(struct sockaddr)) == -1)
	{
	perror("bind");
	close(sfd);
	exit(-1);
	}

	/*listen 监听*/
	if(listen(sfd,10) == -1)
	{
		perror("listen");
		close(sfd);
		exit(-1);
	}
	return sfd;
}

	//accept 接收
int tcp_accept(int sfd)
{
	struct sockaddr_in clientaddr;
	memset(&clientaddr, 0, sizeof(struct sockaddr));
	int new_fd = accept(sfd, (struct sockaddr*)&clientaddr,NULL);

	if(new_fd == -1)
	{
		perror("accept");
		close(sfd);
		exit(-1);
	}
	printf("%s %d success connect \n",inet_ntoa(clientaddr.sin_addr),ntohs(clientaddr.sin_port));
	return sfd;
}

	/*connect 连接*/
int tcp_connect(const char* ip,int port)
{

	/*向系统注册申请新的socket*/
	int sfd = socket(AF_INET,SOCK_STREAM,0);
	if(sfd == -1)
	{
		perror("socket");
		exit(-1);
	}
	struct sockaddr_in serveraddr;
	memset(&serveraddr,0,sizeof(struct sockaddr));
	serveraddr.sin_family = AF_INET;
	serveraddr.sin_port = htons(port);
	serveraddr.sin_addr.s_addr = inet_addr(ip);

	/* 将sfd连接至指定的服务器网络地址serveraddr*/
	

	if(connect(sfd,(struct sockaddr*)&serveraddr,sizeof(struct sockaddr)) == -1)
	{
		perror("connect");
		close(sfd);
		exit(-1);
	}
	return sfd;
}


	/* 用于信号处理，在服务器端按下ctrl +c 或者ctrl+\不会退出*/

void signalhandler(void)
{
	sigset_t sigSet;
	sigemptyset(&sigSet);
	sigaddset (&sigSet,SIGINT);
	sigaddset(&sigSet,SIGQUIT);
	sigprocmask(SIG_BLOCK,&sigSet,NULL);
}







