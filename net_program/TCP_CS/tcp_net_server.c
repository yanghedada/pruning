#include"tcp_net_socket.h"
int main(int argc,char* argv[])
{
	if(argc < 3)
	{
		printf("usage : ./servertcp ip port\n");
		exit(-1);
	}
	signalhandler();
	int sfd = tcp_init(argv[1],atoi(argv[2]));


	while(1)  //用while循环表示可以与多个客户端接收和发送，但仍然是阻塞模式的
	{
		int cfd = tcp_accept(sfd);
		char buf[512] = {0};
		
		if(recv(cfd,buf,sizeof(buf),0) == -1) //从cfd客户端接收数据存放在buf中
		{
			perror("recv");
			close(cfd);
			close(sfd);
			exit(-1);
		}
		puts(buf);
		

		if(send(cfd,"hello",12,0) == -1) //从buf中取向cfd客户端发送数据
		{
			perror("send");
			close(cfd);
			close(sfd);
			exit(-1);
		}
		close(cfd);
	}
	close(sfd);
	return 0;
}

