#include <cmath>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <algorithm>
using std::swap;
/*****�û�����*****/
int flag=0;//flag=0:frac;flag=1:dec;
int step=0;//�м䲽�� 
/******************/ 

inline int getint()//���˷������ַ� ��ȫ���� 
{
    char ch;
    int p=0,fh=1;
    while (((ch=getchar())<'0'||ch>'9')&&ch!='-');
	if (ch!='-') p+=ch-'0';
	else fh=-1;
    while ((ch=getchar())>='0'&&ch<='9') p*=10,p+=ch-'0';
    return p*fh;
}

const int MAXN=105;

class frac//�Զ��������  A/B
{
	protected:
		long long A,B;
	public:
		int sgn(int a){return a<0?-1:1;}
		long long abs(long long a){return a>0?a:-a;}
		int gcd(int a,int b){return a%b?gcd(b,a%b):b;}
		frac(){A=0,B=1;};
		frac(int _A,int _B):A(_A),B(_B){cutdown();}
		frac(int q):A(q),B(1){}
		void init(){A=0,B=1;}
		bool is_cut(){return A*B==0||gcd(A,B)==1;}//�Ƿ����
		void cutdown()//������� 
		{
			if (B==0) return;
			if (A==0){B=1;return;}
			int r=gcd(abs(A),abs(B));
			A=A/r,B=B/r;
			if (A*B>=0) A=abs(A),B=abs(B);
			else A=-abs(A),B=abs(B); 
		}
		/****************���������*******************/
		
		/*****plus*****/
		frac operator + (const frac &p)
		{
			return frac(A*p.B+p.A*B,B*p.B);
		}
		frac operator + (const int &p)
		{
			return (*this)+frac(p);
		}
		friend frac operator + (const int &p,const frac &q)
		{
			return frac(p)+q;
		}
		
		/*****minus*****/
		frac operator - (const frac &p)
		{
			return frac(A*p.B-p.A*B,B*p.B);
		}
		frac operator - (const int &p)
		{
			return (*this)-frac(p);
		}
		friend frac operator - (const int &p,const frac &q)
		{
			return frac(p)-q;
		}
		
		/*****mult*****/
		frac operator * (const frac &p)
		{
			return frac(A*p.A,B*p.B);
		}
		frac operator * (const int &p)
		{
			return (*this)*frac(p);
		}
		friend frac operator * (const int &p,const frac &q)
		{
			return frac(p)*q;
		}
		
		/*****div*****/
		frac operator / (const frac &p)
		{
			return frac(A*p.B,B*p.A);
		}
		frac operator / (const int &p)
		{
			return (*this)/frac(p);
		}
		friend frac operator / (const int &p,const frac &q)
		{
			return frac(p)/q;
		}
		
		/*****logic*****/
		bool operator ()()
		{
			return (bool)A;
		}
		bool operator !()
		{
			return !(*this)();
		}
		bool operator == (const frac &p)
		{
			return !(*this-p);
		}
		bool operator == (const int &q)
		{
			return !(*this-q);
		}
		friend bool operator == (const int &q,const frac &p)
		{
			return !(q-p);
		}
		bool neg()
		{
			return A*B<0;
		}
		bool operator < (const frac &p)
		{
			return (*this-p).neg();
		}
		bool operator < (const int &p)
		{
			return (*this-p).neg();
		}
		friend bool operator < (const int &q,const frac &p)
		{
			return (q-p).neg();
		}
		/****************IO******************/
		void operator = (const frac &p)
		{
			A=p.A,B=p.B;
			cutdown();
		}
		void operator = (const int &q)
		{
			*this=frac(q);
		}
		void get()
		{
			int q=getint();
			*this=q;
		}
		void output()
		{
			if (!flag)
			{
				cutdown();
				if ((int)B-1) printf("%lld/%lld",sgn(A*B)*abs(A),abs(B));
				else printf("%lld",A);
			}
			else printf("%f",1.0*A/B);
		}
}p;

class Matrix
{
	protected:
		int r,c;
		int size,sq;
		frac data[MAXN][MAXN];
	public:
		Matrix():r(0),c(0),size(0),sq(0){}
		Matrix(int _r,int _c):r(_r),c(_c){size=r*c;sq=(r==c);}
		Matrix(int _r,int _c,int k):r(_r),c(_c)
		{
			size=r*c;sq=(r==c);
			for (int i=1;i<=r;i++) data[i][i]=k;
		}
		frac* operator [](int pos){return data[0]+MAXN*pos;};//����˫�±������
		Matrix* operator + (Matrix M)
		{
			if (r==M.r&&c==M.c)
			{
				Matrix* temp=new Matrix(r,c);
				for (int i=1;i<=r;i++)
				{
					for (int j=1;j<=c;j++)
					{
						(*temp)[i][j]=(*this)[i][j]+M[i][j];
					}
				}
				return temp;
			}
		}
		Matrix* operator - (Matrix M)
		{
			if (r==M.r&&c==M.c)
			{
				Matrix* temp=new Matrix(r,c);
				for (int i=1;i<=r;i++)
				{
					for (int j=1;j<=c;j++)
					{
						(*temp)[i][j]=(*this)[i][j]-M[i][j];
					}
				}
				return temp;
			}
		}
		/***************����****************/
		Matrix* operator * (int q)
		{
			Matrix* temp=new Matrix(r,c);
			for (int i=1;i<=r;i++)
			{
				for (int j=1;j<=c;j++)
				{
					(*temp)[i][j]=(*this)[i][j]*q;
				}
			}
			return temp;
		}
		friend Matrix* operator * (int q,Matrix &M)
		{
			Matrix* temp=new Matrix(M.r,M.c);
			for (int i=1;i<=M.r;i++)
			{
				for (int j=1;j<=M.c;j++)
				{
					(*temp)[i][j]=M[i][j]*q;
				}
			}
			return temp;
		}
		
		/****************����˷�*****************/ 
		Matrix* operator * (Matrix &M)
		{
			if (c==M.r)
			{
				Matrix* temp=new Matrix(r,M.c);
				for (int i=1;i<=r;i++)
				{
					for (int j=1;j<=M.c;j++)
					{
						for (int k=1;k<=c;k++)
						{
							(*temp)[i][j]=(*temp)[i][j]+(*this)[i][k]*M[k][j];
						}
					}
				}
				return temp;
			}
		}
		
		/****************���ȱ任*****************/
		void swap_r(int a,int b)
		{
			for (int i=1;i<=c;i++) swap(data[a][i],data[b][i]);
		}
		void swap_c(int a,int b)
		{
			for (int i=1;i<=r;i++) swap(data[i][a],data[i][b]);
		}
		void mult_r(int _r,frac p)
		{
			for (int i=1;i<=c;i++) data[_r][i]=data[_r][i]*p;
		}
		void mult_c(int _c,frac p)
		{
			for (int i=1;i<=r;i++) data[i][_c]=data[i][_c]*p;
		}
		void add_r(int target,int bullet)
		{
			for (int i=1;i<=c;i++) data[target][i]=data[target][i]+data[bullet][i];
		}
		void add_r(int target,int bullet,frac speed)
		{
			for (int i=1;i<=c;i++) data[target][i]=data[target][i]+data[bullet][i]*speed;
		}
		void add_c(int target,int bullet)
		{
			for (int i=1;i<=r;i++) data[i][target]=data[i][target]+data[i][bullet];
		}
		void add_c(int target,int bullet,frac speed)
		{
			for (int i=1;i<=r;i++) data[i][target]=data[i][target]+data[i][bullet]*speed;
		}
		
		/*****************����ʽ��ֵ****************/
		frac det ()
		{
			Matrix *temp=new Matrix(r,c,1);//����ԭ���� 
			*temp=*this;
			frac D(1);
			if (!sq) return frac(-32767);
			for (int i=1;i<r;i++)
			{
				if (data[i][i]==0)
				{
					for (int j=i+1;j<=r;j++)
					{
						if(!(data[j][i]==0))
						{
							swap_r(i,j);
							D=D*-1;
							if (step) 
							{
								printf("----------------------------\n");
								output();
								printf("----------------------------\n");
							}
							break;
						}
					}
				}
				if (data[i][i]==0) return frac(0);
				for (int j=i+1;j<=r;j++)
				{
					add_r(j,i,data[j][i]/data[i][i]*-1);
					if (step) 
					{
						printf("----------------------------\n");
						output();
						printf("----------------------------\n");
					}
				}
			}
			for (int i=1;i<=r;i++) 
			{
				D=D*data[i][i];
				if (step) 
				{
					printf("----------------------------\n");
					output();
					printf("----------------------------\n");
				}
			}
			*this=*temp;//�ָ� 
			return D;
		}
		
		/******************��������******************/
		Matrix* rev()
		{
			Matrix *ans=new Matrix(r,c,1);
			Matrix *temp=new Matrix(r,c,1);//����ԭ���� 
			*temp=*this;
			for (int i=1;i<=r;i++)
			{
				if (data[i][i]==0)
				{
					for (int j=i+1;j<=r;j++)
					{
						if(!(data[j][i]==0))
						{
							swap_r(i,j);
							ans->swap_r(i,j);
							if (step) 
							{
								printf("----------------------------\n");
								output();
								putchar('\n');
								ans->output();
								printf("----------------------------\n");
							}
							break;
						}
					}
				}
				for (int j=i+1;j<=r;j++)
				{
					ans->add_r(j,i,data[j][i]/data[i][i]*-1);//ע��������˳�����⣡����
					add_r(j,i,data[j][i]/data[i][i]*-1);
					if (step) 
					{
						printf("----------------------------\n");
						output();
						putchar('\n');
						ans->output();
						printf("----------------------------\n");
					}
				}
			}
			for (int i=r;i>1;i--)
			{
				for (int j=1;j<i;j++)
				{
					ans->add_r(j,i,data[j][i]/data[i][i]*-1);
					add_r(j,i,data[j][i]/data[i][i]*-1);
					if (step) 
					{
						printf("----------------------------\n");
						output();
						putchar('\n');
						ans->output();
						printf("----------------------------\n");
					}
				}
			}
			for (int i=1;i<=r;i++)
			{
				ans->mult_r(i,1/data[i][i]);
				mult_r(i,1/data[i][i]);
				if (step) 
				{
					printf("----------------------------\n");
					output();
					putchar('\n');
					ans->output();
					printf("----------------------------\n");
				}
			}
			*this=*temp;//�ָ�
			return ans;
		}
		
		
		
		/*******************IO********************/
		void get(int _r,int _c)
		{
			r=_r,c=_c;
			size=r*c;
			sq=r==c;
			for (int i=1;i<=r;i++)
			{
				for (int j=1;j<=c;j++)
				{
					data[i][j].get();
				}
			}
		}
		void output()
		{
			for (int i=1;i<=r;i++)
			{
				for (int j=1;j<=c;j++)
				{
					(*this)[i][j].output();
					putchar(' ');
				}
				putchar('\n');
			}
		}
}M,N;


void greeting()
{
	printf("-------------------------------------------------------------------------------\n\n");
	printf("-----------------------Welcome to Matrix calculator V2.1-----------------------\n\n");
	
	                                                                                
	printf("                                     ######                                    \n");
	printf("                                  ##############     ######################### \n");  
	printf("###########################      ################ ##########################   \n");    
	printf("############################     ############    ##########################    \n");
	printf(" ###########################    ###########    ########################        \n");
	printf("    ##########################  #############  ################                \n"); 
	printf("               #######################################################         \n"); 
	printf("               #########################################################       \n"); 
	printf("       ################################################################        \n"); 
	printf("       ###############################################################         \n"); 
	printf("        #########################################################              \n");
	printf("           ################################################                    \n"); 
	printf("                   ########################################                    \n"); 
	printf("                   ###############################################             \n"); 
	printf("             ####################################################              \n"); 
	printf("              ##################################################               \n"); 
	printf("               ###############################################                 \n"); 
	printf("                  ######################################                       \n"); 
	printf("                        #####################################                  \n"); 
	printf("                    #########################################                  \n"); 
	printf("                   #########################################                   \n"); 
	printf("                     ######################################                    \n"); 
	printf("                       ##############################                          \n"); 
	printf("                            ########################                           \n"); 
	printf("                            ############################                       \n"); 
	printf("                         ###############################                       \n"); 
	printf("                          #############################                        \n"); 
	printf("                             #######################                           \n"); 
	printf("                                 ################                              \n"); 
	printf("                                 ################                              \n"); 
	printf("                                *################*                             \n"); 
	printf("                                ##################                             \n"); 
	printf("                                 ################                              \n"); 
	printf("                                  ##############                               \n"); 
	printf("                                   ############                                \n"); 
	printf("                                    ##########                                 \n"); 
	printf("                                     ########                                  \n");
	printf("                                      ######                                   \n"); 
	printf("                                       ####                                    \n"); 
	printf("                                        ##                                     \n"); 
	printf("                                                                               \n"); 
	printf("----------------------------------Made by tieyun--------------------------------\n");
}
int plus()
{
	int r,c,flag;
	std::cout<<"#�������������"<<std::endl;
	std::cout<<"1.����ӷ�"<<std::endl;
	std::cout<<"2.�������"<<std::endl;
	std::cout<<"0.�ص���һ��"<<std::endl;
	flag=getint();
	if(!flag) return 0;
	if (flag<0||flag>2)
	{
		std::cout<<"#���벻�Ϸ�!!!"<<std::endl;
		return 1;
	}
	
	Matrix* A=new Matrix;
	Matrix* B=new Matrix;
	std::cout<<"#�������������������������"<<std::endl;
	std::cin>>r>>c;
	std::cout<<"#�������һ������,����֮���������������ַ��ָ�"<<std::endl;
	A->get(r,c);
	std::cout<<"#������ڶ�������,����֮���������������ַ��ָ�"<<std::endl;
	B->get(r,c); 
	
	std::cout<<"#��������"<<std::endl;
	printf("------------------------------------------------\n"); 
	if (flag==1) ((*A)+(*B))->output();
	if (flag==2) ((*A)-(*B))->output();
	printf("------------------------------------------------\n");
	std::cout<<"#ллʹ��!"<<std::endl;
	return 1;
}

int mul()
{
	int r1,c1,r2,c2,flag;
	std::cout<<"#�������������"<<std::endl;
	std::cout<<"1.����˷�"<<std::endl; 
	std::cout<<"0.�ص���һ��"<<std::endl;
	flag=getint();
	if(!flag) return 0;
	if (flag!=1)
	{
		std::cout<<"#���벻�Ϸ�!!!"<<std::endl;
		return 1;
	}
	
	Matrix* A=new Matrix;
	Matrix* B=new Matrix;
	std::cout<<"#�������һ�����������������"<<std::endl;
	std::cin>>r1>>c1;
	std::cout<<"#�������һ������,����֮���������������ַ��ָ�"<<std::endl;
	A->get(r1,c1);
	std::cout<<"#������ڶ������������������"<<std::endl;
	std::cin>>r2>>c2;
	if (c1!=r2)
	{
		std::cout<<"#������󲻿����!!!"<<std::endl;
		return 1;
	}
	std::cout<<"#������ڶ�������,����֮���������������ַ��ָ�"<<std::endl;
	B->get(r2,c2);
	std::cout<<"#��������"<<std::endl;
	printf("------------------------------------------------\n");
	((*A)*(*B))->output();
	printf("------------------------------------------------\n");
	std::cout<<"#ллʹ��!"<<std::endl;
	return 1;
}

int Det()
{
	int r,c,flag;
	std::cout<<"#�������������"<<std::endl;
	std::cout<<"1.��������ʽ"<<std::endl;
	std::cout<<"0.�ص���һ��"<<std::endl;
	flag=getint();
	if(!flag) return 0;
	if (flag<0||flag>1)
	{
		std::cout<<"#���벻�Ϸ�!!!"<<std::endl;
		return 1;
	}
	
	Matrix* A=new Matrix;
	std::cout<<"#���������Ľ�"<<std::endl;
	std::cin>>r;c=r;
	std::cout<<"#���������,����֮���������������ַ��ָ�"<<std::endl;
	A->get(r,c);
	
	std::cout<<"#��������"<<std::endl;
	printf("------------------------------------------------\n"); 
	A->det().output();
	putchar('\n'); 
	printf("------------------------------------------------\n");
	std::cout<<"#ллʹ��!"<<std::endl;
	return 1;
}

int Rev()
{
	int r,c,flag;
	std::cout<<"#�������������"<<std::endl;
	std::cout<<"1.��������"<<std::endl;
	std::cout<<"0.�ص���һ��"<<std::endl;
	flag=getint();
	if(!flag) return 0;
	if (flag<0||flag>1)
	{
		std::cout<<"#���벻�Ϸ�!!!"<<std::endl;
		return 1;
	}
	
	Matrix* A=new Matrix;
	Matrix* B=new Matrix;
	std::cout<<"#���������Ľ�"<<std::endl;
	std::cin>>r;c=r; 
	std::cout<<"#���������,����֮���������������ַ��ָ�"<<std::endl;
	A->get(r,c);
	int step_temp=step;
	step=0;
	if (A->det()==0) 
	{
		std::cout<<"#����ʽΪ��,���벻�Ϸ�!!!"<<std::endl;
		step=step_temp;
		return 1;
	}
	step=step_temp;
	std::cout<<"#��������"<<std::endl;
	printf("------------------------------------------------\n"); 
	A->rev()->output();
	putchar('\n');
	printf("------------------------------------------------\n");
	std::cout<<"#ллʹ��!"<<std::endl;
	return 1;
}

int setting()
{
	std::cout<<"#��������������"<<std::endl;
	std::cout<<"1.�������"<<std::endl;
	std::cout<<"2.С�����"<<std::endl;
	std::cout<<"3.����м䲽��"<<std::endl;
	std::cout<<"4.������м䲽��"<<std::endl;
	std::cout<<"0.�ص���һ��"<<std::endl;
	int Flag=getint();
	if(!Flag) return 0;
	if (Flag<0||Flag>5)
	{
		std::cout<<"#���벻�Ϸ�!!!"<<std::endl;
		return 1;
	}
	if (Flag==1) flag=0;
	if (Flag==2) flag=1;
	if (Flag==3) step=1;
	if (Flag==4) step=0;
	std::cout<<"#���ø��³ɹ�"<<std::endl;
	return 1;
}
int about()
{
	putchar('\n'); 
	std::cout<<"------------------Matrix calculator V2.1-------------------"<<std::endl;
	std::cout<<"-------------���μ����˾��������������̹���--------------"<<std::endl;
	std::cout<<"-------------���������LU�ֽ�,���������ȹ���--------------"<<std::endl;
	std::cout<<"------------���µ�����汾��Դ��������GitHub��-----------"<<std::endl;
	std::cout<<"-------https://github.com/mrrubiks/Matrix-Calculator-------"<<std::endl;
	std::cout<<"--------------------------ллʹ��-------------------------"<<std::endl;
	std::cout<<"------------------------����:�����V------------------------"<<std::endl;
	putchar('\n');
}
int function_select()
{
	std::cout<<"#�������������"<<std::endl;
	std::cout<<"1.����Ӽ���"<<std::endl;
	std::cout<<"2.����˷�"<<std::endl;
	std::cout<<"3.��������ʽ"<<std::endl;
	std::cout<<"4.��������"<<std::endl;
	std::cout<<"8.���ܸ��¼�Դ����"<<std::endl;
	std::cout<<"9.����"<<std::endl;
	std::cout<<"0.�˳�"<<std::endl;
	int select; 
	select=getint();
	if (select==1)
	{
		while (plus());
	}
	else if (select==2)
	{
		while (mul());
	}
	else if (select==3)
	{
		while (Det()); 
	}
	else if (select==4)
	{
		while (Rev()); 
	}
	else if (select==8)
	{
		about(); 
	}
	else if (select==9)
	{
		while (setting());
	}
	else if (select==0) return 1;
	return 0;
}



int main()
{
	greeting();
	while (true)
	{
		if (function_select()) break;
	}
	return 0;
}
