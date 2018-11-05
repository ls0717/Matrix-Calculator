#include <cmath>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <algorithm>
using std::swap;
/*****用户参数*****/
int flag=0;//flag=0:frac;flag=1:dec;
/******************/ 

inline int getint()//过滤非正常字符 安全读数 
{
    char ch;
    int p=0;
    while ((ch=getchar())<'0'||ch>'9');
    p*=10;p+=ch-'0';
    while ((ch=getchar())>='0'&&ch<='9') p*=10,p+=ch-'0';
    return p;
}

const int MAXN=105;

class frac//自定义分数类  A/B
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
		bool is_cut(){return A*B==0||gcd(A,B)==1;}//是否最简
		void cutdown()//化简分数 
		{
			if (is_cut()) return;
			if (B==0) return;
			if (A==0){B=1;return;}
			int r=gcd(abs(A),abs(B));
			A=A/r,B=B/r;
		}
		/****************重载运算符*******************/
		
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
				B!=1?printf("%lld/%lld",sgn(A*B)*abs(A),abs(B)):printf("%lld",A);
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
		frac* operator [](int pos){return data[0]+MAXN*pos;};//重载双下标运算符
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
		/***************数乘****************/
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
		
		/****************矩阵乘法*****************/ 
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
		
		/****************初等行变换*****************/
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
		
		/*****************行列式求值****************/
		frac det ()
		{
			frac D(1);
			if (!sq) return frac(-32767);
			for (int i=1;i<r;i++)
			{
				for (int j=i+1;j<=r;j++) 
				{
					add_r(j,i,data[j][i]/data[i][i]*-1);
				}
			}
			for (int i=1;i<=r;i++) D=D*data[i][i];
			return D;
		}
		
		/******************矩阵求逆******************/
		Matrix rev()
		{
			
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
	printf("-----------------------Welcome to Matrix calculator V1.0-----------------------\n\n");
	
	                                                                                
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
	std::cout<<"#请输入计算类型"<<std::endl;
	std::cout<<"1.矩阵加法"<<std::endl;
	std::cout<<"2.矩阵减法"<<std::endl;
	std::cout<<"0.回到上一级"<<std::endl;
	flag=getint();
	if(!flag) return 0;
	if (flag<0||flag>2)
	{
		std::cout<<"#输入不合法!!!"<<std::endl;
		return 1;
	}
	
	Matrix* A=new Matrix;
	Matrix* B=new Matrix;
	std::cout<<"#请输入两个矩阵的行数和列数"<<std::endl;
	std::cin>>r>>c;
	std::cout<<"#请输入第一个矩阵,数字之间可用任意非数字字符分隔"<<std::endl;
	A->get(r,c);
	std::cout<<"#请输入第二个矩阵,数字之间可用任意非数字字符分隔"<<std::endl;
	B->get(r,c); 
	
	std::cout<<"#计算结果是"<<std::endl;
	printf("------------------------------------------------\n"); 
	if (flag==1) ((*A)+(*B))->output();
	if (flag==2) ((*A)-(*B))->output();
	printf("------------------------------------------------\n");
	std::cout<<"#谢谢使用!"<<std::endl;
	return 0;
}

int mul()
{
	int r1,c1,r2,c2,flag;
	std::cout<<"#请输入计算类型"<<std::endl;
	std::cout<<"1.矩阵乘法"<<std::endl; 
	std::cout<<"0.回到上一级"<<std::endl;
	flag=getint();
	if(!flag) return 0;
	if (flag!=1)
	{
		std::cout<<"#输入不合法!!!"<<std::endl;
		return 1;
	}
	
	Matrix* A=new Matrix;
	Matrix* B=new Matrix;
	std::cout<<"#请输入第一个矩阵的行数和列数"<<std::endl;
	std::cin>>r1>>c1;
	std::cout<<"#请输入第一个矩阵,数字之间可用任意非数字字符分隔"<<std::endl;
	A->get(r1,c1);
	std::cout<<"#请输入第二个矩阵的行数和列数"<<std::endl;
	std::cin>>r2>>c2;
	if (c1!=r2)
	{
		std::cout<<"#输入矩阵不可相乘!!!"<<std::endl;
		return 1;
	}
	std::cout<<"#请输入第二个矩阵,数字之间可用任意非数字字符分隔"<<std::endl;
	B->get(r2,c2);
	std::cout<<"#计算结果是"<<std::endl;
	printf("------------------------------------------------\n");
	((*A)*(*B))->output();
	printf("------------------------------------------------\n");
	std::cout<<"#谢谢使用!"<<std::endl;
	return 0;
}

int Det()
{
	int r,c,flag;
	std::cout<<"#请输入计算类型"<<std::endl;
	std::cout<<"1.矩阵行列式"<<std::endl;
	std::cout<<"0.回到上一级"<<std::endl;
	flag=getint();
	if(!flag) return 0;
	if (flag<0||flag>1)
	{
		std::cout<<"#输入不合法!!!"<<std::endl;
		return 1;
	}
	
	Matrix* A=new Matrix;
	std::cout<<"#请输入矩阵的行数和列数"<<std::endl;
	std::cin>>r>>c;
	std::cout<<"#请输入矩阵,数字之间可用任意非数字字符分隔"<<std::endl;
	A->get(r,c);
	
	std::cout<<"#计算结果是"<<std::endl;
	printf("------------------------------------------------\n"); 
	A->det().output();
	putchar('\n'); 
	printf("------------------------------------------------\n");
	std::cout<<"#谢谢使用!"<<std::endl;
	return 0;
}
int setting()
{
	std::cout<<"#请输入设置类型"<<std::endl;
	std::cout<<"1.分数输出"<<std::endl;
	std::cout<<"2.小数输出"<<std::endl;
	std::cout<<"0.回到上一级"<<std::endl;
	int Flag=getint();
	if(!Flag) return 0;
	if (Flag<0||Flag>2)
	{
		std::cout<<"#输入不合法!!!"<<std::endl;
		return 1;
	}
	if (Flag==1) flag=0;
	if (Flag==2) flag=1;
	std::cout<<"设置更新成功"<<std::endl;
	return 0; 
}
int about()
{
	putchar('\n'); 
	std::cout<<"------------------Matrix calculator V1.0-------------------"<<std::endl;
	std::cout<<"--------后续会加入矩阵求逆,LU分解,导出到表格等功能---------"<<std::endl;
	std::cout<<"------------更新的软件版本及源代码会放在GitHub上-----------"<<std::endl;
	std::cout<<"-------https://github.com/mrrubiks/Matrix-Calculator-------"<<std::endl;
	std::cout<<"--------------------------谢谢使用-------------------------"<<std::endl;
	std::cout<<"------------------------作者:张铁沄------------------------"<<std::endl;
	putchar('\n');
}
int function_select()
{
	std::cout<<"#请输入计算类型,按Ctrl+C退出"<<std::endl;
	std::cout<<"1.矩阵加减法"<<std::endl;
	std::cout<<"2.矩阵乘法"<<std::endl;
	std::cout<<"3.矩阵行列式"<<std::endl;
	std::cout<<"9.功能更新及源代码"<<std::endl;
	std::cout<<"0.设置"<<std::endl;
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
	else if (select==0)
	{
		while (setting());
	}
	else if (select==9)
	{
		about(); 
	}
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
