#导入你发给我的csv文件
library(readr)
data1 <- read_csv("Downloads/data1.csv")
View(data1)
#把要处理的第一列转为字符格式
data1$ASSTTGSLNGYT = as.character(data1$ASSTTGSLNGYT)
#看一下生效没有
typeof(data1[,1])
#肉眼判断是要统计26个大写英文字母，所以通通先初始化为0，其中第一列的每一个单元格不含有异常值、空值等情况
A_numbers=0
B_numbers=0
C_numbers=0
D_numbers=0
E_numbers=0
F_numbers=0
G_numbers=0
H_numbers=0
I_numbers=0
J_numbers=0
K_numbers=0
L_numbers=0
M_numbers=0
N_numbers=0
O_numbers=0
P_numbers=0
Q_numbers=0
R_numbers=0
S_numbers=0
T_numbers=0
U_numbers=0
V_numbers=0
W_numbers=0
X_numbers=0
Y_numbers=0
Z_numbers=0


#初始化向量变量
colume=c()

data1 <- transform(data1,Four_times_frequency_of_A=0)
data1 <- transform(data1,Four_times_frequency_of_B=0)
data1 <- transform(data1,Four_times_frequency_of_C=0)
data1 <- transform(data1,Four_times_frequency_of_D=0)
data1 <- transform(data1,Four_times_frequency_of_E=0)
data1 <- transform(data1,Four_times_frequency_of_F=0)
data1 <- transform(data1,Four_times_frequency_of_G=0)
data1 <- transform(data1,Four_times_frequency_of_H=0)
data1 <- transform(data1,Four_times_frequency_of_I=0)
data1 <- transform(data1,Four_times_frequency_of_J=0)
data1 <- transform(data1,Four_times_frequency_of_K=0)
data1 <- transform(data1,Four_times_frequency_of_L=0)
data1 <- transform(data1,Four_times_frequency_of_M=0)
data1 <- transform(data1,Four_times_frequency_of_N=0)
data1 <- transform(data1,Four_times_frequency_of_O=0)
data1 <- transform(data1,Four_times_frequency_of_P=0)
data1 <- transform(data1,Four_times_frequency_of_Q=0)
data1 <- transform(data1,Four_times_frequency_of_R=0)
data1 <- transform(data1,Four_times_frequency_of_S=0)
data1 <- transform(data1,Four_times_frequency_of_T=0)
data1 <- transform(data1,Four_times_frequency_of_U=0)
data1 <- transform(data1,Four_times_frequency_of_V=0)
data1 <- transform(data1,Four_times_frequency_of_W=0)
data1 <- transform(data1,Four_times_frequency_of_X=0)
data1 <- transform(data1,Four_times_frequency_of_Y=0)
data1 <- transform(data1,Four_times_frequency_of_Z=0)


#第一列一共95588行，每一行都进行如下骚操作
for (i in 1:95588){
  #要把变量归零，要不然又累加了
  A_numbers = 0
  #按你的思路用strsplit函数返回list数据类型
  colume<-strsplit(data1$ASSTTGSLNGYT[i],split = "")
  #百度一下你就知道，从list转vector
  vcolume = as.vector(unlist(colume))
  #每次都从1开始循环，所以j赋值为1
  j=1
  #判断每一行的向量长度作为结束条件
  while(j<=length(vcolume)){
    #达到你想要的条件，咱就让他+1，要写26次，奶奶的
    if(vcolume[j]=="A") {
      A_numbers<-A_numbers+1
      
      #每一行的第五列赋值，第五列是A
      data1[i,5]=A_numbers   
    }
    
    #while循环终止，所以要自增
    j<-j+1
  }
} 
#结果出来了，任你处置

