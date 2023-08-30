library (ggplot2)
library(tidyverse)
library(ggtree)  #聚类
library(aplot)   #拼图
library (reshape2)#数据转换
require(scales)#数据缩放
library(ggtree)#聚类
library(aplot)#拼图
##第一组
beta1_A <- read.delim("heatmapdata1_beta_1.txt",row.names = 1)#数据行为基因，列为样本
beta1_B <- beta1_A
beta1_B <- t(beta1_B)
beta1_B %>% scale() %>% as.data.frame()#缩放数据
beta1_C <- beta1_B %>% scale(center = T) %>% as.data.frame()#以行缩放  #中间有
beta1_C <- beta1_C%>% mutate(beta1_B=row.names(.)) %>% melt()#转化为ggplot画图需要的长列表
beta1_A <- read.delim("heatmapdata1_beta_1.txt",row.names = 1)#数据行为基因，列为样本

p1_A <- read.delim("heatmapdata1_p_1.txt",row.names = 1)#数据行为基因，列为样本
p1_B <- p1_A
p1_B <- t(p1_B)
p1_B %>% scale() %>% as.data.frame()#缩放数据
p1_C <- p1_B %>%  as.data.frame()#以行缩放  #中间有
p1_C <- p1_C%>% mutate(p1_B=row.names(.)) %>% melt()#转化为ggplot画图需要的长列表

D_1 <- beta1_C
D_1$P.value <- p1_C$value
colnames(D_1)[3] <- "Z_score"
D_1 <- D_1 %>% mutate(label = case_when( #设置label，并加入判断，当P值符合特定条件就显示"\n"外加特定数量的*号
  is.na(P.value) ~ " ", #NA值赋值为空格
  P.value <= 2.63e-5 ~ "***", #P<0.001就显示回车加三个星号
  between(P.value, 2.63e-5, 0.001) ~ "**", #P为0.001-0.01 显示回车加两个*号
  between(P.value, 0.001, 0.05) ~ "*", #P为0.01-0.05 显示回车加一个星号
  T ~ ""
))

p1<-ggplot(D_1,aes(x=beta1_B,y=variable,fill= Z_score)) #热图绘制
p1 <- p1+geom_raster()+scale_fill_gradient2(low="#ff0000", high="#0000ff", mid="white")+geom_text(aes(label=D_2$label),col ="black",size = 5)

p1 <- p1 + theme_minimal() + # 不要背景
  theme(text=element_text(size=16, family="serif"),
    axis.title.x=element_blank(), # 去掉 title
        axis.ticks.x=element_blank(), # 去掉x 轴
        axis.title.y=element_blank(), # 去掉 y 轴
        axis.text.x = element_text(angle = 45), # 调整x轴文字，字体加粗
        axis.text.y = element_text()) + #调整y轴文字
        labs(fill =paste0(" * 0.001 < p < 0.05","\n","** 2.63e-5 < p < 0.001","\n","*** p < 2.63e-5"))    # 修改 legend 内容
#p1 <- p1 +theme(legend.position = "none")
##第二组
beta2_A <- read.delim("heatmapdata1_beta_2.txt",row.names = 1)#数据行为基因，列为样本

beta2_B <- beta2_A
beta2_B <- t(beta2_B)
beta2_B %>% scale() %>% as.data.frame()#缩放数据
beta2_C <- beta2_B %>% scale(center = T) %>% as.data.frame()#以行缩放  #中间有
beta2_C <- beta2_C%>% mutate(beta2_B=row.names(.)) %>% melt()#转化为ggplot画图需要的长列表
beta2_A <- read.delim("heatmapdata1_beta_1.txt",row.names = 1)#数据行为基因，列为样本

p2_A <- read.delim("heatmapdata1_p_1.txt",row.names = 1)#数据行为基因，列为样本
p2_B <- p2_A
p2_B <- t(p2_B)
p2_B %>% scale() %>% as.data.frame()#缩放数据
p2_C <- p2_B %>%  as.data.frame()#以行缩放  #中间有
p2_C <- p2_C%>% mutate(p2_B=row.names(.)) %>% melt()#转化为ggplot画图需要的长列表

D_2 <- beta2_C
D_2$P.value <- p2_C$value
colnames(D_2)[3] <- "Z_score"
D_2 <- D_2 %>% mutate(label = case_when( #设置label，并加入判断，当P值符合特定条件就显示"\n"外加特定数量的*号
  is.na(P.value) ~ " ", #NA值赋值为空格
  P.value <= 2.63e-5 ~ "***", #P<0.001就显示回车加三个星号
  between(P.value, 2.63e-5, 0.001) ~ "**", #P为0.001-0.01 显示回车加两个*号
  between(P.value, 0.001, 0.05) ~ "*", #P为0.01-0.05 显示回车加一个星号
  T ~ ""
))

p2<-ggplot(D_2,aes(x=beta2_B,y=variable,fill= Z_score)) #热图绘制
p2 <- p2+geom_raster()+scale_fill_gradient2(low="#ff0000", high="#0000ff", mid="white")+geom_text(aes(label=D_2$label),col ="black",size = 5)

p2 <- p2 + theme_minimal() + # 不要背景
  theme(text=element_text(size=16, family="serif"),
    axis.title.x=element_blank(), # 去掉 title
        axis.ticks.x=element_blank(), # 去掉x 轴
        axis.title.y=element_blank(), # 去掉 y 轴
        axis.text.x = element_text(angle = 45), # 调整x轴文字，字体加粗
        axis.text.y = element_text()) + #调整y轴文字
  labs(fill =paste0(" * 0.001 < p < 0.05","\n","** 2.63e-5 < p < 0.001","\n","*** p < 2.63e-5"))    # 修改 legend 内容


###导出图片
ggsave(p1,filename = "no1.pdf",width = 6,height = 7)
ggsave(p2,filename = "no2.pdf",width = 6,height = 7)



group <- colnames(A) %>% as.data.frame() %>% 
  mutate(group=c(rep("A",3),rep("B",3))) %>%
  mutate(p="Group") %>%
  ggplot(aes(.,y=p,fill=group))+
  geom_tile() + 
  scale_y_discrete(position="right") +
  theme_minimal()+xlab(NULL) + ylab(NULL) +
  theme(axis.text.x = element_blank())+
  labs(fill = "Group")

#画热图并将以上信息添加进去
p2<-ggplot(C,aes(x=B,y=variable,fill=value)) #热图绘制
p2 <- p2+geom_raster()+scale_fill_gradient2(low="#ff0000", high="#0000ff", mid="yellow",midpoint = 0.05)+
  geom_tile()+theme_minimal()+
  theme(axis.text.x =element_text(angle =90,hjust =0.5,vjust = 0.5))+
  xlab(NULL) + ylab(NULL)
p2 %>%
  insert_top(group, height = .05)
