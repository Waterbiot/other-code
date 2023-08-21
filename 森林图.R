library(readxl)
library(forestploter)
library(grid)

dt <- read_excel("data.xlsx")
dt <- read.csv(file = "data111.csv",header = TRUE,fileEncoding = "GBK")#导入数据
dt$'' <- paste(rep("",20),collapse = "  ") #设置空列
dt$"β(95%CI)" <- ifelse(is.na(dt$beta),"",
                       sprintf("%.2f(%.2f to %.2f)",
                       dt$beta,dt$down_95ci,dt$up_95ci))
#将列的NA改为空白
dt$"Exposure/Outcome" <- ifelse(is.na(dt$"Exposure/Outcome"),"",dt$"Exposure/Outcome")
dt$Heterogeneity.Test.P <- ifelse(is.na(dt$Heterogeneity.Test.P),"",dt$Heterogeneity.Test.P)
dt$`OR(95%CI)` <- ifelse(is.na(dt$`OR(95%CI)`),"",dt$`OR(95%CI)`)
dt$MR.Egger.Intercept.P <- ifelse(is.na(dt$MR.Egger.Intercept.P),"",dt$MR.Egger.Intercept.P)
dt[is.na(dt)] <- " "

#更改主题
tm <- forest_theme(base_size = 10,#基础大小
                   #编辑置信区间的细节
                   ci_pch = 20, #点的形状
                   ci_col = "#4575b4",#颜色
                   ci_lty=1,#线形状
                   ci_lwd = 2.3,#线宽度
                   
                   #参考线宽度等
                   refline_lwd = 1.5,
                   refline_lty = "dashed",#线形状
                   refline_col = "red",
                   
                   #汇总菱形的填充色和边框色
                   summary_col = "#4575b4",
                   summary_fill = "#4575b4",
                   
                   #脚注大小字体等
                   footnote_cex = 1.1,
                   footnote_fontface = "italic", #脚注字体
                   footnote_col = "blue"
                   )
#绘制
p <- forest(dt[,c(1:3,8,12:13,9:11)],#对数据进行排序
            est = dt$beta, #HR
            lower = dt$down_95ci,
            upper = dt$up_95ci,
            sizes = 0.6,#点估计框大小
            ci_column = 5,#置信区间在第几列展示
            ref_line = 1,#参考线在多少
            xlim = c(-3,3),#x轴范围
            ticks_at = c(-3,-2,-1,0,1,2,3),#x轴标点
            arrow_lab = c('protective factor','risk factor'),
            footnote = 'P<0.05 was considered statistically significant',
            theme = tm
            )
print(p)
png(filename = "aaa.tif",width = 4500,height = 2800,res = 350)
print(p)