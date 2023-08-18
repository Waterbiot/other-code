#forestploter包
library(forestploter)
#将列的NA改为空白
dt$Exposures <- ifelse(is.na(dt$Exposures),"",dt$Exposures)
dt$Methods <- ifelse(is.na(dt$Methods),"",dt$Methods)
dt$`OR(96%CI)` <- ifelse(is.na(dt$`OR(96%CI)`),"",dt$`OR(96%CI)`)
dt$`P value` <- ifelse(is.na(dt$`P value`),"",dt$`P value`)

#增加空白列
dt$'' <- paste(rep("",36),collapse = "  ")

#读取excel数据
library(readxl)
dt <- read_excel("forest.xlsx")

#更改主题
tm <- forest_theme(ci_col="#762a83",ci_lty=1,ci_lwd=1.5,refline_lwd = 1,refline_col = "grey",ci_Theight = 0.2)

#绘图
forest1_plot <- forest(dt[,c(1,2,11,4,5)],est=dt$or,lower=dt$or_lci95,upper=dt$or_uci95,ci_column = 3,theme=tm)