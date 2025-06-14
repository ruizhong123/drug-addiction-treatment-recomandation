## 輸入資料
# age : 年齡
# beck : 貝克憂鬱量表
# hercoc : 是否服用海洛因與古柯鹼
# ivhx : 靜脈注射 (排序型變數)
# ndrug : 濫用藥物次數
# race 參與著人種: 黑人 、白人 
# treat : 治療時長
# site : 臨床實驗地點
# txtime : 持續治療時間 (病患對於此治療方案的接受程度)
# time : 藥物濫用復發時間
# status : 設限




View(survUIS)
 
## 評估變數比例

summary(survUIS)

barplot(table(survUIS$hercoc),names.arg  = c('遺失值','使用海洛因及古柯鹼','只有海洛因','只有古柯鹼','其他'))
barplot(table(survUIS$ivhx),names.arg  = c('遺失值','從未使用注射毒品','以前有過注射毒品','最近有注射毒品' ))
barplot(table(survUIS$race),names.arg  = c('遺失值','白人','非白人' ))
barplot(table(survUIS$treat),names.arg  = c('短時間','長時間'))
barplot(table(survUIS$site),names.arg  = c('A地區','B地區'))
barplot(table(survUIS$status),names.arg  = c('設限','藥物濫用復發'))





# 轉成數字
survUIS$age = as.numeric(survUIS$age)
survUIS$beck = as.numeric(survUIS$beck)
survUIS$ndrug = as.numeric(survUIS$ndrug)
survUIS$txtime = as.numeric(survUIS$txtime)
survUIS$time = as.numeric(survUIS$time)

# 轉成因子
survUIS$hercoc = as.factor(survUIS$hercoc)
survUIS$ivhx = as.factor(survUIS$ivhx)
survUIS$race = as.factor(survUIS$race)
survUIS$treat = as.factor(survUIS$treat)
survUIS$site = as.factor(survUIS$site)
survUIS$status = as.factor(survUIS$status)

summary(survUIS)


# 找出遺失值在各變數之中

survUIS[is.na(survUIS$age),]
survUIS[is.na(survUIS$beck),]
survUIS[is.na(survUIS$age),]
survUIS[survUIS$hercoc == ".",]
survUIS[survUIS$ivhx == ".",]
survUIS[is.na(survUIS$ndrug),]
survUIS[survUIS$race==".",]


sum(is.na(survUIS$age) )/nrow(survUIS)
sum(is.na(survUIS$beck))/nrow(survUIS)
sum(is.na(survUIS$age))/nrow(survUIS)

sum(survUIS$hercoc == ".")/nrow(survUIS)
sum(survUIS$ivhx == ".")/nrow(survUIS)
sum(is.na(survUIS$ndrug))/nrow(survUIS)
sum(survUIS$race==".")/nrow(survUIS)

# 遺失總比例
sum(is.na(survUIS))/nrow(survUIS)


# 資料分佈 

plot(density(survUIS$age, na.rm = TRUE), 
     main = "Density Plot of Age",
     xlab = "Age",
     ylab = "Density",
     col = "blue")


# Add a red vertical line at the mean age
abline(v = 30, col = "red", lwd = 2, lty = 2)  # lwd = line width, lty = line type (dashed)


plot(density(survUIS$time, na.rm = TRUE), 
     main = "Density Plot of Age",
     xlab = "Age",
     ylab = "Density",
     col = "blue")

library(dplyr)
# 計算整體治療的完成進度

survUIS <- survUIS %>%
  mutate(治療方案 = case_when(
    site == 0 & treat == 0 ~ "A 地區短時間治療",
    site == 0 & treat == 1 ~ "A 地區長時間治療",
    site == 1 & treat == 0 ~ "B 地區短時間治療",
    site == 1 & treat == 1 ~ "B 地區長時間治療",
    TRUE ~ NA_character_  # Default for unexpected values or NA
  ))

View(survUIS)

# 繪製箱型圖
p <- ggplot(survUIS, aes(x = 治療方案, y = txtime, fill = 治療方案)) +
  geom_boxplot(width = 0.5) +
  labs(title = "Boxplot of Treatment Time by Treatment Plan with Proportion of Y94",
       x = "Treatment Plan (治療方案)",
       y = "Treatment Time (txtime)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 添加比例標籤
p + geom_text(data = prop_data, 
              aes(x = 治療方案, y = max(survUIS$txtime, na.rm = TRUE) , 
                  label = sprintf("%.2f", proportion)),
              vjust = 30, size = 4, color = "black")

library(tidyr)


prop_data <- survUIS %>%
  group_by(治療方案) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

install.packages("openxlsx")
library(openxlsx)
write.xlsx(survUIS, "C:/Users/ruizhong/Desktop/output.xlsx")
nrow(survUIS)
survUIS <- survUIS %>%
  mutate(是否達成治療方案 = case_when(
    治療方案 == "A 地區短時間治療" & txtime >= 90  ~ "完成治療",
    治療方案 == "A 地區長時間治療" & txtime >= 180 ~ "完成治療",
    治療方案 == "B 地區短時間治療" & txtime >= 180 ~ "完成治療",
    治療方案 == "B 地區長時間治療" & txtime >= 360 ~ "完成治療",
    
    治療方案 == "A 地區短時間治療" & txtime < 90  ~ "未完成治療",
    治療方案 == "A 地區長時間治療" & txtime < 180 ~ "未完成治療",
    治療方案 == "B 地區短時間治療" & txtime < 180 ~ "未完成治療",
    治療方案 == "B 地區長時間治療" & txtime < 360 ~ "未完成治療",
    
    
    TRUE ~ NA_character_  # Default for unexpected values or NA
  ))








## remove the missing value create survival analysis 

km_fit <- survfit(Surv(time, status) ~ , data = survUIS)




# processing the data 

library(dplyr)

# Create beck_level

# transfor data into

survUIS_rm_missing <- na.omit(survUIS)

survUIS$status = as.numeric(survUIS$status) - 1
survUIS$status = as.factor(survUIS$status)


survUIS_rm_missing <- survUIS_rm_missing %>%
  mutate(beck_level = case_when(
    beck <= 13 ~ "正常範圍",
    beck > 13 & beck <= 19 ~ "輕度憂鬱",
    beck > 19 & beck <= 28 ~ "中度憂鬱",
    beck > 28 ~ "重度憂鬱"                # Assumed for beck > 28
  ))


# Convert to factor for ordered levels

survUIS_rm_missing$beck_level <- factor(
  survUIS_rm_missing$beck_level,
  levels = c("正常範圍", "輕度憂鬱", "中度憂鬱", "重度憂鬱"),
  ordered = TRUE
)

# 先檢測 整體存活機率 ，在檢測個特徵之間的存活機率差異


# KM curve by beck_level

library(dplyr)
library(survival)
library(survminer)
library(ggplot2)

install.packages("ggsurvfit")

library(ggsurvfit)
View(survUIS_rm_missing)


survUIS_rm_missing$status= as.numeric(survUIS_rm_missing$status)

survUIS_rm_missing$地區 = ifelse(survUIS_rm_missing$site==0,"SiteA","SiteB")
survUIS_rm_missing$治療時間 = ifelse(survUIS_rm_missing$treat==1,"long time","short time")

survfit2(Surv(time, status) ~ 地區+治療時間, data = survUIS_rm_missing) %>%
    # Adds the survival curves to the plot
  ggsurvfit() +
  # Adds confidence intervals to the survival curves
  add_confidence_interval() 


survdiff(formula = Surv(time,status)~治療方案,data = survUIS)



# 年齡
install.packages("gridExtra")
library(gridExtra)
library(grid)
library(dplyr)
library(survival)
library(ggsurvfit)
View(survUIS)

survUIS$年齡族群= ifelse(survUIS$age > 30 ,"older group","young group")

young = survUIS[survUIS$年齡族群 =="young group",]
older = survUIS[survUIS$年齡族群 =="older group",]


nrow(young)
nrow(older)

g1 = 
  survfit2(Surv(time, status) ~ 治療方案, data = young) %>%
  # Adds the survival curves to the plot
  ggsurvfit() +
  # Adds confidence intervals to the survival curves
  add_confidence_interval() +
  labs(title = " 年輕族群 ")  # Correct way to add title


g2=survfit2(Surv(time, status) ~ 治療方案, data = older) %>%
  # Adds the survival curves to the plot
  ggsurvfit() +
  # Adds confidence intervals to the survival curves
  add_confidence_interval() +
  labs(title = " 中年族群")  # Correct way to add title


grid.arrange(g1, g2,ncol=2)


survdiff(formula = Surv(time,status)~治療方案,data =young)
survdiff(formula = Surv(time,status)~治療方案,data = older)
View(survUIS)

survUIS <- na.omit(survUIS)

排除A地區短時間 <- survUIS[survUIS$治療方案 != "A 地區短時間治療",]

年輕族群 <-排除A地區短時間[排除A地區短時間$年齡 == "年輕族群" ,]
View(年輕族群)

survdiff(formula = Surv(time,status)~治療方案,data = 年輕族群)

# 憂鬱程度

正常範圍 = survUIS_rm_missing[survUIS_rm_missing$beck_level =="正常範圍",]
輕度憂鬱 = survUIS_rm_missing[survUIS_rm_missing$beck_level =="輕度憂鬱",]
中度憂鬱 = survUIS_rm_missing[survUIS_rm_missing$beck_level =="中度憂鬱",]
重度憂鬱 = survUIS_rm_missing[survUIS_rm_missing$beck_level =="重度憂鬱",]

View(輕度憂鬱)

g1 = 
  survfit2(Surv(time, status) ~ 地區+治療時間, data = 正常範圍) %>%
  # Adds the survival curves to the plot
  ggsurvfit() +
  # Adds confidence intervals to the survival curves
  add_confidence_interval() +
  labs(title = "正常範圍")  # Correct way to add title


g2 = 
  survfit2(Surv(time, status) ~ 地區+治療時間, data = 輕度憂鬱) %>%
  # Adds the survival curves to the plot
  ggsurvfit() +
  # Adds confidence intervals to the survival curves
  add_confidence_interval() +
  labs(title = "輕度憂鬱")  # Correct way to add title

g3 = 
  survfit2(Surv(time, status) ~ 地區+治療時間, data = 中度憂鬱) %>%
  # Adds the survival curves to the plot
  ggsurvfit() +
  # Adds confidence intervals to the survival curves
  add_confidence_interval() +
  labs(title = "中度憂鬱")  # Correct way to add title


g4=survfit2(Surv(time, status) ~ 地區+治療時間, data = 重度憂鬱) %>%
  # Adds the survival curves to the plot
  ggsurvfit() +
  # Adds confidence intervals to the survival curves
  add_confidence_interval() +
  labs(title = "重度憂鬱")  # Correct way to add title



grid.arrange(g1, g2,g3,g4,ncol=2)

survdiff(formula = Surv(time,status)~site+treat,data = 正常範圍)
survdiff(formula = Surv(time,status)~site+treat,data = 輕度憂鬱)
survdiff(formula = Surv(time,status)~site+treat,data = 中度憂鬱)
survdiff(formula = Surv(time,status)~site+treat,data = 重度憂鬱)


# 靜脈注射


從未使用靜脈注射 = survUIS_rm_missing[survUIS_rm_missing$ivhx == "1",]
以前有使用過靜脈注射 = survUIS_rm_missing[survUIS_rm_missing$ivhx =="2",]
最近有使用過靜脈注射 = survUIS_rm_missing[survUIS_rm_missing$ivhx =="3",]


View(輕度憂鬱)

g1 = 
  survfit2(Surv(time, status) ~ 地區+治療時間, data = 從未使用過靜脈注射) %>%
  # Adds the survival curves to the plot
  ggsurvfit() +
  # Adds confidence intervals to the survival curves
  add_confidence_interval() +
  labs(title = "從未使用過靜脈注射")  # Correct way to add title


g2 = 
  survfit2(Surv(time, status) ~ 地區+治療時間, data = 以前有使用過靜脈注射) %>%
  # Adds the survival curves to the plot
  ggsurvfit() +
  # Adds confidence intervals to the survival curves
  add_confidence_interval() +
  labs(title = "以前有使用過靜脈注射")  # Correct way to add title

g3 = 
  survfit2(Surv(time, status) ~ 地區+治療時間, data = 最近有使用過靜脈注射) %>%
  # Adds the survival curves to the plot
  ggsurvfit() +
  # Adds confidence intervals to the survival curves
  add_confidence_interval() +
  labs(title = "最近有使用過靜脈注射")  # Correct way to add title





grid.arrange(g1, g2,g3,ncol=2)

survdiff(formula = Surv(time,status)~site+treat,data =從未使用靜脈注射 )
survdiff(formula = Surv(time,status)~site+treat,data =以前有使用過靜脈注射 )
survdiff(formula = Surv(time,status)~site+treat,data = 最近有使用過靜脈注射 )


# ndrug
View(survUIS)

rm_na_omit = na.omit(survUIS)

survUIS$ndrug = as.numeric(survUIS$ndrug)

plot(density(rm_na_omit$ndrug, na.rm = TRUE), 
     main = "Density Plot of Age",
     xlab = "Age",
     ylab = "Density",
     col = "blue")

abline(v = 1, col = "red", lwd = 2, lty = 2)  # lwd = line width, lty = line type (dashed)

nrow(survUIS[survUIS$ndrug <= 1,])/nrow(survUIS)


max(na.omit(survUIS$ndrug))

plot(time~ndrug,data=rm_na_omit)
cor(rm_na_omit$ndrug,rm_na_omit$time)


# race

白人 = survUIS_rm_missing[survUIS_rm_missing$race == "0",]
黑人 = survUIS_rm_missing[survUIS_rm_missing$race =="1",]

g1 = 
  survfit2(Surv(time, status) ~ 地區+治療時間, data = 白人) %>%
  # Adds the survival curves to the plot
  ggsurvfit() +
  # Adds confidence intervals to the survival curves
  add_confidence_interval() +
  labs(title = "白人")  # Correct way to add title


g2 = 
  survfit2(Surv(time, status) ~ 地區+治療時間, data = 以前有使用過靜脈注射) %>%
  # Adds the survival curves to the plot
  ggsurvfit() +
  # Adds confidence intervals to the survival curves
  add_confidence_interval() +
  labs(title = "非白人")  # Correct way to add title



grid.arrange(g1, g2,ncol=2)

survdiff(formula = Surv(time,status)~site+treat,data = 黑人)

survdiff(formula = Surv(time,status)~site+treat,data = 白人)



# ndrug
library(dplyr)
library(survival)
library(survminer)
library(ggsurvfit)
library(ggplot2)
library(gridExtra)
library(grid)


住院前無藥物治療 = survUIS[survUIS$ndrug == 0,]
住院前有藥物治療 = survUIS[survUIS$ndrug != 0,]

g1 = 
  survfit2(Surv(time, status) ~ 治療方案, data = 住院前無藥物治療) %>%
  # Adds the survival curves to the plot
  ggsurvfit() +
  # Adds confidence intervals to the survival curves
  add_confidence_interval() +
  labs(title = "住院前無藥物治療")  # Correct way to add title


g2 = 
  survfit2(Surv(time, status) ~ 治療方案, data = 住院前有藥物治療) %>%
  # Adds the survival curves to the plot
  ggsurvfit() +
  # Adds confidence intervals to the survival curves
  add_confidence_interval() +
  labs(title = "住院前有藥物治療")  # Correct way to add title



grid.arrange(g1, g2,ncol=2)

survdiff(formula = Surv(time,status)~site+treat,data = 黑人)

survdiff(formula = Surv(time,status)~site+treat,data = 白人)












## Transcational analysis

install.packages("arules")

library(arulesViz)
library(dplyr)

survUIS <- survUIS %>%
  mutate(治療方案 = case_when(
    site == 0 & treat == 0 ~ "A 地區短時間治療",
    site == 0 & treat == 1 ~ "A 地區長時間治療",
    site == 1 & treat == 0 ~ "B 地區短時間治療",
    site == 1 & treat == 1 ~ "B 地區長時間治療",
    TRUE ~ NA_character_  # Default for unexpected values or NA
  ))


survUIS <- survUIS %>%
  mutate(beck_level = case_when(
    beck <= 13 ~ "正常範圍",
    beck > 13 & beck <= 19 ~ "輕度憂鬱",
    beck > 19 & beck <= 28 ~ "中度憂鬱",
    beck > 28 ~ "重度憂鬱"                # Assumed for beck > 28
  ))


survUIS <- survUIS %>%
  mutate( 海洛因以及古柯鹼 = case_when(
    hercoc == 1 ~ "海洛因及古柯鹼",
    hercoc == 2 ~ "海洛因",
    hercoc == 3 ~ "古柯鹼",
    hercoc == 4 ~ "無"
    ))

survUIS <- survUIS %>%
  mutate( 靜脈注射史 = case_when(
    ivhx == 1 ~ "從未使用注射毒品",
    ivhx == 2 ~ "以前注射過毒品",
    ivhx == 3 ~ "最近有注射毒品"
  ))


survUIS <- survUIS %>%
  mutate( 人種 = case_when(
    race == 0 ~ "白人",
    race == 1 ~ "黑人"
  ))


library(dplyr)
library(survival)
survUIS <- survUIS %>%
  mutate(drug_status = ifelse(ndrug == 0, "從未使用藥物治療", "使用藥物治療"))

從未使用藥物治療 = survUIS[survUIS$drug_status == "從未使用藥物治療",]
使用藥物治療 = survUIS[survUIS$drug_status == "使用藥物治療",]




survdiff(formula = Surv(time,status)~site+treat,data = 從未使用藥物治療)
survdiff(formula = Surv(time,status)~site+treat,data = 使用藥物治療)

survUIS <- survUIS %>%
  mutate(年齡 = ifelse(age <= 30, "年輕族群", "中老年族群"))









censor = survUIS[survUIS$status==0,]
library(arulesViz)
View(censor)

censor <- data.frame(censor$beck_level,censor$治療方案,censor$海洛因以及古柯鹼,censor$靜脈注射史,censor$人種,censor$drug_status)

View(censor)


censor1 <- censor[censor$censor.治療方案=="A 地區短時間治療",]
censor1<- na.omit(censor1)
censor1

censor2 <-censor[censor$censor.治療方案 =="A 地區長時間治療",]
censor2 <-na.omit(censor2)
censor2

censor3 <- censor[censor$censor.治療方案=="B 地區短時間治療",]
censor3 <-na.omit(censor3)
censor3

censor4 <-censor[censor$censor.治療方案=="B 地區長時間治療",]
censor4 <-na.omit(censor4)
censor4


censor1 <- as(censor1,"transactions")
censor1
rule1 <- apriori(censor1,parameter = list(supp=0.4,conf=0.5,target="rules"))
plot(rule1,method="graph")

censor2 <- as(censor2,"transactions")
rule2 <- apriori(censor2,parameter = list(supp=0.4,conf=0.5,target="rules"))
plot(rule2,method="graph")

censor3 <- as(censor3,"transactions")
rule3 <- apriori(censor3,parameter = list(supp=0.4,conf=0.5,target="rules"))
plot(rule3,method="graph")

censor4 <- as(censor4,"transactions")
rule4 <- apriori(censor4,parameter = list(supp=0.4,conf=0.5,target="rules"))
plot(rule4,method="graph")


## 使用分類樹探討發生右設限的主要原因

library(dplyr)
library(ggplot2)
library(hrbrthemes)

survUIS$status <- as.factor(survUIS$status)

survUIS%>%
  ggplot( aes(x=time,fill=status)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080"),
                    labels = c("以復發的病患", "未復發的藥癮著(治療效果最佳)")) +
  theme_ipsum() +
  labs(fill="")


survUIS_rm_missing <- na.omit(survUIS)

View(survUIS_rm_missing)

########## imputate value ###########

# analysis variable

# missing propotion 
# age : 0.007961783

sum(is.na(survUIS$age) )/nrow(survUIS) # remove 
sum(is.na(survUIS$beck))/nrow(survUIS) # need imputation

sum(is.na(survUIS$hercoc))/nrow(survUIS) # need imputation 
sum(is.na(survUIS$ivhx))/nrow(survUIS) # need imputation 
sum(is.na(survUIS$ndrug))/nrow(survUIS)# need imputation 

sum(is.na(survUIS$race))/nrow(survUIS)# remove 


cor(survUIS_rm_missing[, c("age", "beck", "ndrug","time","txtime")])


library(dplyr)

survUIS$海洛因以及古柯鹼



survUIS <- survUIS %>%
  mutate(海洛因或古柯鹼 = case_when(
    海洛因以及古柯鹼 == '無' ~ 0,
    TRUE ~ 1  # 其他情況設為 1
  ))


View(occur)

occur= survUIS[survUIS$status==1,]
boxplot(time~治療方案+海洛因以及古柯鹼 ,data = occur)

## relational transcational analysis 

library(openxlsx)
survUIS <- na.omit(survUIS)
censor <- survUIS[survUIS$status==0,]
occur <- survUIS[survUIS$status==1,]
write.xlsx(censor,"C:\\存活分析報告\\新增資料夾\\censor.xlsx")


censor <- censor %>%
  mutate(治療方案 = case_when(
    site == 0 & treat == 0 ~ "A 地區短時間治療",
    site == 0 & treat == 1 ~ "A 地區長時間治療",
    site == 1 & treat == 0 ~ "B 地區短時間治療",
    site == 1 & treat == 1 ~ "B 地區長時間治療",
    TRUE ~ NA_character_  # Default for unexpected values or NA
  ))




nrow(censor[censor$治療方案=="A 地區短時間治療",])
nrow(censor[censor$治療方案=="A 地區長時間治療",])
nrow(censor[censor$治療方案=="B 地區短時間治療",])
nrow(censor[censor$治療方案=="B 地區長時間治療",])

barplot(table(censor$治療方案),col='lightblue')

A地區時間短治療方案=censor[censor$治療方案=="A 地區短時間治療",]
A地區時間長治療方案=censor[censor$治療方案=="A 地區長時間治療",]
B地區時間短治療方案=censor[censor$治療方案=="B 地區短時間治療",]
B地區時間長治療方案=censor[censor$治療方案=="B 地區長時間治療",]








## aruvisualization 
View(A地區時間短治療方案)
View(censor)

A地區時間短治療方案$海洛因以及古柯鹼
library(arulesViz)
transcation = A地區時間短治療方案[,c('海洛因或古柯鹼','人種')]

transcation<- as.data.frame(transcation)

transcation <- lapply(transcation, as.factor)

View(transcation)
censor_A_short <- as(transcation,'transactions')

rule1 <- apriori(censor_A_short,parameter = list(supp=0.5,conf=0.6,target="rules"))
plot(rule1,method="graph")



library(arulesViz)
transcation = A地區時間長治療方案[,c('年齡','海洛因以及古柯鹼','靜脈注射史','人種','beck_level','drug_status')]

transcation<- as.data.frame(transcation)
transcation[, 1:6] <- lapply(transcation[, 1:6], as.factor)

View(transcation)
censor_A_long <- as(transcation,'transactions')

rule1 <- apriori(censor_A_short,parameter = list(supp=0.5,conf=0.5,target="rules"))
plot(rule1,method="graph")


library(arulesViz)
transcation = B地區時間長治療方案[,c('年齡','海洛因以及古柯鹼','靜脈注射史','人種','beck_level','drug_status')]

transcation<- as.data.frame(transcation)
transcation[, 1:6] <- lapply(transcation[, 1:6], as.factor)

View(transcation)
censor_A_long <- as(transcation,'transactions')

rule1 <- apriori(censor_A_long,parameter = list(supp=0.4,conf=0.5,target="rules"))
plot(rule1,method="graph")



View(A地區時間長治療方案)



海洛因及古柯鹼 = A地區時間長治療方案[A地區時間長治療方案$海洛因以及古柯鹼 == "海洛因及古柯鹼",]

barplot(table(海洛因及古柯鹼$人種))

海洛因 = A地區時間長治療方案[A地區時間長治療方案$海洛因以及古柯鹼 == "海洛因",]
barplot(table(海洛因$人種))

古柯鹼 = A地區時間長治療方案[A地區時間長治療方案$海洛因以及古柯鹼 == "古柯鹼",]
barplot(table(古柯鹼$人種))



海洛因及古柯鹼 = A地區時間短治療方案[A地區時間短治療方案$海洛因以及古柯鹼 == "海洛因及古柯鹼",]

barplot(table(海洛因及古柯鹼$人種))

海洛因 = A地區時間短治療方案[A地區時間短治療方案$海洛因以及古柯鹼 == "海洛因",]
barplot(table(海洛因$人種))

古柯鹼 = A地區時間短治療方案[A地區時間短治療方案$海洛因以及古柯鹼 == "古柯鹼",]
barplot(table(古柯鹼$人種))

############# cox ph ######################

library(survival)


View(survUIS)

survUIS$age = as.numeric(survUIS$age)
survUIS$beck = as.numeric(survUIS$beck)
survUIS$ndrug = as.numeric(survUIS$ndrug)
survUIS$txtime = as.numeric(survUIS$txtime)
survUIS$time = as.numeric(survUIS$time)
survUIS$hercoc = as.numeric(survUIS$hercoc)
survUIS$ivhx = as.numeric(survUIS$ivhx)
survUIS$race = as.numeric(survUIS$race)
survUIS$treat = as.numeric(survUIS$treat)
survUIS$site = as.numeric(survUIS$site)
survUIS$status = as.numeric(survUIS$status)

survUIS$hercoc = as.factor(survUIS$hercoc)
survUIS$ivhx = as.factor(survUIS$ivhx)
survUIS$race = as.factor(survUIS$race)
survUIS$treat = as.factor(survUIS$treat)
survUIS$site = as.factor(survUIS$site)
survUIS$status = as.numeric(survUIS$status)

View(survUIS)
rm_na_data = na.omit(survUIS)




### 治療方案 V.S. 

View(survUIS)

# age 、beck 、hercoc、ivhx、ndrug、race、

##治療方案 及 age
library(survival)

fit1 <-  coxph(Surv(time,status)~factor(治療方案),data = survUIS)
fit2 <-  coxph(Surv(time,status)~factor(治療方案)+age,data = survUIS) # significant 0.04031
anova(fit1,fit2)


###################
fit3 <-  coxph(Surv(time,status)~factor(治療方案),data = survUIS)
fit4 <-  coxph(Surv(time,status)~factor(治療方案)+beck,data = survUIS) # significant 0.0412

anova(fit3,fit4)

##############
fit5 <-  coxph(Surv(time,status)~factor(治療方案),data = survUIS)
fit6 <-  coxph(Surv(time,status)~factor(治療方案)+factor(hercoc),data = survUIS)# 0.08558

anova(fit5,fit6)

########################
fit7 <-  coxph(Surv(time,status)~factor(治療方案),data = survUIS)
fit8 <-  coxph(Surv(time,status)~factor(治療方案)+factor(ivhx),data = survUIS) #significant 0.00318
anova(fit7,fit8)


#####
fit9 <-  coxph(Surv(time,status)~factor(治療方案),data = survUIS)
fit10 <-  coxph(Surv(time,status)~factor(治療方案)+ndrug,data = survUIS) #significant 0.0003469
anova(fit9,fit10)

fit11 <-  coxph(Surv(time,status)~factor(治療方案),data = survUIS)
fit12 <-  coxph(Surv(time,status)~factor(治療方案)+factor(race),data = survUIS) # significant 0.003369
anova(fit11,fit12)




### 已知 race 與 治療方案有交互作用 ，列入考量作用
## ndrug > ivhx > race > age > beck

fit13 <-  coxph(Surv(time,status)~factor(治療方案)+ndrug,data = survUIS)
fit14 <-  coxph(Surv(time,status)~factor(治療方案)+ndrug+factor(ivhx),data = survUIS) 
anova(fit13,fit14)


fit15 <-  coxph(Surv(time,status)~factor(治療方案)+ndrug,data = survUIS)
fit16 <-  coxph(Surv(time,status)~factor(治療方案)+ndrug+factor(race),data = survUIS) # significant
anova(fit15,fit16)

fit17 <-  coxph(Surv(time,status)~factor(治療方案)+ndrug,data = survUIS)
fit18 <-  coxph(Surv(time,status)~factor(治療方案)+ndrug+age,data = survUIS) # significant
anova(fit17,fit18)

fit19 <-  coxph(Surv(time,status)~factor(治療方案)+ndrug,data = survUIS)
fit20 <-  coxph(Surv(time,status)~factor(治療方案)+ndrug+beck,data = survUIS) # significant
anova(fit19,fit20)

# age > race > beck

fit21 <-  coxph(Surv(time,status)~factor(治療方案)+ndrug+age,data = survUIS)
fit22 <-  coxph(Surv(time,status)~factor(治療方案)+ndrug+age+factor(race),data = survUIS) # significant
anova(fit21,fit22)

fit23 <-  coxph(Surv(time,status)~factor(治療方案)+ndrug+age,data = survUIS)
fit24 <-  coxph(Surv(time,status)~factor(治療方案)+ndrug+age+beck,data = survUIS) # significant
anova(fit23,fit24)


fit25 <-  coxph(Surv(time,status)~factor(治療方案)+ndrug+age,data = survUIS)
fit26 <-  coxph(Surv(time,status)~factor(治療方案)+ndrug+age+factor(race),data = survUIS) # significant
anova(fit25,fit26)


# interaction 
fit27 <-  coxph(Surv(time,status)~factor(治療方案)+ndrug+age+factor(race),data = survUIS)
fit28 <-  coxph(Surv(time,status)~factor(治療方案)+factor(治療方案)*ndrug+ndrug+age+factor(race),data = survUIS) 
anova(fit27,fit28)


fit29 <-  coxph(Surv(time,status)~factor(治療方案)+ndrug+age+factor(race),data = survUIS)
fit30 <-  coxph(Surv(time,status)~factor(治療方案)+factor(治療方案)*factor(race)+ndrug+age+factor(race),data = survUIS) # significant
anova(fit29,fit30)

summary(fit30)


fit31 <-  coxph(Surv(time,status)~factor(治療方案)+ndrug+age+factor(race),data = survUIS)
fit32 <-  coxph(Surv(time,status)~factor(治療方案)+factor(治療方案)*age+ndrug+age+factor(race),data = survUIS) # significant
anova(fit31,fit32)


fit33 <-  coxph(Surv(time,status)~factor(治療方案)+ndrug+age+factor(race),data = survUIS)
fit34 <-  coxph(Surv(time,status)~factor(治療方案)+ndrug*age+ndrug+age+factor(race),data = survUIS) # significant
anova(fit33,fit34)



fit35 <-  coxph(Surv(time,status)~factor(治療方案)+ndrug+age+factor(race),data = survUIS)
fit36 <-  coxph(Surv(time,status)~factor(治療方案)+ndrug*factor(race)+ndrug+age+factor(race),data = survUIS) # significant
anova(fit35,fit36)




##

fit37 <-  coxph(Surv(time,status)~factor(治療方案)+ndrug+age+factor(race),data = survUIS)
fit38 <-  coxph(Surv(time,status)~factor(治療方案)+age*factor(race)+ndrug+age+factor(race),data = survUIS) # significant
anova(fit37,fit38)



####

survUIS$治療方案= factor(survUIS$治療方案,levels=c('A 地區短時間治療','A 地區長時間治療','B 地區短時間治療','B 地區長時間治療'))

survUIS
fit30 <-  coxph(Surv(time,status)~factor(治療方案)+factor(治療方案)*factor(race)+ndrug+age+factor(race),data = survUIS) # significant

surv_prob <- survfit(Surv(time,status) ~ -1,data=survUIS)
fit_surv <- survfit(fit30)



# Fit Cox proportional hazards model
library(dplyr)

imputate_survUIS <- imputate_survUIS %>%
  mutate(治療方案 = case_when(
    site == 0 & treat == 0 ~ "A 地區短時間治療",
    site == 0 & treat == 1 ~ "A 地區長時間治療",
    site == 1 & treat == 0 ~ "B 地區短時間治療",
    site == 1 & treat == 1 ~ "B 地區長時間治療",
    TRUE ~ NA_character_  # Default for unexpected values or NA
  ))


imputate_survUIS = survUIS_imputed_median_mode


library(survival)

fit <- coxph(Surv(time, status) ~ factor(治療方案) + factor(治療方案)*factor(race) + ndrug + age, 
               data = survUIS)
IMPUT_fit <- coxph(Surv(time, status) ~ factor(治療方案) + factor(治療方案)*factor(race) + ndrug + age, 
             data = imputate_survUIS)

summary(fit30)
summary(IMPUT_fit)
# Create Kaplan-Meier survival curves stratified by 治療方案

actual_surv_prob <- survfit(Surv(time, status) ~ -1, data = survUIS)

pred_fit_surv <- survfit(fit30,data = survUIS)
pred_fit_surv


## fit_surv 、surv_prob

surv_list <- list(Actual = actual_surv_prob, prediaction = pred_fit_surv)

surv_list

# Combine the plots
ggsurvplot_combine(
  surv_list,
  title = " 預估跟預測結果",
  legend.labs = c(" 實際 ", " 預估 "), # Customize labels
  palette = c("#E41A1C", "#377EB8"), # Distinct colors for each curve
  conf.int = TRUE
)






cox.zph(fit30)
summary(fit30)



########################################################
hercocandcocc = survUIS[survUIS$hercoc=='1',]


hercocandcoccA = hercocandcocc[hercocandcocc$site=='0',]
hercocandcoccB = hercocandcocc[hercocandcocc$site=='1',]

hercocandcoccB

shapiro.test(hercocandcoccA$txtime)
shapiro.test(hercocandcoccB$txtime)

boxplot(hercocandcoccA$txtime,hercocandcoccB$txtime,names = c("地區A", "地區B"),xlab='地區',ylab='復發時長')

wilcox.test(hercocandcoccA$txtime, hercocandcoccB$txtime)


###########################################################

View(survUIS)
A地區長時間治療  = survUIS[survUIS$治療方案=="A 地區長時間治療",]
A地區短時間治療  = survUIS[survUIS$治療方案=="A 地區短時間治療",]

B地區長時間治療 = survUIS[survUIS$治療方案 == "B 地區長時間治療",]
B地區短時間治療 = survUIS[survUIS$治療方案 == "B 地區短時間治療",]

A地區長時間治療
blackA_L = A地區長時間治療[A地區長時間治療$race=='1',]
blackA_S = A地區短時間治療[A地區短時間治療$race=='1',]



boxplot(blackA$txtime,blackother$txtime,names = c("地區A", "地區B"),xlab='地區',ylab='復發時長')

wilcox.test(blackA_L$txtime, blackA_S$txtime)


