########################################
########################################
# Porto Seguro’s Safe Driver Prediction
########################################
########################################


########################################
# read data
# biref summary of data
########################################
PS= read.csv("train-1.csv", sep=",", header=T, na.strings=c("","."))
head(PS)
names(PS)
summary(PS)

## Ordinal variable double check
sum(PS$ps_ind_01%%1!=0)
sum(PS$ps_ind_03%%1!=0)
sum(PS$ps_ind_14%%1!=0)
sum(PS$ps_ind_15%%1!=0)
sum(PS$ps_car_11%%1!=0)
sum(PS$ps_calc_04%%1!=0)
sum(PS$ps_calc_05%%1!=0)
sum(PS$ps_calc_06%%1!=0)
sum(PS$ps_calc_07%%1!=0)
sum(PS$ps_calc_08%%1!=0)
sum(PS$ps_calc_09%%1!=0)
sum(PS$ps_calc_10%%1!=0)
sum(PS$ps_calc_11%%1!=0)
sum(PS$ps_calc_12%%1!=0)
sum(PS$ps_calc_13%%1!=0)
sum(PS$ps_calc_14%%1!=0)

## Check missing value
sum(PS == -1) 
# missing value: 846458
sum(PS$ps_ind_02_cat == -1) #216
sum(PS$ps_ind_04_cat == -1) #83
sum(PS$ps_ind_05_cat == -1) #5809
sum(PS$ps_reg_03 == -1)     #107772
sum(PS$ps_car_01_cat == -1) #107
sum(PS$ps_car_02_cat == -1) #5
sum(PS$ps_car_03_cat == -1) #411231
sum(PS$ps_car_05_cat == -1) #266551
sum(PS$ps_car_07_cat == -1) #11489
sum(PS$ps_car_09_cat == -1) #569
sum(PS$ps_car_11 == -1)     #5
sum(PS$ps_car_12 == -1)     #1
sum(PS$ps_car_14 == -1)     #42620

## Check the meaning of target variable
sum(PS$target==0)
# 573518 cases with target value "0"
sum(PS$target==1)
# 21694 cases with target value "1"

########################################
# Preprocessing
# reformat the dataset
# change "cat" and "ord" to its type
########################################
PS$target = factor(PS$target)

PS$ps_ind_01 = ordered(PS$ps_ind_01)

PS$ps_ind_02_cat[PS$ps_ind_02_cat==-1] = NA
PS$ps_ind_02_cat = factor(PS$ps_ind_02_cat)

PS$ps_ind_03 = ordered(PS$ps_ind_03)

PS$ps_ind_04_cat[PS$ps_ind_04_cat==-1] = NA
PS$ps_ind_04_cat = factor(PS$ps_ind_04_cat)

PS$ps_ind_05_cat[PS$ps_ind_05_cat==-1] = NA
PS$ps_ind_05_cat = factor(PS$ps_ind_05_cat)

PS$ps_ind_06_bin = factor(PS$ps_ind_06_bin)
PS$ps_ind_07_bin = factor(PS$ps_ind_07_bin)
PS$ps_ind_08_bin = factor(PS$ps_ind_08_bin)
PS$ps_ind_09_bin = factor(PS$ps_ind_09_bin)
PS$ps_ind_10_bin = factor(PS$ps_ind_10_bin)
PS$ps_ind_11_bin = factor(PS$ps_ind_11_bin)
PS$ps_ind_12_bin = factor(PS$ps_ind_12_bin)
PS$ps_ind_13_bin = factor(PS$ps_ind_13_bin)
PS$ps_ind_14 = ordered(PS$ps_ind_14)
PS$ps_ind_15 = ordered(PS$ps_ind_15)
PS$ps_ind_16_bin = factor(PS$ps_ind_16_bin)
PS$ps_ind_17_bin = factor(PS$ps_ind_17_bin)
PS$ps_ind_18_bin = factor(PS$ps_ind_18_bin)

PS$ps_reg_03[PS$ps_reg_03==-1] = NA

PS$ps_car_01_cat[PS$ps_car_01_cat==-1] = NA
PS$ps_car_01_cat = factor(PS$ps_car_01_cat)

PS$ps_car_02_cat[PS$ps_car_02_cat==-1] = NA
PS$ps_car_02_cat = factor(PS$ps_car_02_cat)

PS$ps_car_03_cat[PS$ps_car_03_cat==-1] = NA
PS$ps_car_03_cat = factor(PS$ps_car_03_cat)

PS$ps_car_04_cat = factor(PS$ps_car_04_cat)

PS$ps_car_05_cat[PS$ps_car_05_cat==-1] = NA
PS$ps_car_05_cat = factor(PS$ps_car_05_cat)

PS$ps_car_06_cat = factor(PS$ps_car_06_cat)

PS$ps_car_07_cat[PS$ps_car_07_cat==-1] = NA
PS$ps_car_07_cat = factor(PS$ps_car_07_cat)

PS$ps_car_08_cat = factor(PS$ps_car_08_cat)

PS$ps_car_09_cat[PS$ps_car_09_cat==-1] = NA
PS$ps_car_09_cat = factor(PS$ps_car_09_cat)

PS$ps_car_10_cat = factor(PS$ps_car_10_cat)
PS$ps_car_11_cat = factor(PS$ps_car_11_cat)

PS$ps_car_11[PS$ps_car_11==-1] = NA
PS$ps_car_11 = ordered(PS$ps_car_11)

PS$ps_car_12[PS$ps_car_12==-1] = NA

PS$ps_car_14[PS$ps_car_14==-1] = NA

PS$ps_calc_04 = ordered(PS$ps_calc_04)
PS$ps_calc_05 = ordered(PS$ps_calc_05)
PS$ps_calc_06 = ordered(PS$ps_calc_06)
PS$ps_calc_07 = ordered(PS$ps_calc_07)
PS$ps_calc_08 = ordered(PS$ps_calc_08)
PS$ps_calc_09 = ordered(PS$ps_calc_09)
PS$ps_calc_10 = ordered(PS$ps_calc_10)
PS$ps_calc_11 = ordered(PS$ps_calc_11)
PS$ps_calc_12 = ordered(PS$ps_calc_12)
PS$ps_calc_13 = ordered(PS$ps_calc_13)
PS$ps_calc_14 = ordered(PS$ps_calc_14)

PS$ps_calc_15_bin = factor(PS$ps_calc_15_bin)
PS$ps_calc_16_bin = factor(PS$ps_calc_16_bin)
PS$ps_calc_17_bin = factor(PS$ps_calc_17_bin)
PS$ps_calc_18_bin = factor(PS$ps_calc_18_bin)
PS$ps_calc_19_bin = factor(PS$ps_calc_19_bin)
PS$ps_calc_20_bin = factor(PS$ps_calc_20_bin)

summary(PS)

########################################
# exploration
# the meaning of grouping
########################################
install.packages("psych")
library(psych)

group_ind = PS[,3:20]
group_reg = PS[,21:23]
group_car = PS[,24:39]
group_calc = PS[,40:59]

## group_ind -- all factor


## group_reg -- all numeric
# correlationship
corrplot.mixed(cor(group_reg,use = "pairwise"),
               lower="ellipse",upper="number",order="AOE")
# correlation matrix and tests of significance
corr.test(group_reg,use = "pairwise")

## group_car
carnums = sapply(group_car, is.numeric)
group_car_num = group_car[,carnums]
corrplot.mixed(cor(group_car_num,use = "pairwise"),
               lower="ellipse",upper="number",order="AOE")
corr.test(group_car_num,use = "pairwise")

# group_calc
calcnums = sapply(group_calc, is.numeric)
group_calc_num = group_calc[,calcnums]
corrplot.mixed(cor(group_calc_num,use = "pairwise"),
               lower="ellipse",upper="number",order="AOE")
corr.test(group_calc_num,use = "pairwise")


########################################
# Preprocessing
# missing value
# pattern-- expecially, ps_car_03_cat, ps_car_05_cat, and ps_reg_03
########################################
install.packages("VIM")
library(VIM)

PSnomissing = PS[complete.cases(PS),] #124931
PSmissing = PS[!complete.cases(PS),]  #470281

histMiss(PS[c("ps_car_03_cat")])
histMiss(PS$ps_car_05_cat)
mosaicMiss(PS[c("ps_car_03_cat","ps_car_05_cat","target")],shade=TRUE)

# correlation test between attribute’s missningness
x = as.data.frame(abs(is.na(PS)))
y = x[which(apply(x,2,sum)>0)]
cor(y)
corr.test(y)

# ps_car_03_cat, ps_car_05_cat, target
PSoriginal$ps_car_03_cat = factor(PSoriginal$ps_car_03_cat)
PSoriginal$ps_car_05_cat = factor(PSoriginal$ps_car_05_cat)
PSoriginal$target = factor(PSoriginal$target)

tablecar03target = xtabs(~ps_car_03_cat+target,data = PSoriginal)
tablecar03target
prop.table(tablecar03target,1)

tablecar05target = xtabs(~ps_car_05_cat+target,data = PSoriginal)
tablecar05target
prop.table(tablecar05target,1)

# whether 
chisq.test(PS$target,y$ps_car_03_cat) #sig
fisher.test(table(PS$target,y$ps_car_03_cat))
chisq.test(PS$target,y$ps_car_05_cat) #sig
fisher.test(table(PS$target,y$ps_car_05_cat))
chisq.test(PS$target,PS$ps_car_03_cat) #sig
fisher.test(table(PS$target,PS$ps_car_03_cat))
chisq.test(PS$target,PS$ps_car_05_cat) #not sig
fisher.test(table(PS$target,PS$ps_car_05_cat))
chisq.test(PSoriginal$target,PSoriginal$ps_car_03_cat) #sig
chisq.test(PSoriginal$target,PSoriginal$ps_car_05_cat) #sig

########################################
# Preprocessing
# missing value
# traditional way
########################################

PSnomissing = PS[complete.cases(PS),] #124931
PSmissing = PS[!complete.cases(PS),]  #470281

# delete high missing value ratio variables: 
# ps_car_03_cat, ps_car_05_cat, ps_reg_03
delete = c("ps_car_03_cat",
           "ps_car_05_cat",
           "ps_reg_03")
PST = PS[ ,!(names(PS) %in% delete)]
PS = PS[ ,!(names(PS) %in% delete)]

# listwise approach
PST = na.omit(PST) 
PS = na.omit(PS)
#541860, while original is 595212

# delete variable id
PST = PST[,-1]
PS = PS[,-1]

########################################
# Preprocessing
# Exploration for the dataset
########################################
install.packages("ggplot2")
library(ggplot2)
install.packages("corrplot")
library(corrplot)
install.packages("gridExtra")
library(gridExtra)

## NUMERIC
# numeric subset
nums = sapply(PST, is.numeric)
subnumeric = PST[,nums]
names(subnumeric)

#hist(PS$ps_reg_01,freq = FALSE, breaks = 10, col = "red",
#     xlab = "ps_reg_01", main = "Histogram of ps_reg_01 with density curve") +
#  lines(density(PS$ps_reg_01), col="blue", lwd=2)
n11 = ggplot(PST,aes(ps_reg_01))+geom_histogram(bins = 10)+
  labs(title="Histogram of ps_reg_01") +
  scale_x_continuous(limits = c(0, 1)) +theme(plot.title=element_text(size=12))
n12 = ggplot(PST,aes(x=target,y=ps_reg_01,fill = target))+geom_boxplot()+
  labs(title="Boxplot of ps_reg_01 by target")+theme(plot.title=element_text(size=12))
n13 = ggplot(PST, aes(ps_reg_01,fill=target))+geom_density(alpha=.5)+
  labs(title="Density curve of ps_reg_01 by target")+theme(plot.title=element_text(size=12))

n21 = ggplot(PST,aes(ps_reg_02))+geom_histogram(bins = 10)+
  labs(title="Histogram of ps_reg_02")+
  theme(plot.title=element_text(size=12))
#+scale_x_continuous(limits = c(0, 1))
n22 = ggplot(PST,aes(x=target,y=ps_reg_02,fill = target))+geom_boxplot()+
  labs(title="Boxplot of ps_reg_02 by target")+theme(plot.title=element_text(size=12))
n23 = ggplot(PST, aes(ps_reg_02,fill=target))+geom_density(alpha=.5)+
  labs(title="Density curve of ps_reg_02 by target")+theme(plot.title=element_text(size=12))

n31 = ggplot(PST,aes(ps_car_12))+geom_histogram(bins = 10)+
  labs(title="Histogram of ps_car_12") +theme(plot.title=element_text(size=12))
#+scale_x_continuous(limits = c(0, 1))
n32 = ggplot(PST,aes(x=target,y=ps_car_12,fill = target))+geom_boxplot()+
  labs(title="Boxplot of ps_car_12 by target")+theme(plot.title=element_text(size=12))
n33 = ggplot(PST, aes(ps_car_12,fill=target))+geom_density(alpha=.5)+
  labs(title="Density curve of ps_car_12 by target")+theme(plot.title=element_text(size=12))

n41 = ggplot(PST,aes(ps_car_13))+geom_histogram(bins = 10)+
  labs(title="Histogram of ps_car_13") +theme(plot.title=element_text(size=12))
#+scale_x_continuous(limits = c(0, 1))
n42 = ggplot(PST,aes(x=target,y=ps_car_13,fill = target))+geom_boxplot()+
  labs(title="Boxplot of ps_car_13 by target")+theme(plot.title=element_text(size=12))
n43 = ggplot(PST, aes(ps_car_13,fill=target))+geom_density(alpha=.5)+
  labs(title="Density curve of ps_car_13 by target")+theme(plot.title=element_text(size=12))

n51 = ggplot(PST,aes(ps_car_14))+geom_histogram(bins = 10)+
  labs(title="Histogram of ps_car_14") +theme(plot.title=element_text(size=12))
#+scale_x_continuous(limits = c(0, 1))
n52 = ggplot(PST,aes(x=target,y=ps_car_14,fill = target))+geom_boxplot()+
  labs(title="Boxplot of ps_car_14 by target")+theme(plot.title=element_text(size=12))
n53 = ggplot(PST, aes(ps_car_14,fill=target))+geom_density(alpha=.5)+
  labs(title="Density curve of ps_car_14 by target")+theme(plot.title=element_text(size=12))

n61 = ggplot(PST,aes(ps_car_15))+geom_histogram(bins = 10)+
  labs(title="Histogram of ps_car_15") +theme(plot.title=element_text(size=12))
#+scale_x_continuous(limits = c(0, 1))
n62 = ggplot(PST,aes(x=target,y=ps_car_15,fill = target))+geom_boxplot()+
  labs(title="Boxplot of ps_car_15 by target")+theme(plot.title=element_text(size=12))
n63 = ggplot(PST, aes(ps_car_15,fill=target))+geom_density(alpha=.5)+
  labs(title="Density curve of ps_car_15 by target")+theme(plot.title=element_text(size=12))

n71 = ggplot(PST,aes(ps_calc_01))+geom_histogram(bins = 10)+
  labs(title="Histogram of ps_calc_01") +theme(plot.title=element_text(size=12))
#+scale_x_continuous(limits = c(0, 1))
n72 = ggplot(PST,aes(x=target,y=ps_calc_01,fill = target))+geom_boxplot()+
  labs(title="Boxplot of ps_calc_01 by target")+theme(plot.title=element_text(size=12))
n73 = ggplot(PST, aes(ps_calc_01,fill=target))+geom_density(alpha=.5)+
  labs(title="Density curve of ps_calc_01 by target")+theme(plot.title=element_text(size=12))

n81 = ggplot(PST,aes(ps_calc_02))+geom_histogram(bins = 10)+
  labs(title="Histogram of ps_calc_02") +theme(plot.title=element_text(size=12))
#+scale_x_continuous(limits = c(0, 1))
n82 = ggplot(PST,aes(x=target,y=ps_calc_02,fill = target))+geom_boxplot()+
  labs(title="Boxplot of ps_calc_02 by target")+theme(plot.title=element_text(size=12))
n83 = ggplot(PST, aes(ps_calc_02,fill=target))+geom_density(alpha=.5)+
  labs(title="Density curve of ps_calc_02 by target")+theme(plot.title=element_text(size=12))

n91 = ggplot(PST,aes(ps_calc_03))+geom_histogram(bins = 10)+
  labs(title="Histogram of ps_calc_03") +theme(plot.title=element_text(size=12))
#+scale_x_continuous(limits = c(0, 1))
n92 = ggplot(PST,aes(x=target,y=ps_calc_03,fill = target))+geom_boxplot()+
  labs(title="Boxplot of ps_calc_03 by target")+theme(plot.title=element_text(size=12))
n93 = ggplot(PST, aes(ps_calc_03,fill=target))+geom_density(alpha=.5)+
  labs(title="Density curve of ps_calc_03 by target")+theme(plot.title=element_text(size=12))

grid.arrange(n11, n12, n13, 
             n21, n22, n23,
             n31, n32, n33,
             ncol=3)

grid.arrange(n41, n42, n43, 
             n51, n52, n53,
             n61, n62, n63,
             ncol=3)

grid.arrange(n71, n72, n73, 
             n81, n82, n83,
             n91, n92, n93,
             ncol=3)

# correlation visualization and t-test
corrplot.mixed(cor(subnumeric),
               tl.cex=0.5,
#               title="Corrplot of Numeric Varaibles",
               lower="ellipse",upper="number",order="AOE")
cor.test(cor(subnumeric))
x1 = as.data.frame(abs(subnumeric))
y1 = x1[which(apply(x1,2,sum)>0)]
cor(y1)
corr.test(y1)

## Ordered factor
# ordered categorical subset
orderfactor = sapply(PST, is.ordered)
subordered = PST[,orderfactor]
suborderedwithtarget = PST[,(names(PST) %in% c(names(subordered),"target"))]

names(suborderedwithtarget)

attach(suborderedwithtarget)
#o1 = ggplot(suborderedwithtarget,aes(x=target,fill=ps_ind_01))+geom_bar(position="dodge")
o1 = ggplot(suborderedwithtarget,aes(x=ps_ind_01,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_ind_01") +theme(plot.title=element_text(size=11))
o2 = ggplot(suborderedwithtarget,aes(x=ps_ind_03,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_ind_03") +theme(plot.title=element_text(size=11))
o3 = ggplot(suborderedwithtarget,aes(x=ps_ind_14,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_ind_14") +theme(plot.title=element_text(size=11))
o4 = ggplot(suborderedwithtarget,aes(x=ps_ind_15,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_ind_15") +theme(plot.title=element_text(size=11))
o5 = ggplot(suborderedwithtarget,aes(x=ps_car_11,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_car_11") +theme(plot.title=element_text(size=11))
o6 = ggplot(suborderedwithtarget,aes(x=ps_calc_04,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_calc_04") +theme(plot.title=element_text(size=11))
o7 = ggplot(suborderedwithtarget,aes(x=ps_calc_05,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_calc_05") +theme(plot.title=element_text(size=11))
o8 = ggplot(suborderedwithtarget,aes(x=ps_calc_06,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_calc_06") +theme(plot.title=element_text(size=11))
o9 = ggplot(suborderedwithtarget,aes(x=ps_calc_07,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_calc_07") +theme(plot.title=element_text(size=11))
o10 = ggplot(suborderedwithtarget,aes(x=ps_calc_08,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_calc_08") +theme(plot.title=element_text(size=11))
o11 = ggplot(suborderedwithtarget,aes(x=ps_calc_09,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_calc_09") +theme(plot.title=element_text(size=11))
o12 = ggplot(suborderedwithtarget,aes(x=ps_calc_10,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_calc_10") +theme(plot.title=element_text(size=11))
o13 = ggplot(suborderedwithtarget,aes(x=ps_calc_11,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_calc_11") +theme(plot.title=element_text(size=11))
o14 = ggplot(suborderedwithtarget,aes(x=ps_calc_12,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_calc_12") +theme(plot.title=element_text(size=11))
o15 = ggplot(suborderedwithtarget,aes(x=ps_calc_13,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_calc_13") +theme(plot.title=element_text(size=11))
o16 = ggplot(suborderedwithtarget,aes(x=ps_calc_14,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_calc_14") +theme(plot.title=element_text(size=11))

grid.arrange(o1,o2,o3,
             o4,o5,o6,
             o7,o8,o9,
             ncol=3)
grid.arrange(o10,o11,o12,
             o13,o14,o15,
             o16,
             ncol=2)

## Other categorical
subcategoricalwithtarget = PST[,!(names(PST) %in% c(names(subordered),names(subnumeric)))]
names(subcategoricalwithtarget)

c1 = ggplot(subcategoricalwithtarget,aes(x=ps_ind_02_cat,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_ind_02_cat") +theme(plot.title=element_text(size=11))
c2 = ggplot(subcategoricalwithtarget,aes(x=ps_ind_04_cat,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_ind_04_cat") +theme(plot.title=element_text(size=11))
c3 = ggplot(subcategoricalwithtarget,aes(x=ps_ind_05_cat,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_ind_05_cat") +theme(plot.title=element_text(size=11))
c4 = ggplot(subcategoricalwithtarget,aes(x=ps_ind_06_bin,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_ind_06_bin") +theme(plot.title=element_text(size=11))
c5 = ggplot(subcategoricalwithtarget,aes(x=ps_ind_07_bin,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_ind_07_bin") +theme(plot.title=element_text(size=11))
c6 = ggplot(subcategoricalwithtarget,aes(x=ps_ind_08_bin,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_ind_08_bin") +theme(plot.title=element_text(size=11))
c7 = ggplot(subcategoricalwithtarget,aes(x=ps_ind_09_bin,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_ind_09_bin") +theme(plot.title=element_text(size=11))
c8 = ggplot(subcategoricalwithtarget,aes(x=ps_ind_10_bin,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_ind_10_bin") +theme(plot.title=element_text(size=11))
c9 = ggplot(subcategoricalwithtarget,aes(x=ps_ind_11_bin,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_ind_11_bin") +theme(plot.title=element_text(size=11))
c10 = ggplot(subcategoricalwithtarget,aes(x=ps_ind_12_bin,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_ind_12_bin") +theme(plot.title=element_text(size=11))
c11 = ggplot(subcategoricalwithtarget,aes(x=ps_ind_13_bin,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_ind_13_bin") +theme(plot.title=element_text(size=11))
c12 = ggplot(subcategoricalwithtarget,aes(x=ps_ind_16_bin,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_ind_16_bin") +theme(plot.title=element_text(size=11))
c13 = ggplot(subcategoricalwithtarget,aes(x=ps_ind_17_bin,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_ind_17_bin") +theme(plot.title=element_text(size=11))
c14 = ggplot(subcategoricalwithtarget,aes(x=ps_ind_18_bin,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_ind_18_bin") +theme(plot.title=element_text(size=11))
c15 = ggplot(subcategoricalwithtarget,aes(x=ps_car_01_cat,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_car_01_cat") +theme(plot.title=element_text(size=11))
c16 = ggplot(subcategoricalwithtarget,aes(x=ps_car_02_cat,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_car_02_cat") +theme(plot.title=element_text(size=11))
c17 = ggplot(subcategoricalwithtarget,aes(x=ps_car_04_cat,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_car_04_cat") +theme(plot.title=element_text(size=11))
c18 = ggplot(subcategoricalwithtarget,aes(x=ps_car_06_cat,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_car_06_cat") +theme(plot.title=element_text(size=11))
c19 = ggplot(subcategoricalwithtarget,aes(x=ps_car_07_cat,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_car_07_cat") +theme(plot.title=element_text(size=11))
c20 = ggplot(subcategoricalwithtarget,aes(x=ps_car_08_cat,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_car_08_cat") +theme(plot.title=element_text(size=11))
c21 = ggplot(subcategoricalwithtarget,aes(x=ps_car_09_cat,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_car_09_cat") +theme(plot.title=element_text(size=11))
c22 = ggplot(subcategoricalwithtarget,aes(x=ps_car_10_cat,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_car_10_cat") +theme(plot.title=element_text(size=11))
c23 = ggplot(subcategoricalwithtarget,aes(x=ps_car_11_cat,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_car_11_cat") +theme(plot.title=element_text(size=11))
c24 = ggplot(subcategoricalwithtarget,aes(x=ps_calc_15_bin,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_calc_15_bin") +theme(plot.title=element_text(size=11))
c25 = ggplot(subcategoricalwithtarget,aes(x=ps_calc_16_bin,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_calc_16_bin") +theme(plot.title=element_text(size=11))
c26 = ggplot(subcategoricalwithtarget,aes(x=ps_calc_17_bin,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_calc_17_bin") +theme(plot.title=element_text(size=11))
c27 = ggplot(subcategoricalwithtarget,aes(x=ps_calc_18_bin,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_calc_18_bin") +theme(plot.title=element_text(size=11))
c28 = ggplot(subcategoricalwithtarget,aes(x=ps_calc_19_bin,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_calc_19_bin") +theme(plot.title=element_text(size=11))
c29 = ggplot(subcategoricalwithtarget,aes(x=ps_calc_20_bin,fill=target))+geom_bar(position="dodge")+
  labs(title="Bar Chart of ps_calc_20_bin") +theme(plot.title=element_text(size=11))


grid.arrange(c2,c4,c5,
             c6,c7,c8,
             c9,c10,c11,
             ncol=3)
grid.arrange(c12,c13,c14,
             c16,c19,c20,
             c22,c24,c25,
             ncol=3)
grid.arrange(c26,c27,c28,
             c29,c1,c21,
             c3,c17,c15,
             ncol=3)
grid.arrange(c18,
             c23,
             ncol=1)

levels(subcategoricalwithtarget$ps_car_11_cat)

##############################
# Analysis -- Decision Tree
##############################
# Training and testing dataset splitting (80%,20%)
set.seed(2222)
ind = sample(3,nrow(PS),replace = TRUE, prob = c(0.7,0.15,0.15))
PStrain = PS[ind==1,]
PSvalidation = PS[ind==2,]
PStest = PS[ind==3,]

# Normalized GINI
normalizedGini <- function(aa, pp) {
  Gini <- function(a, p) {
    if (length(a) !=  length(p)) stop("Actual and Predicted need to be equal lengths!")
    temp.df <- data.frame(actual = a, pred = p, range=c(1:length(a)))
    temp.df <- temp.df[order(-temp.df$pred, temp.df$range),]
    population.delta <- 1 / length(a)
    total.losses <- sum(a)
    null.losses <- rep(population.delta, length(a)) # Hopefully is similar to accumulatedPopulationPercentageSum
    accum.losses <- temp.df$actual / total.losses # Hopefully is similar to accumulatedLossPercentageSum
    gini.sum <- cumsum(accum.losses - null.losses) # Not sure if this is having the same effect or not
    sum(gini.sum) / length(a)
  }
  Gini(aa,pp) / Gini(aa,aa)
}

# Cross validation on PSTtrain and build decision tree
install.packages("rpart")
library(rpart)
set.seed(2222)
dtree1 = rpart(target~.,data = PStrain,method = "class",
               parms = list(split="normalizedGini"),
               control=rpart.control(minsplit=2, minbucket=1,cp=0.00001))
dtree1$cptable

dtree2 = rpart(target~.,data = PStrain,method = "class",
               parms = list(split="normalizedGini"),
               control=rpart.control(minsplit=2, minbucket=1,cp=0.0001))
dtree2$cptable

#install.packages("caret")
#library(caret)
#set.seed(2222)
#traincontrol = trainControl(method = "repeatedcv",
#                            repeats = 2,
#                            classProbs = TRUE,
#                            summaryFunction = twoClassSummary)
#feature.names=names(PStrain)
#for (f in feature.names) {
#  if (class(PStrain[[f]])=="factor") {
#    levels <- unique(c(PStrain[[f]]))
#    PStrain[[f]] <- factor(PStrain[[f]],
#                         labels=make.names(levels))
#  }
#}
#dtree2 = train(target~.,data = PStrain, trControl = traincontrol,
#               method = "rpart")


##############################
# Analysis -- Random Forest
##############################
install.packages("randomForest")
library(randomForest)
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

# initial try
rf1 = randomForest(target~.-ps_car_11_cat,data=PStrain,ntree=100,cp=0.0001)
rf1
predict_rf1 = predict(rf1,PSvalidation,typs="class")
t_rf1 = table(predict=predict_rf1,reference=PSvalidation$target)
confusionMatrix(t_rf1)
plot(rf1)
importance(rf1,type = 2)

# try to change the random forest tree size (from 100 to 200)
rf2 = randomForest(target~.-ps_car_11_cat,data=PStrain,ntree=200,cp=0.0001)
predict_rf2 = predict(rf2,PSvalidation,typs="class")
t_rf2 = table(predict=predict_rf2,reference=PSvalidation$target)
confusionMatrix(t_rf2)
plot(rf2)
importance(rf2, type = 2)

# try to change the random forest tree size (from 200 to 500)
rf5 = randomForest(target~.-ps_car_11_cat,data=PStrain,ntree=500,cp=0.0001)
predict_rf5 = predict(rf5,PSvalidation,typs="class")
t_rf5 = table(predict=predict_rf5,reference=PSvalidation$target)
confusionMatrix(t_rf5)
plot(rf5)
importance(rf5, type = 2)

# change the weights of the class
# 80% on target "1"
rf6 = randomForest(target~.-ps_car_11_cat,classwt = c(0.2, 0.8),data=PStrain,ntree=100,cp=0.0001)
predict_rf6 = predict(rf6,PSvalidation,typs="class")
t_rf6 = table(predict=predict_rf6,reference=PSvalidation$target)
confusionMatrix(t_rf6)

rf6.1 = randomForest(target~.-ps_car_11_cat,data=PStrain,ntree=100,cp=0.0001,
                     sampsize=c(1=10, versicolor=30, virginica=10))
  
# 90% on target "1"
rf7 = randomForest(target~.-ps_car_11_cat,classwt = c(0.1, 0.9),data=PStrain,ntree=100,cp=0.0001)
predict_rf7 = predict(rf7,PSvalidation,typs="class")
t_rf7 = table(predict=predict_rf7,reference=PSvalidation$target)
confusionMatrix(t_rf7)

rf7 = randomForest(target~.-ps_car_11_cat,classwt = c(0.3, 0.7),data=PStrain,ntree=100,cp=0.0001)
predict_rf7 = predict(rf6,PSvalidation,typs="class")
confusionMatrix(t_rf7)

#################################
# Analysis -- Decision Tree again
#################################
install.packages("rpart")
library(rpart)
set.seed(2222)
dtree_1 = rpart(target~ps_car_13+ps_car_14+ps_calc_10+ps_calc_14+ps_calc_11+ps_car_06_cat
                +ps_ind_15+ps_reg_02+ps_calc_03+ps_calc_02+ps_calc_01+ps_ind_03+ps_calc_13
                +ps_calc_08+ps_calc_07+ps_car_15+ps_calc_06+ps_calc_09+ps_calc_05+ps_car_01_cat
                +ps_reg_01+ps_calc_04+ps_ind_01+ps_calc_12+ps_car_12,data = PStrain,method = "class",
               parms = list(split="normalizedGini"),
               control=rpart.control(minsplit=2, minbucket=1,cp=0.00001))
dtree_1$cptable

#################################
# Analysis -- Random Forest again
#################################

rf_1 = randomForest(target~ps_car_13+ps_car_14+ps_calc_10+ps_calc_14+ps_calc_11+ps_car_06_cat
                   +ps_ind_15+ps_reg_02+ps_calc_03+ps_calc_02+ps_calc_01+ps_ind_03+ps_calc_13
                   +ps_calc_08+ps_calc_07+ps_car_15+ps_calc_06+ps_calc_09+ps_calc_05+ps_car_01_cat
                   +ps_reg_01+ps_calc_04+ps_ind_01+ps_calc_12+ps_car_12,data=PStrain,ntree=100,cp=0.0001)
rf1
predict_rf_1 = predict(rf_1,PSvalidation,typs="class")
t_rf_1 = table(predict=predict_rf_1,reference=PSvalidation$target)
confusionMatrix(t_rf_1)
plot(rf_1)
importance(rf_1,type = 2)

# try to change the random forest tree size (from 100 to 200)
rf_2 = randomForest(target~ps_car_13+ps_car_14+ps_calc_10+ps_calc_14+ps_calc_11+ps_car_06_cat
                    +ps_ind_15+ps_reg_02+ps_calc_03+ps_calc_02+ps_calc_01+ps_ind_03+ps_calc_13
                    +ps_calc_08+ps_calc_07+ps_car_15+ps_calc_06+ps_calc_09+ps_calc_05+ps_car_01_cat
                    +ps_reg_01+ps_calc_04+ps_ind_01+ps_calc_12+ps_car_12,data=PStrain,ntree=200,cp=0.0001)
predict_rf_2 = predict(rf_2,PSvalidation,typs="class")
t_rf_2 = table(predict=predict_rf_2,reference=PSvalidation$target)
confusionMatrix(t_rf_2)

# change the weights of the class
# 80% on target "1"
rf_6 = randomForest(target~ps_car_13+ps_car_14+ps_calc_10+ps_calc_14+ps_calc_11+ps_car_06_cat
                    +ps_ind_15+ps_reg_02+ps_calc_03+ps_calc_02+ps_calc_01+ps_ind_03+ps_calc_13
                    +ps_calc_08+ps_calc_07+ps_car_15+ps_calc_06+ps_calc_09+ps_calc_05+ps_car_01_cat
                    +ps_reg_01+ps_calc_04+ps_ind_01+ps_calc_12+ps_car_12,classwt = c(0.2, 0.8),data=PStrain,
                    ntree=100,cp=0.0001)
predict_rf_6 = predict(rf_6,PSvalidation,typs="class")
t_rf_6 = table(predict=predict_rf_6,reference=PSvalidation$target)
confusionMatrix(t_rf_6)

# 90% on target "1"
rf_7 = randomForest(target~ps_car_13+ps_car_14+ps_calc_10+ps_calc_14+ps_calc_11+ps_car_06_cat
                    +ps_ind_15+ps_reg_02+ps_calc_03+ps_calc_02+ps_calc_01+ps_ind_03+ps_calc_13
                    +ps_calc_08+ps_calc_07+ps_car_15+ps_calc_06+ps_calc_09+ps_calc_05+ps_car_01_cat
                    +ps_reg_01+ps_calc_04+ps_ind_01+ps_calc_12+ps_car_12,classwt = c(0.1, 0.9),
                    data=PStrain,ntree=100,cp=0.0001)
predict_rf_7 = predict(rf_7,PSvalidation,typs="class")
t_rf_7 = table(predict=predict_rf_7,reference=PSvalidation$target)
confusionMatrix(t_rf_7)

# Still no difference about the rf_1, rf_2, rf_6, and rf_7
