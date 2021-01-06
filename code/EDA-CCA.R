setwd('C:/Users/ehd03/Downloads')
data<-read.csv(file='finaldata4.csv', header=T)
library(ggplot2)
library(plotly)
library(ggcorrplot)
library(MASS)
library(corrplot)
library(RColorBrewer)
library(CCA)
library(candisc)
library(psych)
library(psy)
#Pie Chart about Gender ratio.
gender<-as.factor(data$sex)
gender<-as.factor(data$sex)
dfGender<-data.frame("성별" = c("남성","여성"),"gender"=c(table(gender)[1],table(gender)[2]))
gen<- ggplot(dfGender, aes(x="", y=gender, fill=성별))+
  geom_bar(stat="identity", width=1)+coord_polar(theta="y" ,start=0)+ 
  scale_fill_manual(values=c("#55DDE0", "#F26419"))+
  theme_classic() + theme(axis.line = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          plot.title = element_text(hjust = 0.5, color = "#666666"))+
  geom_text(aes(label = 성별), position = position_stack(vjust = 0.5))
gen

# Pie Chart about Recognition of smartphone overdependence
think <- as.factor(data$q91)
dfThink<-data.frame("항목" = c("매우 비의존적이다","비의존적이다","보통이다","의존적이다","매우 의존적이다"),
                    "list"=c(table(think)[1],table(think)[2],table(think)[3],table(think)[4],table(think)[5]))
th<- ggplot(dfThink, aes(x="", y=list, fill=항목))+
  geom_bar(stat="identity", width=1)+coord_polar(theta="y" ,start=0)+ 
  scale_fill_manual(values=c("#55DDE0", "#33658A", "#4ed41e", "#F26419", "#999999"))+
  theme_classic() + theme(axis.line = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          plot.title = element_text(hjust = 0.5, color = "#666666"))+
  geom_text(aes(label = paste0(round((list/length(think))*100),"%")), position = position_stack(vjust = 0.5))+
  labs(x = NULL, y = NULL, fill = NULL, title = "자신의 스마트폰 과의존에 대한 인식")
th


par(mfrow=c(1,1))
for( i in (1:52)){
  qqnorm(data_int[,i] , main=colnames(data_int)[i])
  qqline(data_int[,i] , col = 2)
}


count <- 0
data_int<-subset(data , select = -c(q96,q112,q158,sex))
#리커트 척도 특성상 통계적으로 정규성 검정하기 어려움
for( i in (1:52)){
  if(shapiro.test(data_int[,i])[2] < 0.001 ){
    print(paste(colnames(data_int)[i], shapiro.test(data_int[,i])[2]))
    count = count + 1
  }
}
count
#q-q도표와 왜도 첨도를 통해 어느정도 만족한다고 가정하고 향후분석에 이용
for( i in (1:52)){
  if(abs(psych::describe(data_int[,i])$kurtosis) > 1.6 |
     abs(psych::describe(data_int[,i])$skew)> 1.6){
    print(paste(colnames(data_int)[i],psych::describe(data_int[,i])$skew,psych::describe(data_int[,i])$kurtosis))
    
  }
}
data_int2<-subset(data_int , select = -c(q49 , q50))
par(mfrow=c(4,13))
for( i in (1:52)){
  qqnorm(data_int2[,i] , main=colnames(data_int2)[i])
  qqline(data_int2[,i] , col = 2)
}

par(mfrow=c(1,1))
#상관 히트맵
corr<- round(cor(data_int2),2)  
corrplot(corr, method = "color", outline = T, addgrid.col = "darkgray", 
         order="hclust",
         cl.pos = "b", tl.col = "indianred4", 
         tl.cex = 0.5, cl.cex = 0.5, addCoef.col = "white", 
         number.digits = 2, number.cex = 0.4, 
         col = colorRampPalette(c("darkred","white","midnightblue"))(100))

#Factor Analysis
cortest.bartlett(R=cor(data_int2),4886)
datafa<-subset(data , select = c(news,schoolwork,commercialserv,transportation,search, radio,ebook,email,messanger, sns,purchase,sell,q61,q62,q63,q64,q65,q70,q143,q144,q145,q146,q148,q151,q152,q157,q155))
KMO(datafa)
cron <- psych::alpha(datafa)
cron$total
eigen(cor((datafa)))$values
plot(eigen(cor(datafa))$values, main="screeplot",
     ylab="eigenvalue"); grid(); abline(h=1, col="red", lty=2)
  
  ##Principal factor method not varimax
faa <- principal(datafa, nfactors=5,
                 score=TRUE , rotate="none")

  ##Principal Factor method – rotate:varimax
ff <- principal(datafa, nfactors=5,
                score=TRUE, rotate="varimax")

F1<-subset(data , select= c(news,schoolwork,commercialserv,transportation,search,email,sns))
F2<-subset(data , select = c(q61,q62,q63,q64,q65,q66,q67,q68,q69,q70,q51,q91))
F3<-subset(data , select = c(radio,ebook,purchase))
F4<-subset(data, select = c(q148,q151,q152,q155,q153,q154,q156))
F5<-subset(data , select = c(q143,q144,q145,q146,q147))
psych::alpha(F1)$total[1]
psych::alpha(F2)$total[1]
psych::alpha(F3)$total[1]
psych::alpha(F4)$total[1]
psych::alpha(F5)$total[1]

ff$communality

#biplot 
par(mfrow=c(1,2))
  ## rotate
biplot(head(ff$scores,1),ff$loadings)
  ## not rotate
biplot(head(faa$scores,1) , faa$loadings)


#인자를 이용한 CCA 시도
cor(ff$scores)
#종속변수
ccacorr <- subset(data , select = c(q91,q61,q62,q63,q64,q65,q70))
Dv<-scale(ccacorr)
#독립변수
Iv<-cbind(ff$scores[,1],ff$scores[,3:5]);Iv<-scale(Iv)
colnames(Iv)[1]<-"RC1"
cc1<-cc(Iv,Dv)
cc1$cor
  ##변수집단간 표본상관행렬
img.matcor(matcor(Iv, Dv))


candisc::cancor(Iv, Dv)
#canonical coefficients
-cc1$xcoef
-cc1$ycoef
dat <- data.frame(Iv,Dv)
one<-dat[,1:4]
two<-dat[,5:11]
dat1<-scale(dat)
# H0: ∑yx = 0 검정
wil <- det(cor(dat1))/(det(cor(one))*det(cor(two)))
n <- dim(one)[1];n
p <- dim(one)[2];p
q <- dim(two)[2];q
m <- n - 3/2 - (p + q)/2
s <- sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5))
F <- (1-wil^(1/s)) * (m*s - (p*q)/2 + 1) / (wil^(1/s) * p * q)
pv <- pf(F, p*q, (m*s - (p*q)/2 + 1) , lower.tail = FALSE);pv


# 종속변수와 독립변수 설정 후 CCA
with1 <- subset(data , select = c(q61,q62,q63,q64,q65))
psych::alpha(with1)$total[1]
with1<- (data$q61 + data$q62 +data$q63 + data$q64 + data$q65)/5
with2 <- subset(data , select = c(q68,q69,q70))
psych::alpha(with2)$total[1]
with2 <- (data$q68 + data$q69 + data$q70)/3
with<- cbind(with1,with2, data$q91)
with
var <- subset(data , select = c(q51,q115,q118,q119,q151,q157))

xx<- (data$game + data$video +data$music + data$ebook)/4
xx

var<-cbind(var,xx)

with<-scale(with)
var<-scale(var)
cc1<-cc(var,with)
cc1$cor
  ##변수집단간 표본상관행렬
img.matcor(matcor(var, with))


