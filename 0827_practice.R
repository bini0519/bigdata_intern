#####문제1#####
x1<-c(65,87,73,79,81,69,80,77,68,74) 
x2<-c(75,69,83,81,72,79,90,88,76,82) 
x3<-c(59,78,67,62,83,76,55,75,49,68) 
x4<-c(94,89,80,88,90,85,79,93,88,85) 
shapiro.test(x1) #0.94
shapiro.test(x2) #0.95
shapiro.test(x3) #0.88
shapiro.test(x4) #0.60
#0.05보다 크므로 모든 그룹은 정규분포를 이룬다
mydata <-c(x1,x2,x3,x4)
group <-c(rep(1,10), rep(2,10), rep(3,10),rep(4,10))
oneway.test(mydata~group, var=T) #1.653e-05
#귀무가설: 각 교육방법의 효과는 동일하다
#0.05보다 작아서 귀무가설 기각 > 효과가 동일하지 않다

####문제2####
x<-c(52,60,63,43,46,56,62,50) 
summary(x)
y<-c(58,62,62,48,50,55,68,57)  
summary(y)
shapiro.test(x) #0.60
shapiro.test(y) #0.90
#0.05보다 크므로 모든 그룹은 정규분포를 이룬다
t.test(x,y,paired = TRUE) #0.0166
#귀무가설: 교육 전과 후의 성적은 동일하다
#0.05보다 작아서 귀무가설 기각 > 교육전과 후의 성적이 변한다
#통계량을 보니 교육 후, 성적이 향상되었다.

####문제3####
pre<-c(13.2, 8.2, 10.9, 14.3, 10.7, 6.6, 9.5, 10.8, 8.8, 13.3) 
post <-c(14.0,8.8,11.2,14.2,11.8,6.4,9.8,11.3,9.3,13.6) 
summary(pre)
summary(post)
shapiro.test(pre) #0.8129
shapiro.test(post) #0.6467
#0.05보다 크므로 모든 그룹은 정규분포를 이룬다
t.test(pre,post, paired = TRUE) # 0.008539
#귀무가설: 성형 전과 후의 만족도는 동일하다
#0.05보다 작아서 귀무가설 기각 > 만족도는 변한다
#통계량을 보니 성형후 만족도가 더 높아진다

####문제4####
x<-c(15,10,13,7,9,8,21,9,14,8) 
y<-c(15,14,12,8,14,7,16,10,15,12) 
shapiro.test(x) #0.09131
shapiro.test(y) #0.2986
#0.05보다 크므로 모든 그룹은 정규분포를 이룬다
t.test(x,y) #0.6012
#귀무가설: 신약과 위약의 효과는 같다
#0.05보다 크므로 귀무가설 채택 > 신약은 위약과 같은 효과를 보인다.

####문제5####
x1<-c(23,27,24,25,29,30,26) 
x2<-c(35,32,38,36,32,33,34)
x3<-c(36,41,38,39,40,38,39) 
x4<-c(32,30,37,34,35,34,32)
shapiro.test(x1) #0.8366
shapiro.test(x2) #0.5554
shapiro.test(x3) #0.8766
shapiro.test(x4) #0.9147
#0.05보다 크므로 모든 그룹은 정규분포를 이룬다
mydata <-c(x1,x2,x3,x4)
group <-c(rep("A",7), rep("B",7), rep("C",7),rep("D",7))
oneway.test(mydata~group, var=T) #2.581e-09
#귀무가설: 온도에 따른 제품의 강도차이가 없다
#0.05보다 작아서 귀무가설 기각 > 온도에 따라 강도 차이가 있다.

#####문제6#####
x<-c(15,10,13,7,9,8,21,9,14,8) 
y<-c(15,14,12,8,14,7,16,10,15,12) 
lm(y~x)
summary(lm(y~x)) #0.01585
#귀무가설: 친절도는 만족도에 영향을 끼치지 않는다.
#0.05보다 작아서 귀무가설 기각 > 만족도에 영향을 준다.
#47%의 신뢰도를 가진다.


####문제7####
인맥관리<-c(100,90,98,79,81,69,80,77,68,54)
인성<-c(5,4,5,3,4,3,2,3,2,1)
얼굴<-c(5,3,4,3,4,3,2,3,2,1) 
성격<-c(5,3,3,2,3,3,4,3,2,1) 
mydata <-data.frame(인맥관리, 인성, 얼굴, 성격)
mydata
model <-lm(인맥관리~., data=mydata)
summary(model) #0.00102
#귀무가설: 개인요인은 인맥관리에 영향을 끼치지 않는다.
#0.05보다 작아서 귀무가설 기각 > 인성과 성격이 인맥관리에 영향을 끼친다


###문제8###
x1<-c(100,90,98,79,81,69,80,77,68,74) 
x2<-c(5,4,4,3,4,3,4,3,2,3) 
x3<-c(5,3,5,2,3,2,3,3,2,3) 
x4<-c(5,3,4,4,3,2,4,4,2,3) 
mydata <-data.frame(y=x1, s1=x2, s2=x3, s3=x4)
mydata 
model <-lm(y~., data=mydata)
summary(model) #0.002179
#귀무가설: 서비스요인은 고객충성도와 상관없다.
#0.05보다 작아서 귀무가설 기각 > 가격은 고객충성도에 영향을 끼친다.


#####문제9#####
a <- c(58, 49, 39, 99, 32, 88, 62, 30, 55, 65, 44, 55, 57, 53, 88, 42, 39)
shapiro.test(a) #0.1058
t.test(a, alternative=c("greater"), mu=55) #0.4046
#귀무가설: 0교시 수업전과 같다
#0.05보다 커서 귀무가설 채택 > 0교시 수업전과 성적은 같다.


#====데이터와 표본분포====
loans_income2 <- read.csv("navercafe_download/data/loans_income.csv")[,1]
head(loans_income2)
class(loans_income2)

loans_income <- read.csv("navercafe_download/data/loans_income.csv")
head(loans_income)
class(loans_income)

samp_mean_05 <- data.frame(
  income=tapply(sample(loans_income2,1000*5),
                         rep(1:1000, rep(5,1000)), FUN=mean), 
  type="mean_of_5")
tapply(sample(loans_income2,1000*5),rep(1:1000, rep(5,1000)), FUN=mean)
tapply(sample(loans_income2,1000*5),rep(1:1000, 5), FUN=mean)

head(rep(1:1000, rep(5,1000)),20)
length(rep(1:1000, rep(5,1000)))
head(rep(1:1000, 5),20)
length(rep(1:1000, 5))



