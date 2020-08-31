#p.219
data1 <- c(30, -5, 55, -30, -20, 45)
t.test(data1, alternative = c("greater")) 
#p-value = 0.2145
#귀무가설 채택 > 효과 없다.

#p222
session_times <- read.csv("navercafe_download/data/web_page_data.csv")
head(session_times)
t.test(Time~Page, alternative="less", data = session_times)
#p-value = 0.1408
#귀무가설 채택 > 페이지에 영향을 받지 않는다.

#p.229
four <- read.csv("navercafe_download/data/four_sessions.csv")
head(four)
table(four$Page)
#방법1
summary(aov(Time~Page, four)) #0.0776
#방법2
oneway.test(four$Time~four$Page, var=T) #0.07759
#귀무가설: 페이지별 종류는 머무는 시간에 영향을 끼치지 않는다.
#0.05보다 크므로 귀무가설 채택

#p.232 독립성 검정
click_rate <- read.csv("navercafe_download/data/click_rates.csv")
click_rate
clicks <- matrix(click_rate$Rate, nrow=3, ncol=2, byrow=T)
clicks
chisq.test(clicks,simulate.p.value = TRUE) #0.4793
#두 변수는 독립을 이룬다

#239 적합성 검정
obs <- c(20, 40, 40) 
obs.probs <- c(2/10, 3/10, 5/10)
chisq.test(obs, p=obs.probs) #0.06948
#종자별 분포는 멘델이 주장한 분포와 동일하다

#p.243 독립성 검정
raw_data <- c(7, 13, 9, 12, 13, 21, 10, 19, 11, 18, 12, 13)
data_mtx <- matrix(raw_data, byrow=TRUE, nrow=3)
data_mtx
dimnames(data_mtx) <- list("Class" = c("Class1", "Class2", "Class3"), 
                           "Score" = c("ScoreA", "ScoreB", "ScoreC", "ScoreF"))
data_mtx
chisq.test(data_mtx) #0.9667
#class와 score변수는 독립을 이룬다

#추가
addmargins(data_mtx) 
addmargins(prop.table(data_mtx)) 
barplot(t(data_mtx), beside=TRUE, legend=TRUE, 
        ylim=c(0, 30), 
        ylab="Observed frequencies in sample", 
        main="Frequency of math score by class")

#p.249 동질성 검정
raw_data <- c(50, 30, 20, 50, 80, 70)
data_mtx <- matrix(raw_data, byrow=TRUE, nrow=2)
data_mtx

dimnames(data_mtx) <- list("성별" = c("남학생", "여학생"), 
                           "DS교과목" = c("통계", "머신러닝", "딥러닝"))
data_mtx
chisq.test(data_mtx) #6.384e-05
#성별에 따른 교과목 비율은 동일하지 않다.

##256
lung <- read.csv("navercafe_download/data/LungDisease.csv")
head(lung)
attach(lung)
summary(lung)
#예측
model <- lm(PEFR~Exposure, lung)
summary(model)
yhat <- predict(model)
cbind(Exposure, yhat)

#시각화
plot(lung$Exposure, lung$PEFR, xlab="Exposure", ylab="PEFR")
abline(model, col="orange", lwd=4)

join <- function(i)
  lines( c(Exposure[i], Exposure[i]), c(PEFR[i], yhat[i]), col="green")
sapply(1:122, join)

##258
head(cars)
#예측
m3 <- lm(dist~speed, cars)
yhat <- round(predict(m3),0)
cbind(cars$dist, yhat) #예측값과 비교

#시각화
plot(dist~speed, data = cars)
abline(m3, col = "purple", lwd=4)
attach(cars)
join <- function(i){
  lines( c(speed[i], speed[i]), c(dist[i], yhat[i]), col="green")
  }
sapply(1:50, join)


#===== p289
data(mtcars)
head(mtcars)
fit_02=lm(formula=mpg~hp*wt, data=mtcars)
summary(fit_02)

# 회귀식 결과
# mpg=49.8-0.12*hp-8.22*wt+0.027*hp*wt

#상호작용을 그래프로 작성
plot(mpg~hp, data=mtcars, main="Interaction of hp:wt")

# curve(함수식, add=TRUE) 기존그래프에 겹쳐 그림
# wt에 2.2를 대입하면 
# 49.8-0.12*hp-8.22*wt(2.2)+0.027*hp*wt(2.2)=31.71-0.06*X

curve(31.71-0.06*x, add=TRUE)               # wt가 2.2
curve(23.49-0.03*x, add=TRUE, lty=2, col=2) # wt가 3.2
curve(15.28-0.003*x, add=TRUE, lty=3, col=3) # wt가 4.2

# lty(line type) 선의 모양(1~6)
legend("topright", c("2.2", "3.2", "4.2"), title="wt", lty=1:3, col=1:3)

# 차의 중량이 늘어날수록 마력과 연비의 관계가 약해짐을 알 수 있다

#======================= p290
#======================= 참고 p42-47

# 다중 선형 회귀 : trees data
# trees 데이터에는 벗나무 31개 각각에 대해 나무의 지름(Girth), 
# 나무의 키(Height), 목재의 부피(Volume) 3개의 숫자형 변수로 구성
# 지름(Girth), 나무의 키(Height)를 설명변수, 부피(Volume)를 반응 변수

str(trees)
summary(trees)

# trees 데이터의 분포  
#install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(trees$Girth, trees$Height, trees$Volume)


# 다중 선형 회귀 모델 생설하기 
m <- lm(Volume ~ Girth + Height, data = trees)

# trees 데이터와 회귀 모델을 중첩하여 시각화 
s <- scatterplot3d(trees$Girth, trees$Height, trees$Volume, 
                   pch = 20, type = 'h', angle = 55)
s$plane3d(m)

# 벗나무 세 그루의 지름과 키를 측정하여 부피를 예상하기  
(n.data <- data.frame(Girth=c(8.5, 13.0, 19.0), Height=c(72, 86, 85)))
(n.y <- predict(m, newdata = n.data))

# 벗나무 세 그루의 지름과 키를 측정하여 부피를 예상한 결과 시각화   
s <- scatterplot3d(c(8.5, 13.0, 19.0), c(72, 86, 85), n.y, pch = 20, type = 'h', 
                   color = 'red', angle = 55)
s$plane3d(m)

four
head(four)
head(four$Page)
four$
attach(four)
head(Page)
