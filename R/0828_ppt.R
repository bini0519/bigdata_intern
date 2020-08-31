std90 <- read.csv("navercafe_download/data/student90.csv")
head(std90)
str(std90)
attach(std90)
#모델만들기
m <- lm(weight_kg ~ height_cm)
m
summary(m) 
#y= 0.02265 + 0.00838*x
#pvalue: 0.008385
fitted(m)
plot(m, which=4)

std90$weight_kg[1:4]
((32.6604144) + (0.2246605) * (std90$height_cm[1:4]))
fitted(m)[1:4]

std90$weight_kg[1:4]
fitted(m)[1:4] + residuals(m)[1:4]


#이상치
cooks.distance(m)
#install.packages("car")
library(car)
outlierTest(m)

#잔차
residuals(m)
#방법1
qqnorm(residuals(m))
qqline(residuals(m))
#방법2
shapiro.test(residuals(m))
#귀무가설 채택
#잔차는 정규분포를 이룬다.

#신뢰구간
confint(m,level = 0.95)
m_conf <- predict(m, level = 0.95, interval = "confidence")
m_conf

plot(weight_kg~height_cm, std90)
lwr <- m_conf[,2]
lwr
upr <- m_conf[,3]
sx <- sort(std90$height_cm, index.return=T)
sx

abline(coef(m), lwd=2)
lines(sx$x, lwr[sx$ix], col="blue", lty=2)
lines(sx$x, upr[sx$ix], col="blue", lty=2)

##유의수준 0.01
m_pred2 <- predict(m, level=0.95, interval = "predict")
head(m_pred2)
p_lwr <- m_pred2[,2]   
p_upr <- m_pred2[,3]   
lines(std90$height_cm, p_lwr, col="red", lty=2) 
lines(std90$height_cm, p_upr, col="red", lty=2) 

#잔차제곱합
deviance(m)

#예측
predict(m, newdata = data.frame(height_cm=175), interval = "confidence")
summary(m)

#분산분석
anova(m)

#모델1
(m_a <- lm(weight_kg ~ height_cm, data = std90))
#모델2
(m_b <- lm(weight_kg ~ 1, data = std90))
anova(m_a, m_b)


