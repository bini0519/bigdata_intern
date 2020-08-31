airline <- read.csv("navercafe_download/data/airline_stats.csv")
head(airline)
summary(airline)
str(airline)

#####class와 typeof의 차이####
class(airline$pct_carrier_delay) #numeric
typeof(airline$pct_carrier_delay) #double
#class: 객체지향관점(큰범위)
#typeof: 언어관점(r에서 자체적 사용, 작은범위)
#예: logical/ integer/ double/ character/ list

#str적용하면 class값으로 나옴

####is.integer(1)이 왜 거짓인지####
is.integer(1) #FALSE
is.numeric(1) #TRUE
class(1) #numeric
typeof(1) #double
is.double(1)
#1은 소수 중에서 가장 큰 수다.

####설정한 개수와 옵션의 개수가 다르다면?####
seq(0,1, length.out = 5)
#앞에서 짤린다

###집합함수####
union() #합집합
intersect() #교집합
setdiff() #차집합
setequal() #집합이 동일한지

#====dimnames옵션=====

#====sample함수====
#배열 안에서 샘플링한다

#=====팩터의 레벨화====
gender <- factor(c("A", "B", "C"))
gender

#====프레임 값 직접 입력====
test <- data.frame()
test <- edit(test)

#===칼럼명을 하나의 변수로 만들기====
head(cars)
attach(cars)
speed
detach(cars) #원래대로 붙이기
speed

#====subset====
subset(전체데이터, 조건, 변수추출)

#======집계함수=====
lapply(집계낼변수, 함수) #list로 반환
sapply(집계낼변수, 함수) #벡터로 반환


