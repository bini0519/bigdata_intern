#====데이터프레임 문제내기=====
그룹명 <- c("금호아시아나", "HDC", "웅진", "셀트리온", "KG", "태영", "케이티앤지", "코오롱")
전일대비 <- c("+4.33%", "+3.93%", "+2.14%", "+2.07%", "+1.74%", "+0.95%", "+0.36%", "+0.33%")
상승 <- c(4, 3, 1, 3, 4, 3, 1, 2)
하락 <- c(2, 1, 0, 0, 1, 1, 1, 4)
그룹별시세 <- data.frame(그룹명, 전일대비, 상승, 하락)
그룹별시세

#1시세가 하락하지 않은 그룹명을 출력하라
subset(그룹별시세, 하락 != '0', select=그룹명)
#2케이티앤지의 데이터를 출력하라
subset(그룹별시세, 그룹명 == '케이티앤지')
#3그룹명과 상승만 출력하라
그룹별시세[,c("그룹명", "상승")]

#-----반복문과 사용자정의함수-----
#1
for (i in 2:9){
  print(paste("======", i, "단======"))
  for (j in 1:9){
    print(paste(i, "X", j, "=", i*j))
  }
  i = i +1
}

#2
sum <-0
func1 <- function(x){
  for (i in 1:x){
    if (i %%3 == 0 & i %%4!=0){
      sum = sum + i
    }
  }
  print(sum)
}
func1(100)

#3
sum <-0
multipsum <- function(x,n){
  for (i in 1:n){
    if (i %%x== 0){
      sum = sum + i
    }
  }
  cat("1부터",n,"까지의 수 중에서", x,"의 배수 합은",sum,"입니다.")
}
multipsum(3,100)

#====gapminder연습====
#install.packages("gapminder")
#install.packages("dplyr")
library(gapminder)
library(dplyr)
table(gapminder$country)
gapminder[gapminder$country == "Korea, Rep." & gapminder$year > 1990, c("lifeExp", "pop")]
gapminder[gapminder$country == "Korea, Dem. Rep." & gapminder$year > 1990, c("lifeExp", "pop")]
