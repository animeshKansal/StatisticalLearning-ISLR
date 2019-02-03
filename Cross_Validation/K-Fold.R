---
  title: "K-Fold CV"
author: "Animesh Kansal"
date: "2/3/2019"
output: html_document
---
  
  # Libraries
  
library('ISLR')
library('boot')

?cv.glm



#LOOCV

plot(mpg~horsepower,data=Auto)

glm.fit = glm(mpg~horsepower,data=Auto)
cv.glm(Auto,glm.fit)$delta # Pretty slow

degree =1:5
cv_error=rep(0,5)
for (d in degree){
  glm.fit = glm(mpg~poly(horsepower,d) ,data=Auto)
  cv_error[d] = cv.glm(Auto,glm.fit)$delta[1]
}

plot(degree, cv_error,type = 'b')




# 10 - Fold CV


degree =1:5
cv_error_10=rep(0,5)
for (d in degree){
  glm.fit = glm(mpg~poly(horsepower,d) ,data=Auto)
  cv_error_10[d] = cv.glm(Auto,glm.fit,K = 10)$delta[1]
}

plot(degree, cv_error_10,type = 'b',col='red')


