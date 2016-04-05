### COEFFICIENT TESTING ###############################################
### Ro Neumann, 2015 ###########################################################

### INPUT: Time series with first column = time, 2nd  = data
### OUTPUT: Matrix "x_Table" with 30 columns and 11 rows
### Polynomials to be read column-wise
### If Tt < Tcrit coefficients is not significantly different from zero



coefficienttesting_function<-function (data) {
  

trend1 = lm(t ~y)
a = summary(trend1)$coefficients[,1]
a=t(a)
test1 = (tstats <- coef(trend1) / sqrt(diag(vcov(trend1))))
test1 = t(test1)
r = length(y)-2
Ttcrit1=qt(0.95,r)

trend2 <- lm(y~poly(t,2,raw=TRUE))
b = summary(trend2)$coefficients[,1]
b=t(b)
test2 = (tstats <- coef(trend2) / sqrt(diag(vcov(trend2))))
test2 = t(test2)
r = length(y)-3
Ttcrit2=qt(0.95,r)

trend3 <- lm(y~poly(t,3,raw=TRUE))
c = summary(trend3)$coefficients[,1]
c=t(c)
test3 = (tstats <- coef(trend3) / sqrt(diag(vcov(trend3))))
test3 = t(test3)
r = length(y)-4
Ttcrit3=qt(0.95,r)

trend4 <- lm(y~poly(t,4,raw=TRUE))
d = summary(trend4)$coefficients[,1]
d=t(d)
test4 = (tstats <- coef(trend4) / sqrt(diag(vcov(trend4))))
test4 = t(test4)
r = length(y)-5
Ttcrit4=qt(0.95,r)

trend5  <- lm(y~poly(t,5,raw=TRUE))
e = summary(trend5)$coefficients[,1]
e=t(e)
test5 = (tstats <- coef(trend5) / sqrt(diag(vcov(trend5))))
test5 = t(test5)
r = length(y)-6
Ttcrit5=qt(0.95,r)

trend6  <- lm(y~poly(t,6,raw=TRUE))
f = summary(trend6)$coefficients[,1]
f=t(f)
test6 = (tstats <- coef(trend6) / sqrt(diag(vcov(trend6))))
test6 = t(test6)
r = length(y)-7
Ttcrit6=qt(0.95,r)

trend7  <- lm(y~poly(t,7,raw=TRUE))
g = summary(trend7)$coefficients[,1]
g=t(g)
test7 = (tstats <- coef(trend7) / sqrt(diag(vcov(trend7))))
test7 = t(test7)
r = length(y)-8
Ttcrit7=qt(0.95,r)

trend8  <- lm(y~poly(t,8,raw=TRUE))
h = summary(trend8)$coefficients[,1]
h=t(h)
test8 = (tstats <- coef(trend8) / sqrt(diag(vcov(trend8))))
test8 = t(test8)
r = length(y)-9
Ttcrit8=qt(0.95,r)

trend9  <- lm(y~poly(t,9,raw=TRUE))
j = summary(trend9)$coefficients[,1]
j=t(j)
test9 = (tstats <- coef(trend9) / sqrt(diag(vcov(trend9))))
test9 = t(test9)
r = length(y)-10
Ttcrit9=qt(0.95,r)

trend10  <- lm(y~poly(t,10,raw=TRUE))
k = summary(trend10)$coefficients[,1]
k=t(k)
test10 = (tstats <- coef(trend10) / sqrt(diag(vcov(trend10))))
test10 = t(test10)
r = length(y)-11
Ttcrit10=qt(0.95,r)


F = matrix(0, nrow = 11, ncol = 30)

F[1:length(a),1]=a
F[1:length(test1),2]=test1
F[1:length(test1),3]=Ttcrit1
F[1:length(b),4]=b
F[1:length(test2),5]=test2
F[1:length(test2),6]=Ttcrit2
F[1:length(c),7]=c
F[1:length(test3),8]=test3
F[1:length(test3),9]=Ttcrit3
F[1:length(d),10]=d
F[1:length(test4),11]=test4
F[1:length(test4),12]=Ttcrit4
F[1:length(e),13]=e
F[1:length(test5),14]=test5
F[1:length(test5),15]=Ttcrit5
F[1:length(f),16]=f
F[1:length(test6),17]=test6
F[1:length(test6),18]=Ttcrit6
F[1:length(g),19]=g
F[1:length(test7),20]=test7
F[1:length(test7),21]=Ttcrit7
F[1:length(h),22]=h
F[1:length(test8),23]=test8
F[1:length(test8),24]=Ttcrit8
F[1:length(j),25]=j
F[1:length(test9),26]=test9
F[1:length(test9),27]=Ttcrit9
F[1:length(k),28]=k
F[1:length(test10),29]=test10
F[1:length(test10),30]=Ttcrit10


rowNames <- c('Linear coefficients', 'Tt','Tcrit', '2nd deg. coefficients', 'Tt', 'Tcrit', '3rd deg. coefficients', 'Tt', 'Tcrit', '4th deg. coefficients', 'Tt', 'Tcrit', '5th deg. coefficients', 'Tt', 'Tcrit', '6th deg. coefficients', 'Tt', 'Tcrit', '7th deg. coefficients', 'Tt', 'Tcrit', '8th deg. coefficients', 'Tt', 'Tcrit', '9th deg. coefficients', 'Tt', 'Tcrit',  '10th deg. coefficients', 'Tt', 'Tcrit')
 x_Table= rbind(rowNames,F)
 x_Table[is.na(x_Table)] <- 0
 
}

######################################################################