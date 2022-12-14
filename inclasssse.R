qnorm(c(0.025, 0.975))
?qt
?dbinom
?qnorm
qnorm(c(0.025, 0.975)) 
qt(p=c(0.025, 0.975), df = 10)
qt(p = c(0.025, 0.975), df = 65) 

qnorm(c(0.025, 0.975), mean = 10, sd = 3.14)


qt(c(0.025, 0.975), df = 49) 
tval = qt(p = c(0.025, 0.975), df = 49) 

sse_50 = 3.14 / sqrt(50) 

CIrad = tval * sse_50 

print(CIrad) 

CI = 10 + CIrad  

print(CI) 