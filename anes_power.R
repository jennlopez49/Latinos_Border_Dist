## power calculations --- minimum was 70% of getting sig relationship with sample size (that I got)
# u = # of parameters - intercept
# v = n - u -1 
# f2 = r^2 / (1-r^2)
# sig.level = .05 


# testing with the r^2 from the psych model without interaction
pwr.f2.test(u = 7, v = 170, f2 = 0.11, sig.level = .05)
