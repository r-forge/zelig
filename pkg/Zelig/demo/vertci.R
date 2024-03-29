
##  Attaching the sample turnout dataset:
data(turnout)

##  Estimate the model:
user.prompt()
z.out <- zelig(vote ~ race + educate + age + I(age^2) + income,
               model = "logit", data = turnout)
user.prompt()
summary(z.out)

##  Creating setx structures with education set to high school and
##  post-college levels, for the whole range of the age variable.  
user.prompt()
x.low <- setx(z.out, educate = 12, age = 18:95)
x.high <- setx(z.out, educate = 16, age = 18:95)

##  Using sim to generate the simulated predicted probabilites:
user.prompt()
s.out <- sim(z.out, x = x.low, x1 = x.high)
user.prompt()
plot.ci(s.out, xlab = "Age in Years",
        ylab = "Predicted Probability of Voting",
        main = "Effect of Education and Age on Voting Behavior")
user.prompt()
legend(45, 0.55, legend = c("College Education (16 years)",
       "High School Education (12 years)"), col = c("blue","red"), 
       lty = c("solid"))














