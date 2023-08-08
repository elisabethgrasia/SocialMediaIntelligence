# Week 10
# Simulating a Cascade
# 09 May 2022

N = (2*n) + 1 #number of balls
r = 20 # assume, number of red balls
n = 15 # assume, just a number
b = N-r #number of blue balls

pick_marble = function(r, b){
  p_r = r/N
  p_b = b/N
  a = sample(c('Red', 'Blue'), size = 1, prob = c(r/N, 1-r/N))
  return(a)
}

# creating a for loop function for m people
m = 100
a=1


cascade = function(m, a, r, b){
  
  results = vector()
  for (i in seq(m)){
  
  outcome = pick_marble(r,b)
  
  if(length(results)<3){        
    results=append(results, outcome)    #because the first two person don't have to think about anything, just directly declare
  } else{
    count_r=sum(results=='Red')
    count_b=sum(results=='Blue')
    
    if (outcome =='Red'){
      count_r=count_r+a
    } else{
      count_b=count_b+a
    }
    
    if (count_r > count_b){     # results based on the first two balls
      results=append(results, 'Red')
    } else{
      results=append(results, 'Blue')
  }}
  }
  return(results)
}

ans = cascade(m ,a, r, b)
table(ans)

# plotting the answers
y= ans
y[y=='Red']=1
y[y=='Blue']=-1
y=as.numeric(y)

plot(c(0,cumsum(y)), type='l', ylim= c(-100, 100))
abline(h=-2, lty=2)
abline(h=2, lty=2)

# Run the simulation 10 times, where p = 0.5, n = 1, r= 2, m=20, and a=1 to
# observe that the cascade begins at the third draw
p=0.5
n=1
r=2
m=20
a=1
b=r-n

pick_marbles2 = function(r, b){
  a = sample(c('Red', 'Blue'), size = 1, prob = c(0.5, 1-0.5))
  return(a)
}

cascade2 = function(m, a, r, b){
  
  results = vector()
  for (i in seq(m)){
    
    outcome = pick_marbles2(r,b)
    
    if(length(results)<3){        
      results=append(results, outcome)    #because the first two person don't have to think about anything, just directly declare
    } else{
      count_r=sum(results=='Red')
      count_b=sum(results=='Blue')
      
      if (outcome =='Red'){
        count_r=count_r+a
      } else{
        count_b=count_b+a
      }
      
      if (count_r > count_b){     # results based on the first two balls
        results=append(results, 'Red')
      } else{
        results=append(results, 'Blue')
      }}
  }
  return(results)
}

ans2 = cascade2(m ,a, r, b)
table(ans2)

# plotting the answers
y2= ans2
y2[y2=='Red']=1
y2[y2=='Blue']=-1
y2=as.numeric(y2)

plot(c(0,cumsum(y2)), type='l', ylim= c(-100, 100))
abline(h=-2, lty=2)
abline(h=2, lty=2)

# Set a=2, 3, 4 and observe the effect. Are the results as you expected?
ansa2 = cascade(m, 2, r, b)
table(ansa2)

ansa3 = cascade(m, 3, r, b)
table(ansa3)

ansa4 = cascade(m, 4, r, b)
table(ansa4)

# set n=5, r=6 and observe the effect. Observe the effect of incresing r.
# What do you think should happen?
n = 5
r = 6