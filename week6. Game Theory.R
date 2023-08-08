# Week 06
# Game Theory
# 11 April 2022
# Elisabeth Putri

# COMPUTING PAYOFF USING MATRIX MULTIPLICATION
p = c(0.3, 0.7)

A = matrix(c(0, 5, 10, 0), 2, 2)
B = matrix(c(0, -10, -5, 0), 2, 2)

B
q = c(0.8, 0.2)
p %*% q     # matrix multiplication, probability multiplication

# If player A has the mixed strategy p and player B has the mixed strategy,
# Compute the expected pay off for each player choosing each strategy using matrix multiplication
## player A 
A %*% q

# player B 
B %*% p

# Set the strategies to pure strategies & verify the expected payoff for player A choosing strategy 1
ppure = c(1, 0)
qpure = c(0,1)
A %*% qpure
B %*% ppure

# COMPUTING OPTIMAL STRATEGIES
library("lpSolve")
q2 = c(0.2, 0.8)
osA = lp(direction="max", objective.in = A %*% q2, const.mat = c(1,1),
         const.dir = c("==", "=="), const.rhs = c(1,1))
osA
osA$solution

## Change the value of q and observe how it effects p
## what q should be?

osB = lp(direction = "max", objective.in = B %*% p,  const.mat = c(1,1), 
       const.dir = c("==","=="), const.rhs = c(1,1))
osB$solution


# OPTIMAL MIXED STRATEGIES
X = rbind(cbind(c(1,1), -A), c(0,1,1))
x = lp(direction = "max", objective.in = c(1,0,0),  const.mat = X, 
       const.dir = c("<=", "<=","=="), const.rhs = c(0,0,1))
x$solution
## The payoff
x[1]
## Player A's p
x[2]
## Player B's q
x[3]
