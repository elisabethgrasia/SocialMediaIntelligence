5. Account Popularity
# Measuring popularity of each account by Scaled PageRank

hu = rep(1, length(names(V(bb))))
names(hu) = names(V(bb))
au = hu

M = as_adjacency_matrix(bb)
M = as.matrix(M)

## iterate K times
ik = 100
for (i in 1:ik) {
  au = t(M) %*% hu
  hu = M %*% au
}

au = au/sum(au)
hu = hu/sum(hu)

# eigenvalue solution
ed1 = eigen(M %*% t(M))
hub.ed = ed1$vectors[,1]

ed2 = eigen(t(M) %*% M)
au.ed = ed2$vectors[,1]

hub.ed = hub.ed/sum(hub.ed)
au.ed = au.ed/sum(au.ed)

# Result checking
au - au.ed
hu - hub.ed

## PageRank

P = M %*% diag(1/colSums(M))
nonodes = nrow(P)
R = matrix(1/nonodes, nonodes, nonodes) 

ld = 0.8
S = ld*P + (1-ld)*R
colSums(S)

iter = rep(1/nonodes, nonodes)
for (k in 1:ik) {
  iter = S %*% iter
}

e.val = eigen(S)

pr = e.val$vector[,1]/sum(e.val$vector[,1])
pr = Re(pr)

#checking method
iter - pr

# Ten highest page rank
pro = sort(pr, index.return=TRUE)
pro$ix[1:10]
[1] 141 142 143   1   8   9  10  11  13  16

prorder = pr[order(pr,decreasing=TRUE)]
prorder[1:10]
[1] 0.308518527 0.039552618 0.026090744 0.020782265 0.017171617 0.013849264 0.012462664 0.010517661 0.010349100 0.009315947

V(lcgraphed)[141, 142, 143, 1, 8, 9, 10, 11, 13, 16]
+ 10/146 vertices, named, from 6efdbdc:
  [1] 6182852             128372940           51827346            1513921541918138372 1316181534          1021241294          1450520650335072262
[8] 1452304054206488576 1510000127745970186 1423355927302975489

em %>% filter_at(vars(user_id, reply_to_user_id), any_vars(. %in% c("6182852", "128372940", "51827346", "1513921541918138372", "1316181534", "1021241294", "1450520650335072262", "1452304054206488576", "1510000127745970186", "1423355927302975489")))
```

6. Account Selection
explained manually in the report