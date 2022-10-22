

# information

lambda = 0.76
psi = 1
tau <- c(-1.2, 1)
D <- 1.6

a <- lambda/psi

b <- c(-1000,tau/(D*lambda),1000)
theta <- 0

t <- 1
p0 <- numeric(length(b)-1)
info_cat <- numeric(length(b)-1)


for(t in 1:length(p0)){
  p0[t] = pnorm(a*(theta - b[t])) - pnorm(a*(theta - b[t+1]))
  pder = a*dnorm(a*(theta-b[t])) - a*dnorm(a*(theta-b[t+1]))
  info_cat[t] = (pder)^2/(p0[t])
}
p0
info_cat
info_item = sum(info_cat)
info_test = sum(info_item)

# information 
# p*(theta) = pnorm(a * (theta - b[t]))
# have R obtain derivative for us
D(quote(pnorm(a * (theta - b))), "theta")
# derivative of p*(theta) = a*dnorm(a * (theta - b[t]))



tau <- parameterestimates(fit) %>%
  filter(op == "|") %>%
  dplyr::select(lhs,rhs,est) %>%
  pivot_wider(
    id_cols = "lhs",
    names_from = "rhs",
    values_from = "est"
  )

lambda <- parameterestimates(fit) %>%
  filter(op == "=~") %>%
  dplyr::select(lhs,rhs,est) %>%
  pivot_wider(
    id_cols = "lhs",
    names_from = "rhs",
    values_from = "est",
    values_fill = 0
  )

theta <- parameterestimates(fit) %>%
  filter(op == "~~", lhs==rhs, !(lhs%in%c("C1","C2","S1","S2","D1","D2"))) %>%
  dplyr::select(lhs,rhs,est) %>%
  pivot_wider(
    id_cols = "lhs",
    names_from = "rhs",
    values_from = "est"
  )
  
  
iNames <- c(paste0("CG",1:3,"_1"), paste0("CI",1:3,"_1"))
lambda = lambda[1,iNames, drop=T]
theta = diag(as.matrix(theta[,iNames]))
tau <- tau %>% filter(lhs %in% iNames)
D <- 1.6

nItem = ncol(lambda)
nThresh = ncol(tau)-1
a <- numeric(nItem)
b <- tau[,-1]
i <- 1
for(i in 1:nItem){
  # compute discrimination
  a[i] <- lambda[i]/theta[i]
  # compute item location
  b[i,1] <- b[i,1]/(D*lambda[i])
  b[i,2] <- b[i,2]/(D*lambda[i])
  b[i,3] <- b[i,3]/(D*lambda[i])
  b[i,4] <- b[i,4]/(D*lambda[i])
  b[i,5] <- b[i,5]/(D*lambda[i])
  b[i,6] <- b[i,6]/(D*lambda[i])
}
a <- as.numeric(a)
b <- as.matrix(cbind(t0=rep(-1000, nItem), b, t7=rep(1000, nItem)))




theta <- seq(-4,4,0.01)
t <- 1
p0 <- numeric(nThresh+1)
info_cat <- array(dim = c(nThresh+1,nItem,length(theta)))
info_item <- matrix(0,ncol=nItem+1, nrow=length(theta))
info_test <- matrix(0,ncol=2, nrow=length(theta))

# loop around theta
for(t in 1:length(theta)){
  # loop around items
  for(i in 1:nItem){
    for(c in 1:(nThresh+1)){
      p0 = pnorm(a[i]*(theta[t] - b[i,c])) - pnorm(a[i]*(theta[t] - b[i,c+1])) + 1e-16
      pder = a[i]*dnorm(a[i]*(theta[t] - b[i,c])) - a[i]*dnorm(a[i]*(theta[t] - b[i,c+1]))
      info_cat[c,i,t] = (pder)^2/(p0)
    }
    info_item[t,1] <- theta[t]
    info_item[t,i+1] <- sum(info_cat[,i,t])
  }
  info_test[t,1] <- theta[t]
  info_test[t,2] <- sum(info_item[t,2:(nItem+1)])
}

info_test <- as.data.frame(info_test)
colnames(info_test) <- c("theta", "info")

info_item <- as.data.frame(info_item)
colnames(info_item) <- c("theta", paste0("item_",1:nItem))
info_item <- info_item %>%
  pivot_longer(
    cols=contains("item"),
    names_to = "item",
    values_to = "info"
  )

info_item %>%
  ggplot(aes(x=theta,y=info, group=item, color=item))+
  geom_line()+
  theme_classic()


info_test %>%
  ggplot(aes(x=theta,y=info))+
  geom_line()+
  theme_classic()
