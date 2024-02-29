

v1<-rep(1,100); v1
v2<-rep(2, 100); v2

# number of fish
N_haul<-1000
prop_v1<-0.59 # proportion of fish v1 type
n_v1 <- round(prop_v1*N_haul) # number of fish v1 type
n_v0 <- N_haul-n_v1	# number of fish v0 type

# random mix
library(sampling)
plot(srswor(n=n_v1, N=N_haul))



windows(10,7); par(mfrow=c(2,2))
# very soft grandient
pik=inclusionprobabilities(c(1:N_haul)^0.5, n_v0)
sum(pik)
s <- UPmaxentropy(pik)
plot(s)


# soft gradient
pik=inclusionprobabilities(1:N_haul,n_v0)
s <- UPmaxentropy(pik)
plot(s)
plot(s1)

# 50 sets of gradient combined
res<-c()
for (i in 1:25)
{
print(i)
#pik=inclusionprobabilities(1:N_haul,n_v0)
pik=inclusionprobabilities(c(1:N_haul)^5,n_v0)
s <- UPmaxentropy(pik)
names(s)<-1:N_haul
res<-c(res,s)
}
res<-res[order(as.numeric(names(res)))]

plot(res)

res1<-res
res1[res1==0] <- 24 # mean weight herring
res1[res1==1] <- 8 # mean weight sprat





1516666/1000


=====================
haul_weight_g <- 20*10^6 # 20 tons

mean_weight_her_g <- 21.3
mean_weight_spr_g <- 8.47

prop_her_weight <- 0.59


haul_her_N <- round(prop_her_weight*haul_weight_g/mean_weight_her_g); haul_her_N
haul_spr_N <- round((1-prop_her_weight)*haul_weight_g/mean_weight_spr_g); haul_spr_N
haul_N <- haul_her_N+haul_spr_N; haul_N


res_len <- res
res_wt <- res
res_vol <- res
res_cum_vol<-res


# herring
    # read data
        load ("001_Inputs_SimRealHaul/Input_data_her.27.25-29_8.Rdata")
   # select a length frequency
        lf_her <- prop.table(table(df0[df0$sampId=="2009_2113","lenCls"]))
        len_pool_her <- as.integer(sample(names(lf_her), size=sum(res==0), replace=T, prob=lf_her)) # should change 

    # calculate weight-length
        mod <- lm(log(df0$indWt)~log(df0$lenCls))
        coefs_weight_length_her<-coef(mod)
        names(coefs_weight_length_her)<-c("a","b")
        coefs_weight_length_her
    
res_len[res==0] <- len_pool_her
res_wt[res==0] <- exp(coefs_weight_length_her[1])*(res_len[res==0]^coefs_weight_length_her[2])
res_vol[res==0] <- res_wt[res==0] / 932.274568364 # density her = 932.274568364 gram/liter

 # sprat
    # read data
        load ("001_Inputs_SimRealHaul/Input_data_spr.27.22-32_8.Rdata")
   # select a length frequency
        lf_spr <- prop.table(table(df0[df0$sampId=="2009_2105","lenCls"]))
        len_pool_spr <- as.integer(sample(names(lf_spr), size=sum(res==1), replace=T, prob=lf_spr))

    # calculate weight-length
        mod <- lm(log(df0$indWt)~log(df0$lenCls))
        coefs_weight_length<-coef(mod)
        names(coefs_weight_length)<-c("a","b")
        coefs_weight_length    

res_len[res==1] <- len_pool_spr
res_wt[res==1] <- exp(coefs_weight_length[1])*(res_len[res==1]^coefs_weight_length[2])
res_vol[res==1] <- res_wt[res==1] / 852.182251494 # density spr = 852.182251494 gram/liter

res_cum_vol<-cumsum(res_vol)
head(res_cum_vol) 

target_vol <- 0.25

res_box<-res_cum_vol%/%target_vol

    # n_ind per box
    max(res_box)
    summary(c(table(res_box)))
    summary(c(table(res_box[res==0])))
    summary(c(table(res_box[res==1])))

# simulates n_samples systematic sampling [for proportions]
    
    # set number of samples
        n_samples <- 30
        max(res_box)%/%n_samples
    
    # random_start
        start_box <- sample(1:((max(res_box)%/%n_samples)-1), size=1)
        sampled_box<-seq(start_box,  max(res_box), by = max(res_box)%/%n_samples)

    # prop her in weight
        # total
        sum(res_wt[res==0])/sum(res_wt)
        # in sampled boxes
        sum(res_wt[res==0 & res_box %in% sampled_box])/sum(res_wt[res_box %in% sampled_box])
        
            # in first box
            sum(res_wt[res==0 & res_box %in% sampled_box[1]])/ sum(res_wt[res_box %in% sampled_box[1]])  
            # in middle box
            sum(res_wt[res==0 & res_box %in% sampled_box[n_samples%/%2] ])/ sum(res_wt[res_box %in% sampled_box[n_samples%/%2]])  
            sum(res_wt[res==0 & res_box %in% sampled_box[n_samples%/%2+1] ])/ sum(res_wt[res_box %in% sampled_box[n_samples%/%2+1]])  
             # in last box
            sum(res_wt[res==0 & res_box %in% sampled_box[n_samples] ])/ sum(res_wt[res_box %in% sampled_box[n_samples]])  
    # n indiv
        length(res_wt[res==0 & res_box %in% sampled_box])
        length(res_wt[res==1 & res_box %in% sampled_box])


    

 sum(res_wt[res==1 & res_box %in% sampled_box])      
  sum(res_wt[res==0 & res_box %in% sampled_box])      




d = m / vol

d_her = 58.2 = 932.274568364 gram/liter

d_spr = 53.2 pound/cubic foot = 852.182251494 gram/liter

8/852.182251494 = 0.009387663 l 




# sharp gradient
plot(UPmaxentropy(pik))



