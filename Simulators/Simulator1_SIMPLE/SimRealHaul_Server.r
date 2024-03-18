library(sampling)
library(foreach) # parallel computing
library(doMC) # parallel computing
library(doRNG) # Generic Reproducible Parallel Backend for 'foreach' Loops

# sim function
do_job1<-function(lo=i){
  s <- sample(1:N_haul, size=1000, replace=F, prob=(grad)/sum(as.numeric(grad)))
  c(1:N_haul)[s]
}  

# ===========  
# haul characteristics  
# ===========  

N_haul=1522114   
N_her=round(N_haul*0.59)
N_spr=round(N_haul*(1-0.59))
N_haul==N_her+N_spr

# ===========
# sim random mix
# ===========  
plot(srswor(n=N_her, N=N_haul))
table(haul)

# ===========
# sim gradient
# ===========  

# initiates parallel
n_CPUs <- 8
registerDoMC(n_CPUs)
print(getDoParWorkers())

# starts the seeds
set.seed(123)  

# set gradient
#grad_her <- 1:N_haul
#grad_spr <- N_haul:1

#grad_her <- (1:N_haul)^2
#grad_spr <- (N_haul:1)^2

grad_her <- (1:N_haul)^4
grad_spr <- (N_haul:1)^4


grad<-grad_her
# runs sims in parallel
system.time(out_sim <- foreach (i=1:(N_her%/%1000)) %dorng% {
  do_job1(lo=i)
  
})  
res<-do.call("c", out_sim)
length(res)
sum(duplicated(res))
length(unique(res))
summary(res)
res_her<-rep(1,length(res))
names(res_her)<-res 

grad<-grad_spr
# runs parallel
system.time(out_sim <- foreach (i=1:(N_spr%/%1000)) %dorng% {
  do_job1(lo=i)
})  
res<-do.call("c", out_sim)
length(res)
sum(duplicated(res))
length(unique(res))
summary(res)
res_spr<-rep(0,length(res))
names(res_spr)<-res

res<-c(res_her, res_spr)
res<-res[order(as.numeric(names(res)))]
names(res)<-as.numeric(names(res))

# randomizes order
ls1<-split(res, f=factor(names(res)))
ls2<-lapply(ls1, function(x) {
  if(length(unique(x))>1) {
    x1<-sample(x, size=length(x))
  } else x1<-x
  #if(length(x)!=length(x1)) browser()
  x1
})  
  
  
  res1<-do.call("c", ls2)
  length(res1)
  names(res)<-1:length(res)
  
  save(grad_her, grad_spr, N_haul, N_her, N_spr, res, res_her, res_spr, file="res_grad_power4.Rdata")
  
  #plot(cumsum(res==1))
  #plot(cumsum(res==0))
  #plot(cumsum(res))
  
  N_her<-sum(res==1); N_her
  N_spr<-sum(res==0);  N_spr

  # herring
  # read her data
  load ("001_Inputs_SimRealHaul/Input_data_her.27.25-29_8.Rdata")
  # select a sample from herring
  df_her<-df0[df0$sampId=="2009_2113",]
  
  # read spr data
  load ("001_Inputs_SimRealHaul/Input_data_spr.27.22-32_8.Rdata")
  # select a length frequency for herring
  df_spr<-df0[df0$sampId=="2009_2105",]
  
  
  meanWt_her<-mean(df_her$indWt); meanWt_her; mean(df_her$lenCls)
  meanWt_spr<-mean(df_spr$indWt); meanWt_spr; mean(df_spr$lenCls)

  res1<-res  
  
  res1[res==1] <- meanWt_her # mean weight herring
  res1[res==0] <- meanWt_spr # mean weight sprat

  # haul weights  
  sum(res1)
  # prop weights
  N_her*meanWt_her/sum(N_her*meanWt_her+N_spr*meanWt_spr)
  sum(res1[res==1])/sum(res1)

  # working objects
  res_len <- res
  res_wt <- res
  res_vol <- res
  res_cum_vol<-res


  # select a length frequency
  lf_her <- prop.table(table(df_her$lenCls)); round(lf_her*100,1); barplot(lf_her)
  lf_spr <- prop.table(table(df_spr$lenCls)); round(lf_spr*100,1); barplot(lf_spr)

  len_pool_her <- as.integer(sample(names(lf_her), size=sum(res==1), replace=T, prob=lf_her))
  len_pool_spr <- as.integer(sample(names(lf_spr), size=sum(res==0), replace=T, prob=lf_spr))
  
    # create gradient on length distribution [via UPSWOR]
      # herring
        a<-as.numeric(names(lf_her))
        a<-a[order(-a)]
        prob<-c(a/sum(a))^4; names(prob)=a[order(-a)]
        prob_pool_her_len<-prob[match(len_pool_her, a)]
        system.time(len_pool_her_grad<-sample(len_pool_her, size=N_her, replace=F, prob=prob_pool_her_len))
      # sprat
        a<-as.numeric(names(lf_spr))
        a<-a[order(-a)]
        prob<-c(a/sum(a))^4; names(prob)=a[order(-a)]
        prob_pool_spr_len<-prob[match(len_pool_spr, a)]
        system.time(len_pool_spr_grad<-sample(len_pool_spr, size=N_spr, replace=F, prob=prob_pool_spr_len))        
        # checking  
        tapply(len_pool_her_grad, (1:length(len_pool_her_grad))%/%10000, mean)
        tapply(len_pool_spr_grad, (1:length(len_pool_spr_grad))%/%10000, mean)

  
    # calculate weight-length
        mod <- lm(log(df_her$indWt)~log(df_her$lenCls))
        coefs_weight_length_her<-coef(mod)
        names(coefs_weight_length_her)<-c("a","b")
        coefs_weight_length_her

        mod <- lm(log(df_spr$indWt)~log(df_spr$lenCls))
        coefs_weight_length_spr<-coef(mod)
        names(coefs_weight_length_spr)<-c("a","b")
        coefs_weight_length_spr            
 
  res_len[res==1] <- len_pool_her_grad
  res_len[res==0] <- len_pool_spr_grad
  
  res_wt[res==1] <- exp(coefs_weight_length_her[1])*(res_len[res==1]^coefs_weight_length_her[2])
  res_wt[res==0] <- exp(coefs_weight_length_spr[1])*(res_len[res==0]^coefs_weight_length_spr[2])
  
  res_vol[res==1] <- res_wt[res==1] / 932.274568364 # density her = 932.274568364 gram/liter
  res_vol[res==0] <- res_wt[res==0] / 852.182251494 # density spr = 852.182251494 gram/liter

  res_cum_vol<-cumsum(res_vol)
  head(res_cum_vol) 
 
  #plot(res)
   
  target_vol <- 30
  
  res_box<-res_cum_vol%/%target_vol
  head(res_box)

  # number of boxes in haul
    max(res_box)
  
  # n_ind per box
    summary(c(table(res_box)))
    summary(c(table(res_box[res==0])))
    summary(c(table(res_box[res==1])))

# simulates n_samples systematic sampling [for proportions]
    
    # set number of samples
        n_samples <- 1
        max(res_box)%/%n_samples
    
    # random_start
        start_box <- sample(1:((max(res_box)%/%n_samples)-1), size=1)
        sampled_box<-seq(start_box,  max(res_box), by = max(res_box)%/%n_samples)

    # prop her in weight
        # total
        sum(res_wt[res==1])/sum(res_wt)
        # in sampled boxes
        sum(res_wt[res==1 & res_box %in% sampled_box])/sum(res_wt[res_box %in% sampled_box])
 
    # mean_length in catch
            mean(res_len[res==1])    
            mean(res_len[res==1 & res_box %in% sampled_box])    
            mean(res_len[res==0])    
            mean(res_len[res==0 & res_box %in% sampled_box])                
            
            
            
            
           # in first box
            sum(res_wt[res==1 & res_box %in% sampled_box[1]])/ sum(res_wt[res_box %in% sampled_box[1]])  
            # in middle box
            sum(res_wt[res==1 & res_box %in% sampled_box[n_samples%/%2] ])/ sum(res_wt[res_box %in% sampled_box[n_samples%/%2]])  
            sum(res_wt[res==0 & res_box %in% sampled_box[n_samples%/%2+1] ])/ sum(res_wt[res_box %in% sampled_box[n_samples%/%2+1]])  
             # in last box
            sum(res_wt[res==1 & res_box %in% sampled_box[n_samples] ])/ sum(res_wt[res_box %in% sampled_box[n_samples]])  
                
     