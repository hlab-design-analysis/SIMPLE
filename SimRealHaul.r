rm(list=ls())
library(sampling)

v1<-rep(1,100); v1
v2<-rep(2, 100); v2

# number of fish
N_haul<-1000
prop_v1<-0.59 # proportion of fish v1 type
n_v1 <- round(prop_v1*N_haul) # number of fish v1 type
n_v0 <- N_haul-n_v1	# number of fish v0 type

# random mix

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



# =====================
# generating 20 tons haul with a specific spp composition in weight
# =====================

# settings:
haul_weight_g <- 20*10^6 # 20 tons
prop_her_weight <- 0.59

target_weight_her<-prop_her_weight*haul_weight_g
target_weight_spr<-(1-prop_her_weight)*haul_weight_g

# generate a herring length frequency at haul level
    # read some data
        load ("001_Inputs_SimRealHaul/Input_data_her.27.25-29_8.Rdata")
   # select a length frequency
        lf_her <- prop.table(table(df0[df0$sampId=="2009_2113","lenCls"]))
    # calculate its weight-length relationship [can also use a standard - probably better that way]
        mod <- lm(log(df0$indWt)~log(df0$lenCls))
        coefs_weight_length_her<-coef(mod)
        names(coefs_weight_length_her)<-c("a","b")
        coefs_weight_length_her
	# calculate mean weight
		mean_weights_per_length_class_her<-exp(coefs_weight_length_her[1])*(as.numeric(names(lf_her))^coefs_weight_length_her[2])	
		mean_weight_her<-sum(mean_weights_per_length_class_her*lf_her)
	# calculate haul level lf and N
		haul_N_her <- target_weight_her/mean_weight_her
		haul_lf_her <- round(haul_N_her*lf_her)
		# check [% rounding error]
			# can be assumed negligible?
			(target_weight_her-sum(haul_lf_her*mean_weights_per_length_class_her))/target_weight_her*100
			(haul_N_her-sum(haul_lf_her))/haul_N_her*100
		# update
		haul_N_her<-sum(haul_lf_her) 
	# individual lengths her
		haul_lengths_her<-rep(as.numeric(names(haul_lf_her)),haul_lf_her)

# generate a sprat length frequency at haul level
    # read data
        load ("001_Inputs_SimRealHaul/Input_data_spr.27.22-32_8.Rdata")
   # select a length frequency
        lf_spr <- prop.table(table(df0[df0$sampId=="2009_2105","lenCls"]))
    # calculate its weight-length relationship [can also use a standard - probably better that way]
        mod <- lm(log(df0$indWt)~log(df0$lenCls))
        coefs_weight_length_spr<-coef(mod)
        names(coefs_weight_length_spr)<-c("a","b")
        coefs_weight_length_spr
	# calculate mean weight
		mean_weights_per_length_class_spr<-exp(coefs_weight_length_spr[1])*(as.numeric(names(lf_spr))^coefs_weight_length_spr[2])	
		mean_weight_spr<-sum(mean_weights_per_length_class_spr*lf_spr)
	# calculate haul level lf and N
		haul_N_spr <- target_weight_spr/mean_weight_spr
		haul_lf_spr <- round(haul_N_spr*lf_spr)
		# check [% rounding error]
			# can be assumed negligible?
			(target_weight_spr-sum(haul_lf_spr*mean_weights_per_length_class_spr))/target_weight_spr*100
			(haul_N_spr-sum(haul_lf_spr))/haul_N_spr*100
		# update
		haul_N_spr<-sum(haul_lf_spr) 
	# individual lengths her
		haul_lengths_spr<-rep(as.numeric(names(haul_lf_spr)),haul_lf_spr)

# final proportion
	# negligible error right?
	sum(haul_lf_her*mean_weights_per_length_class_her)/(sum(haul_lf_spr*mean_weights_per_length_class_spr)+sum(haul_lf_her*mean_weights_per_length_class_her))
	# update
	target_weight_her<-sum(haul_lf_her*mean_weights_per_length_class_her)
	target_weight_spr<-sum(haul_lf_spr*mean_weights_per_length_class_spr)
	haul_weight_g<-target_weight_her+target_weight_spr
	haul_N<-haul_N_her+haul_N_spr

# ==============
# generate a flow for the haul
# ==============
	# 1 == herring
	# 0 == sprat
	
		# type 1: fully randomized on species
			s <- sample(haul_N, size=haul_N, replace=F)
			s[s %in% 1:haul_N_spr]<-0
			s[s %in% (haul_N_spr+1):haul_N]<-1
				# demo:
				table(s)
		
		# type 2: gradient
			# thoughts - what is the gradient hypothesis? needs to be informed by reality
							# is it based on density? if so, is it sprat first or herring first? 
							# or is it based on size or weight of individuals irrespective of species?
			
			# below one possible way to randomize the outcome. 
				# the result can then be applied to the hypothesis made: density, size, weight, etc
			pik=inclusionprobabilities(1:haul_N,haul_N_her)
			# pik=inclusionprobabilities(1:1000,410)
			s1 <- UPmaxentropy(pik)
				# demo:
				table(s1)

# associate the individuals to the flow
		
		# type 1: fully randomized on length
			names(s)<-s
			names(s)[s==1]<-"her"
			names(s)[s==0]<-"spr"
			pop_len<-s
			pop_len[names(pop_len)=="her"]<-sample(haul_lengths_her, size=haul_N_her, replace=F)
			pop_len[names(pop_len)=="spr"]<-sample(haul_lengths_spr, size=haul_N_spr, replace=F)
			
			pop_wt<-s
			pop_wt[names(pop_wt)=="her"]<-exp(coefs_weight_length_her[1])*(pop_len[names(pop_len)=="her"]^coefs_weight_length_her[2])
			pop_wt[names(pop_wt)=="spr"]<-exp(coefs_weight_length_spr[1])*(pop_len[names(pop_len)=="spr"]^coefs_weight_length_spr[2])
			
			pop_vol<-s
			pop_vol[names(pop_vol)=="her"]<-pop_wt[names(pop_wt)=="her"] / 932.274568364 # density her = 932.274568364 gram/liter
			pop_vol[names(pop_vol)=="spr"]<-pop_wt[names(pop_wt)=="spr"] / 852.182251494 # density spr = 852.182251494 gram/liter
			
			pop_vol_cum<-cumsum(pop_vol)
		
		# type 2: larger or smaller first
			# to be coded: 
				# rationale: 
					# alternatives are equivalent - just need to code one of them and invert the flow to get the other
					# if sampling is systematic there is no point in simulating both: only one of them is needed
			# different types of gradients are possible
	
	
	
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



