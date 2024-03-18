rm(list=ls())
library(sampling)


# =====================
# playing around with gradients (sample and  unequal probability approach
# =====================

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
s <- UPsampfordmaxentropy(pik)
plot(s)


# soft gradient
pik=inclusionprobabilities(1:N_haul,n_v0)
s <- UPmaxentropy(pik)
plot(s)
plot(s1)

# 50 sets of gradient combined [note: this does not work as expected]
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


# =====================
# generating 20 kgs/tons haul with a specific spp composition in weight
# =====================

# settings:
#haul_weight_g <- 20*10^6 # 20 tons
haul_weight_g <- 50000 # 20 kgs
prop_her_weight <- 0.59
prop_her_weight <- 0.7 # this gives approx same number of herring and sprat under th lf selected below

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
			flow <-rep(NA, haul_N)
			s <- sample(haul_N, size=haul_N_her, replace=F)
			flow[s]<-1
			flow[!(1:haul_N) %in% s]<-0
				# demo:
				table(flow)
				plot(flow); points(rep(0.5,haul_N), col=flow+1)

		# type 2: first herring then sprat
			flow <-rep(0, haul_N)
			flow[1:haul_N_her]<-1
				# demo:
				table(flow)
				plot(flow, col=flow+1); points(rep(0.5,haul_N), col=flow+1)
		
		# type 3a: gradient
			# To consider - what is the gradient hypothesis? needs to be informed by reality
							# is it based on density? if so, is it sprat first or herring first? 
							# or is it based on size or weight of individuals irrespective of species?
			
			# below one possible way to randomize the outcome. 
				# the result can then be applied to the hypothesis made: density, size, weight, etc
			grad<-(1/(1:haul_N))^0.5 # softer
			grad<-1/(1:haul_N) # soft
			grad<-(1/(1:haul_N))^2 # sharper
			grad<-(1/(1:haul_N))^5 # sharper
			grad<-(1:haul_N)	# slightly heterogeneous, herring last
			grad<-1/(1:haul_N)	# sharper
			flow <-rep(NA, haul_N)
			# it would be better to use UPsampford instead 
				# but is slow. Also note that even when prop=0.7 (i.e., when the no of sprat and herring is about the same)
						# seems blanks are happenning for herring and not so much for sprat - why? could be a problem with the sample function? 
							#(see more here https://stats.stackexchange.com/questions/639211/is-rs-weighted-sample-without-replacement-function-misleading)
			s <- sample(haul_N, size=haul_N_her, replace=F, prob=grad)
			flow[s]<-1
			flow[!(1:haul_N) %in% s]<-0
				# demo:
				table(flow)
				plot(flow, col=flow+1); points(rep(0.5,haul_N), col=flow+1)

		# type 3b: gradient sinusoidal
			f<-0.25
			grad<-sin((1:haul_N)/(f*haul_N)*2*pi)
			flow <-rep(NA, haul_N)
			s <- sample(1:haul_N, size=haul_N_her, replace=F, prob=(grad+1)/2) # see note above on unequal prob and sample
			flow[s]<-1
			flow[!(1:haul_N) %in% s]<-0
				# demo:
				table(flow)
				plot(flow, col=flow+1); points(rep(0.5,haul_N), col=flow+1); points((grad+1)/2, type="l")
			
		# type 3c: other types of heterogeneity
			# x segments/fish-holds with diferent species composition?



# ==============
# associate the individuals to the flow
# ==============	
	
		# type 1: randomized on length within species
			names(flow)<-flow
			names(flow)[flow==1]<-"her"
			names(flow)[flow==0]<-"spr"
			pop_len <- flow
			pop_len[names(pop_len)=="her"]<-sample(haul_lengths_her, size=haul_N_her, replace=F)
			pop_len[names(pop_len)=="spr"]<-sample(haul_lengths_spr, size=haul_N_spr, replace=F)
			
			pop_wt<-flow
			pop_wt[names(pop_wt)=="her"]<-exp(coefs_weight_length_her[1])*(pop_len[names(pop_len)=="her"]^coefs_weight_length_her[2])
			pop_wt[names(pop_wt)=="spr"]<-exp(coefs_weight_length_spr[1])*(pop_len[names(pop_len)=="spr"]^coefs_weight_length_spr[2])
			
			pop_vol<-flow
			pop_vol[names(pop_vol)=="her"]<-pop_wt[names(pop_wt)=="her"] / 932.274568364 # density her = 932.274568364 gram/liter
			pop_vol[names(pop_vol)=="spr"]<-pop_wt[names(pop_wt)=="spr"] / 852.182251494 # density spr = 852.182251494 gram/liter
			
			pop_vol_cum<-cumsum(pop_vol)
		
		# type 2: larger or smaller / denser or less denser / more or less volumous first
			# needs to be coded (gradient strategy proportional to variable could be one one)
	

# ==============
# from flow of fish (flow) to flow of buckets (flow_split_into_buckets)
# ==============	
	
head(pop_vol_cum) 

target_vol <- 0.25

flow_split_into_buckets<-pop_vol_cum%/%target_vol+1

    # number of buckets
	max(flow_split_into_buckets)
	# number of individuals per bucket
	table(flow_split_into_buckets, names(flow_split_into_buckets))
    table(names(flow_split_into_buckets))
	# proportion herring in bucket sequence
	plot(prop.table(table(flow_split_into_buckets, names(flow_split_into_buckets)),1)[,1])


# ==============	
# simulates n_samples systematic sampling [for proportions]
# ==============	
	# note: the following approximates simulating systematic sampling but size slight varies from objective due to edge effects
			# a better way exists - see wonderful in function (repo HaVonboardSmallPelagics)
    
    # set number of samples
        n_samples <- 30
        max(flow_split_into_buckets)%/%n_samples
    
    # random_start
        start_bucket <- sample(1:((max(flow_split_into_buckets)%/%n_samples)-1), size=1)
        sampled_buckets<-seq(start_bucket,  max(flow_split_into_buckets), by = max(flow_split_into_buckets)%/%n_samples)

    # prop her in weight
        # total
        sum(pop_wt[names(pop_wt)=="her"])/sum(pop_wt)
        # in sampled boxes
        sum(pop_wt[names(pop_wt)=="her" & flow_split_into_buckets %in% sampled_buckets])/sum(pop_wt[flow_split_into_buckets %in% sampled_buckets])
        
            # in first box
            sum(pop_wt[names(pop_wt)=="her" & flow_split_into_buckets %in% sampled_buckets[1]])/ sum(pop_wt[flow_split_into_buckets %in% sampled_buckets[1]])  
            # in middle box
            sum(pop_wt[names(pop_wt)=="her" & flow_split_into_buckets %in% sampled_buckets[n_samples%/%2] ])/ sum(pop_wt[flow_split_into_buckets %in% sampled_buckets[n_samples%/%2]])  
            sum(pop_wt[names(pop_wt)=="spr" & flow_split_into_buckets %in% sampled_buckets[n_samples%/%2] ])/ sum(pop_wt[flow_split_into_buckets %in% sampled_buckets[n_samples%/%2]])  
            sum(pop_wt[names(pop_wt)=="her" & flow_split_into_buckets %in% sampled_buckets[n_samples%/%2+1] ])/ sum(pop_wt[flow_split_into_buckets %in% sampled_buckets[n_samples%/%2+1]])  
            sum(pop_wt[names(pop_wt)=="spr" & flow_split_into_buckets %in% sampled_buckets[n_samples%/%2+1] ])/ sum(pop_wt[flow_split_into_buckets %in% sampled_buckets[n_samples%/%2+1]])  
             # in last box
            sum(pop_wt[names(pop_wt)=="her" & flow_split_into_buckets %in% sampled_buckets[n_samples] ])/ sum(pop_wt[flow_split_into_buckets %in% sampled_buckets[n_samples]])  
            sum(pop_wt[names(pop_wt)=="spr" & flow_split_into_buckets %in% sampled_buckets[n_samples] ])/ sum(pop_wt[flow_split_into_buckets %in% sampled_buckets[n_samples]])  
    
	# n indiv sampled
        length(pop_wt[names(pop_wt)=="her" & flow_split_into_buckets %in% sampled_buckets])
        length(pop_wt[names(pop_wt)=="spr" & flow_split_into_buckets %in% sampled_buckets])



# ==============	
# other
# ==============	

# density stuff

d = m / vol
d_her = 58.2 = 932.274568364 gram/liter
d_spr = 53.2 pound/cubic foot = 852.182251494 gram/liter
8/852.182251494 = 0.009387663 l 


