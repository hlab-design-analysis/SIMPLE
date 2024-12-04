do_systematic_samples_N_n<-function(N,n){
  
  # 2021-04-10 Nuno Prista @SLU, Sweden
  
  # Also known as "The wonderful function"
  # finally completed one stormy friday, April 09 2021. Jose Arcadio Buendia would be proud.
  # quite a bit of deduction used in producing it, lots of trial and error, searches in google,
  # and implementation of solutions from others that later were revealed incomplete (did not function for all N and n tested)
  # from all those, only the idea of k=ceiling(N/n) [props to # https://www.statology.org/systematic-sampling-r/ for that]
  # There is an accompanying excel file that details some of the examples	and can be used to check further improvements
  
  samples=c()
  
  # determines k
  k=ceiling(N/n)
  
  if(N/n >= 1.5)
  {
    if(N <= n*k)
    {
      j=0
      for(i in 1:k)
      {
        if(!any(i+((1:n)-1)*k>N))
        {
          #print(i+((1:n)-1)*k) 
          samples <- rbind(samples, c(i+((1:n)-1)*k))
          j=j+1
        }
      }	
      if(j>0) 
      {
        return(samples)
      } else {
        k<-k-1
        for(i in 1:(n-1)) 
        {	
          if(!any(i+((1:n)-1)*k>N)) 
          {
            #print(i+((1:n)-1)*k)
            samples <- rbind(samples, c(i+((1:n)-1)*k))
          }
          if(i==(n-1)) return(samples)
        }
      }			
    }	
  }	else { #se N/n<1.5
    k=1
    for(i in 1:(N-n+1))
    {	
      #print("c")
      #print(i+((1:n)-1)*k)
      samples <- rbind(samples, c((i+((1:n)-1)*k)))
    }
    return(samples)
  }
}

# example
#do_systematic_samples_N_n (N=14, n=6)