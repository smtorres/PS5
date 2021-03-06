#######################
#Problem Set 5        #
#Carlson, Park, Torres#
#Mar 4, 2014          # 
#######################


#####################
#Simulation Activity#
#####################

rm(list=ls())

##### PART 1. SIMULATION SETUP #####

### 1. and 2. Write a function to create a matrix of voters ###

voters.normal<-function(n, mu=0, sigma1=1, sigma2=1){  # As default, we set mu=0, sigma1=1, sigma2=1. "n" indicates the number of voters.
  d1<-rnorm(n, mu, sd=sigma1) # dimension 1
  d2<-rnorm(n, mu, sd=sigma2) # dimension 2
  mat<-cbind(d1, d2) 
  return(mat)  
}

voters.uniform<-function(n, a=0, b=1){ # a=0, b=1 are default.
  d1<-runif(n, a, b)
  d2<-runif(n, a, b)
  mat<-cbind(d1, d2)
  return(mat)    
}

voters.multi<-function(n, Mu, Sigma){ # Mu is a vector giving the means of the variables; Sigma is a positive-definite symmetric matrix specifying the covariance matrix of the variables
  require(MASS)                       # An example: Mu=c(1,2); Sigma=matrix(c(1,0.5,0.5,2), nrow=2, byrow=T)
  mat<-mvrnorm(n, Mu, Sigma)
  colnames(mat)<-c("d1", "d2")
  return(mat)    
}

voters.multi2<-function(n, Mu1, Mu2, Mu3, Sigma, r=3){ # r indicates the number of multivariate normal distributions from which preferences are drawn 
  require(MASS)
  if(r==2){
    mat1<-mvrnorm(n%/%2, Mu1, Sigma)
    mat2<-mvrnorm(n-n%/%2, Mu2, Sigma)
    mat.voters<-rbind(mat1, mat2)
  }
  if(r==3){
    mat1<-mvrnorm(n%/%3, Mu1, Sigma)
    mat2<-mvrnorm(n%/%3, Mu2, Sigma)
    mat3<-mvrnorm(n-2*(n%/%3),Mu3,Sigma)
    mat.voters<-rbind(mat1,mat2,mat3)
  }
  colnames(mat.voters)<-c("d1", "d2")
  return(mat.voters)
}


call.voters<-function(n, mu=0, Mu, Mu1, Mu2, Mu3, r=3, sigma1=1, sigma2=1, Sigma, a=0, b=1, method="normal"){
  if(method=="normal"){
    mat.voters<-voters.normal(n, mu, sigma1, sigma2)
  } 
  if (method=="snormal"){
    mat.voters<-voters.normal(n)
  }
  if (method=="uniform"){
    mat.voters<-voters.uniform(n, a, b)
  }
  if (method=="multivariate"){
    mat.voters<-voters.multi(n, Mu, Sigma)
  }
  if (method=="mixmulti"){
    mat.voters<-voters.multi2(n, Mu1, Mu2, Mu3, Sigma, r)
  }
  colnames(mat.voters)<-c("Dimension 1", "Dimension 2")
  return(mat.voters)
}



### 3. Write a function such that voters affiliate with the closest of the two parties ###

distance<-function(voters,parties){
  require(pdist)
  mat.distance<-as.matrix(pdist(voters, parties))  ##matrix of distances from voter to party - rows are voters, columns are parties
  return(as.numeric(mat.distance[,1]>mat.distance[,2]))  ##returns a vector of 0's for voters closer to party 1, and 1 for voters closer to party 2
}

#This is an example for the distance function
#set.seed(1234)
#voters <- call.voters(n=10) # from a normal distribution
#parties <- matrix(rnorm(4), 2, 2) # the row indicates two parties; the column indicates two dimensions
#distance(voters, parties)



### 4. A function for visualization ###

visualize<-function(voters,parties){
  affiliate<-distance(voters,parties)  ##returns a vector with 0's indicating affiliation with party 1
  plot(voters[,1],voters[,2],col=ifelse(affiliate,"red","blue"),pch=20)  ##plot voters - affiliation with party 1 is blue, party 2 is red
  points(parties[,1],parties[,2],col="black",bg=c("blue","red"),pch=23,cex=2,lwd=2)  ##plot parties as diamonds - party 1 is blue, 2 is red
  abline(h=0)
  abline(v=0)
}



##### PART 2. GET THINGS MOVING #####

## The relocate and master functions below cover all exercises required for PART 2.

relocate<-function(voters,parties){
  affiliate<-distance(voters,parties)  ##returns a vector with 0's indicating affiliation with party 1
  voters.party1<-voters[affiliate==0,]  ##matrix of voters affiliating with party 1
  voters.party2<-voters[affiliate==1,]  ##matrix of voters affiliating with party 2
  newparty1<-c(mean(voters.party1[,1]),mean(voters.party1[,2])) ##reassigns party 1 to mean of supporters along both dimensions
  newparty2<-c(mean(voters.party2[,1]),mean(voters.party2[,2])) ##reassigns party 1 to mean of supporters along both dimensions
  return(matrix(c(newparty1,newparty2),byrow=TRUE,nrow=2))  ##return matrix of new party - row 1 corresponding to party 1, row 2 to party 2
}


master<-function(iter=1000,n=1000, mu=0, Mu=c(0,0), Mu1=c(0,0), Mu2=c(0,0), Mu3=c(0,0), r=3, sigma1=1, sigma2=1, Sigma=matrix(c(1,0,0,1),nrow=2), a=0, b=1, method="normal",seed=.Random.seed[1]){
  set.seed(seed)
  voters<-call.voters(n, mu, Mu, Mu1, Mu2, Mu3, r, sigma1, sigma2, Sigma, a, b, method)  ##sets up random voters with specified method and parameters
  parties<-call.voters(2, mu, Mu, Mu1, Mu2, Mu3, r, sigma1, sigma2, Sigma, a, b, method)  ##sets up 2 random parties with specified method and parameters
  require(animation)
  out.mat1<-matrix(ncol=2,nrow=iter)  ##matrix for party 1's position at each iteration
  out.mat2<-matrix(ncol=2,nrow=iter)  ##matrix for party 2's positions
  if(iter>15){
    saveLatex(expr=   ##creates animation of first 15 iterations and creates a pdf
                for(i in 1:15){
                  out.mat1[i,]<-parties[1,]  ##assigns i-th row of output matrix for party 1 the i-th party position
                  out.mat2[i,]<-parties[2,]  ##assigns i-th row of output matrix for party 2 the i-th party position
                  affiliate<-distance(voters,parties)  ##returns a vector with 0's indicating affiliation with party 1
                  visualize(voters,parties)  ##visualize iterations in animation
                  parties<-relocate(voters,parties) ##reassign parties to means of voters that supported them
                },img.name="Rplot",overwrite=TRUE)
    
    for(k in 16:iter){  ##continues simulation for remaining iterations
      out.mat1[k,]<-parties[1,]
      out.mat2[k,]<-parties[2,]
      affiliate<-distance(voters,parties)  ##returns a vector with 0's indicating affiliation with party 1
      parties<-relocate(voters,parties) ##reassign parties to means of voters that supported them
    }
  }else{
    saveLatex(expr=   ##creates animation of all iterations and creates a pdf
                for(i in 1:iter){
                  out.mat1[i,]<-parties[1,]  ##assigns i-th row of output matrix for party 1 the i-th party position
                  out.mat2[i,]<-parties[2,]  ##assigns i-th row of output matrix for party 2 the i-th party position
                  affiliate<-distance(voters,parties)  ##returns a vector with 0's indicating affiliation with party 1
                  visualize(voters,parties)  ##visualize iterations in animation
                  parties<-relocate(voters,parties) ##reassign parties to means of voters that supported them
                },img.name="Rplot",overwrite=TRUE)
  }
  return(list(out.mat1,out.mat2))  ##return party positions as list. First element is matrix of party 1's positions, second element is matrix of party 2's
}



##### PART 3. EXPLORE THE MODEL #####

## To do the first two excercises we can use the master function we created in PART 2.
## As seen below, we may change the default values for the number of iterations, the paramerters (sigma1 and sigma2), and the random seed.
## There are indefinitely many ways of altering the function. 

master<-function(iter=1500,n=1000, mu=0, Mu=c(0,0), Mu1=c(0,0), Mu2=c(0,0), Mu3=c(0,0), r=3, sigma1=2, sigma2=2, Sigma=matrix(c(1,0,0,1),nrow=2), a=0, b=1, method="normal",seed=.Random.seed[2]){
  set.seed(seed)
  voters<-call.voters(n, mu, Mu, Mu1, Mu2, Mu3, r, sigma1, sigma2, Sigma, a, b, method)  ##sets up random voters with specified method and parameters
  parties<-call.voters(2, mu, Mu, Mu1, Mu2, Mu3, r, sigma1, sigma2, Sigma, a, b, method)  ##sets up 2 random parties with specified method and parameters
  require(animation)
  out.mat1<-matrix(ncol=2,nrow=iter)  ##matrix for party 1's position at each iteration
  out.mat2<-matrix(ncol=2,nrow=iter)  ##matrix for party 2's positions
  if(iter>15){
    saveLatex(expr=   ##creates animation of first 15 iterations and creates a pdf
                for(i in 1:15){
                  out.mat1[i,]<-parties[1,]  ##assigns i-th row of output matrix for party 1 the i-th party position
                  out.mat2[i,]<-parties[2,]  ##assigns i-th row of output matrix for party 2 the i-th party position
                  affiliate<-distance(voters,parties)  ##returns a vector with 0's indicating affiliation with party 1
                  visualize(voters,parties)  ##visualize iterations in animation
                  parties<-relocate(voters,parties) ##reassign parties to means of voters that supported them
                },img.name="Rplot",overwrite=TRUE)
    
    for(k in 16:iter){  ##continues simulation for remaining iterations
      out.mat1[k,]<-parties[1,]
      out.mat2[k,]<-parties[2,]
      affiliate<-distance(voters,parties)  ##returns a vector with 0's indicating affiliation with party 1
      parties<-relocate(voters,parties) ##reassign parties to means of voters that supported them
    }
  }else{
    saveLatex(expr=   ##creates animation of all iterations and creates a pdf
                for(i in 1:iter){
                  out.mat1[i,]<-parties[1,]  ##assigns i-th row of output matrix for party 1 the i-th party position
                  out.mat2[i,]<-parties[2,]  ##assigns i-th row of output matrix for party 2 the i-th party position
                  affiliate<-distance(voters,parties)  ##returns a vector with 0's indicating affiliation with party 1
                  visualize(voters,parties)  ##visualize iterations in animation
                  parties<-relocate(voters,parties) ##reassign parties to means of voters that supported them
                },img.name="Rplot",overwrite=TRUE)
  }
  return(list(out.mat1,out.mat2))  ##return party positions as list. First element is matrix of party 1's positions, second element is matrix of party 2's
}


### 3. Use the expand.grid() function to set up a data frame of possible parameters to explore ###

## Since there are indefinitely many possible parameters to explore the master function,
## we limit our cases to "normal" or "uniform", and create a data frame that accommodates different values for "mu", "sigma1", "sigma2", "a", and "b". 

## Without a loss of generality, we make each parameter have two possible values.
## To increase the number of possible values, we can increase "length.out" below.
mu<-seq(0,1,length.out=2)
sigma1<-seq(1,1.5, length.out=2)
sigma2<-seq(1,2, length.out=2) 
a<-seq(0,1, length.out=2)
b<-seq(2,3, length.out=2)
method <- c("normal", "uniform")
parameters<-data.frame(expand.grid(mu, sigma1, sigma2, a, b, method)) # 64 observations and for 6 variables
colnames(parameters)<-c("mu", "sigma1", "sigma2", "a", "b", "method")


## For this particular problem, we omit the visualization part of the function, since the function would produce 64 plots in this particular case, which are not required for this problem.   
masterParameters<-function(iter=15, n=100, mu, Mu=c(0,0), Mu1=c(0,0), Mu2=c(0,0), Mu3=c(0,0), r=3, sigma1, sigma2, Sigma=matrix(c(1,0,0,1),nrow=2), a, b, method, seed=.Random.seed[2]){
  set.seed(seed)
  output<-list()
  for(j in 1:nrow(parameters)){
    voters<-call.voters(n, Mu, Mu1, Mu2, Mu3, r, Sigma, 
                        mu=parameters[j,1], sigma1=parameters[j,2],
                        sigma2=parameters[j,3], a=parameters[j,4],
                        b=parameters[j,5], method=parameters[j,6])  ##sets up random voters with specified method and parameters
    parties<-call.voters(2, Mu, Mu1, Mu2, Mu3, r, Sigma, 
                         mu=parameters[j,1], sigma1=parameters[j,2],
                         sigma2=parameters[j,3], a=parameters[j,4],
                         b=parameters[j,5], method=parameters[j,6])  ##sets up 2 random parties with specified method and parameters
    out.mat1<-matrix(ncol=2, nrow=iter)  ##matrix for party 1's position at each iteration
    out.mat2<-matrix(ncol=2, nrow=iter)  ##matrix for party 2's positions
    if(iter>15){
      for(i in 1:15){
        out.mat1[i,]<-parties[1,]  ##assigns i-th row of output matrix for party 1 the i-th party position
        out.mat2[i,]<-parties[2,]  ##assigns i-th row of output matrix for party 2 the i-th party position
        affiliate<-distance(voters,parties)  ##returns a vector with 0's indicating affiliation with party 1
        parties<-relocate(voters,parties) ##reassign parties to means of voters that supported them
      }
      
      for(k in 16:iter){  ##continues simulation for remaining iterations
        out.mat1[k,]<-parties[1,]
        out.mat2[k,]<-parties[2,]
        affiliate<-distance(voters,parties)  ##returns a vector with 0's indicating affiliation with party 1
        parties<-relocate(voters,parties) ##reassign parties to means of voters that supported them
      }
    }else{  for(i in 1:iter){
      out.mat1[i,]<-parties[1,]  ##assigns i-th row of output matrix for party 1 the i-th party position
      out.mat2[i,]<-parties[2,]  ##assigns i-th row of output matrix for party 2 the i-th party position
      affiliate<-distance(voters,parties)  ##returns a vector with 0's indicating affiliation with party 1
      parties<-relocate(voters,parties) ##reassign parties to means of voters that supported them
    }         
    }
    output[[j]]<-list(out.mat1,out.mat2)  ##return party positions as list. First element is matrix of party 1's positions, second element is matrix of party 2's
  }
  return(output)
}
## You may run masterParameters() to see if this function works. It will return 64 lists. And under each list, there will be two lists.
## CAUTION: when n is not large enough, this function may break down, since every voter could be closer to one particular party.
## But, we found that if n is large enough (say 100), it would be extremely rare (almost zero possibility) to have this kind of situation. 


### 4. Use a plot to characterize some comparative static of interest ###

## To do this exercise, we can modify the master function to let the output be given by the visualize function.
## You will find that the visualize function runs in the end of masterCompare 1 and masterCompare2 functions below.
## Let's deal with a simple case where we fix method="normal" 
## The comparative static of our interest is to compare different plots resulted from different sets of sigma2.
## That is, we will make the following two cases be the same only except that 
## under case 1 (masterCompare1), voters on the second dimension are drawn from N(0,1), while under case 2 (masterCompare2), voters on the second dimension are drawn from N(0,15^2).
## On the other hand, voters on the first dimension are drawn from the same distribution between (masterCompare1) and (masterCompare1).

masterCompare1<-function(iter=150,n=1000, mu=0, Mu=c(0,0), Mu1=c(0,0), Mu2=c(0,0), Mu3=c(0,0), r=3, sigma1=1, sigma2=1, Sigma=matrix(c(1,0,0,1),nrow=2), a=0, b=1, method="normal",seed=.Random.seed[1]){
  set.seed(seed)
  voters<-call.voters(n, mu, Mu, Mu1, Mu2, Mu3, r, sigma1, sigma2, Sigma, a, b, method)  ##sets up random voters with specified method and parameters
  parties<-call.voters(2, mu, Mu, Mu1, Mu2, Mu3, r, sigma1, sigma2, Sigma, a, b, method)  ##sets up 2 random parties with specified method and parameters
  out.mat1<-matrix(ncol=2,nrow=iter)  ##matrix for party 1's position at each iteration
  out.mat2<-matrix(ncol=2,nrow=iter)  ##matrix for party 2's positions
  if(iter>15){
    for(i in 1:15){
      out.mat1[i,]<-parties[1,]  ##assigns i-th row of output matrix for party 1 the i-th party position
      out.mat2[i,]<-parties[2,]  ##assigns i-th row of output matrix for party 2 the i-th party position
      affiliate<-distance(voters,parties)  ##returns a vector with 0's indicating affiliation with party 1
      parties<-relocate(voters,parties) ##reassign parties to means of voters that supported them
    }
    for(k in 16:iter){  ##continues simulation for remaining iterations
      out.mat1[k,]<-parties[1,]
      out.mat2[k,]<-parties[2,]
      affiliate<-distance(voters,parties)  ##returns a vector with 0's indicating affiliation with party 1
      parties<-relocate(voters,parties) ##reassign parties to means of voters that supported them
    }
  }else{
    for(i in 1:iter){
      out.mat1[i,]<-parties[1,]  ##assigns i-th row of output matrix for party 1 the i-th party position
      out.mat2[i,]<-parties[2,]  ##assigns i-th row of output matrix for party 2 the i-th party position
      affiliate<-distance(voters,parties)  ##returns a vector with 0's indicating affiliation with party 1
      parties<-relocate(voters,parties) ##reassign parties to means of voters that supported them
    }
  }
  output<-list(out.mat1,out.mat2)
  visualize(voters,parties=rbind(output[[1]][iter,], output[[2]][iter,]))
}

masterCompare2<-function(iter=150,n=1000, mu=0, Mu=c(0,0), Mu1=c(0,0), Mu2=c(0,0), Mu3=c(0,0), r=3, sigma1=1, sigma2=15, Sigma=matrix(c(1,0,0,1),nrow=2), a=0, b=1, method="normal",seed=.Random.seed[1]){
  set.seed(seed)
  voters<-call.voters(n, mu, Mu, Mu1, Mu2, Mu3, r, sigma1, sigma2, Sigma, a, b, method)  ##sets up random voters with specified method and parameters
  parties<-call.voters(2, mu, Mu, Mu1, Mu2, Mu3, r, sigma1, sigma2, Sigma, a, b, method)  ##sets up 2 random parties with specified method and parameters
  out.mat1<-matrix(ncol=2,nrow=iter)  ##matrix for party 1's position at each iteration
  out.mat2<-matrix(ncol=2,nrow=iter)  ##matrix for party 2's positions
  if(iter>15){
    for(i in 1:15){
      out.mat1[i,]<-parties[1,]  ##assigns i-th row of output matrix for party 1 the i-th party position
      out.mat2[i,]<-parties[2,]  ##assigns i-th row of output matrix for party 2 the i-th party position
      affiliate<-distance(voters,parties)  ##returns a vector with 0's indicating affiliation with party 1
      parties<-relocate(voters,parties) ##reassign parties to means of voters that supported them
    }
    for(k in 16:iter){  ##continues simulation for remaining iterations
      out.mat1[k,]<-parties[1,]
      out.mat2[k,]<-parties[2,]
      affiliate<-distance(voters,parties)  ##returns a vector with 0's indicating affiliation with party 1
      parties<-relocate(voters,parties) ##reassign parties to means of voters that supported them
    }
  }else{
    for(i in 1:iter){
      out.mat1[i,]<-parties[1,]  ##assigns i-th row of output matrix for party 1 the i-th party position
      out.mat2[i,]<-parties[2,]  ##assigns i-th row of output matrix for party 2 the i-th party position
      affiliate<-distance(voters,parties)  ##returns a vector with 0's indicating affiliation with party 1
      parties<-relocate(voters,parties) ##reassign parties to means of voters that supported them
    }
  }
  output<-list(out.mat1,out.mat2)
  visualize(voters,parties=rbind(output[[1]][iter,], output[[2]][iter,]))
}

par(mfrow=c(1,2))
masterCompare1()
masterCompare2()


####EXPAND YOUR MODEL

#1. Allow for more than 2 parties ("npar", default=2)
master.multi<-function(iter=1500,n=1000, mu=0, Mu=c(0,0), Mu1=c(0,0), Mu2=c(0,0), 
                 Mu3=c(0,0), r=3, sigma1=2, sigma2=2, Sigma=matrix(c(1,0,0,1),nrow=2), 
                 a=0, b=1, method="normal",seed=.Random.seed[2], npar=2){ ##NPAR=Number of parties
  set.seed(seed)
  voters<-call.voters(n, mu, Mu, Mu1, Mu2, Mu3, r, sigma1, sigma2, Sigma, a, b, method)  ##sets up random voters with specified method and parameters
  
  #wITH DIFFERENT NUMBER OF PARTIES 
  parties<-call.voters(npar, mu, Mu, Mu1, Mu2, Mu3, r, sigma1, sigma2, Sigma, a, b, method)  ##sets up 2 random parties with specified method and parameters
  require(animation)

  ##DISTANCE WITH MULTI PARTIES
  distance.multi<-function(voters,parties){
    require(pdist)
    mat.distance<-as.matrix(pdist(voters, parties))  ##matrix of distances from voter to party - rows are voters, columns are parties
    part.names<-as.character(1:nrow(parties))
    colnames(mat.distance)<-part.names
    return(as.numeric(colnames(mat.distance)[apply(mat.distance,1,which.min)]))}
  
  ###VISUALIZE
  visualize.multi<-function(voters,parties){
    #Function to determine affiliation
    affiliate<-distance.multi(voters,parties)  ##returns a vector with the number indicating affiliation
    # Plot voters
    col.pal<-rainbow(nrow(parties))
    palette(col.pal)
    plot(voters[,1],voters[,2],col=affiliate,pch=20, ,xlim=c(min(voters[,1])-1,max(voters[,1])+1 ), ylim=c(min(voters[,2])-1,max(voters[,2])+1 ))  
    points(parties[,1],parties[,2],col="black",bg=1:nrow(parties),pch=23,cex=2,lwd=2)  ##plot parties as diamonds - party 1 is blue, 2 is red
    abline(h=0)
    abline(v=0)
  }
  
  ##RELOCATE
  relocate.multi<-function(voters,parties){
    affiliate<-distance.multi(voters,parties)  ##returns a vector with 0's indicating affiliation with party 1
    voters.parties<-new.parties<-list()
    for (i in 1:npar) {
      voters.parties[[i]]<-assign(paste("voters.party",i,sep=""), as.matrix(voters[affiliate==i,]))
      new.parties[[i]]<-assign(paste("newparty",i,sep=""), c(mean(voters.parties[[i]][,1]),
                                                             mean(voters.parties[[i]][,2])))
    }
    return(matrix(unlist(new.parties),byrow=TRUE,ncol=2))  ##return matrix of new party - row 1 corresponding to party 1, row 2 to party 2
  }
  
  ####ITERATIONS
out.mats.list<-list()
for (i in 1:npar){
  out.mats.list[[i]]<-assign(paste("out.mat",i,sep=""), matrix(ncol=2, nrow=iter))
}
  if(iter>15){
    saveLatex(expr=   ##creates animation of first 15 iterations and creates a pdf
                for(i in 1:15){
                  for(j in 1:npar){
                  out.mats.list[[j]][i,]<-parties[j,]}
                  affiliate<-distance.multi(voters,parties)  ##returns a vector with 0's indicating affiliation with party 1
                  visualize.multi(voters,parties)  ##visualize iterations in animation
                  parties<-relocate.multi(voters,parties) ##reassign parties to means of voters that supported them
                },img.name="Rplot",overwrite=TRUE)
    
    for(k in 16:iter){  ##continues simulation for remaining iterations
      for(j in 1:npar){
      out.mats.list[[j]][k,]<-parties[j,]}
      affiliate<-distance.multi(voters,parties)  ##returns a vector with 0's indicating affiliation with party 1
      parties<-relocate.multi(voters,parties) ##reassign parties to means of voters that supported them
    }
  }else{
    saveLatex(expr=   ##creates animation of all iterations and creates a pdf
                for(l in 1:iter){
                  for (j in 1:npar){
                  out.mats.list[[j]][l,]<-parties[j,]}
                  affiliate<-distance.multi(voters,parties)  ##returns a vector with 0's indicating affiliation with party 1
                  visualize.multi(voters,parties)  ##visualize iterations in animation
                  parties<-relocate.multi(voters,parties) ##reassign parties to means of voters that supported them
                },img.name="Rplot",overwrite=TRUE)
  }
return(out.mats.list)  ##return party positions as list. First element is matrix of party 1's positions, second element is matrix of party 2's
}

#2. Alter your model so that voters vote \probabilistically" as some function of the
#distance between the two parties. (That is, allow them to make the \wrong" decision
#if they are nearly indifferent between the parties.)

master.multi.prob<-function(iter=1500,n=1000, mu=0, Mu=c(0,0), Mu1=c(0,0), Mu2=c(0,0), 
                       Mu3=c(0,0), r=3, sigma1=2, sigma2=2, Sigma=matrix(c(1,0,0,1),nrow=2), 
                       a=0, b=1, method="normal",seed=.Random.seed[2], npar=2){ ##NPAR=Number of parties
  set.seed(seed)
  voters<-call.voters(n, mu, Mu, Mu1, Mu2, Mu3, r, sigma1, sigma2, Sigma, a, b, method)  ##sets up random voters with specified method and parameters
  
  #wITH DIFFERENT NUMBER OF PARTIES 
  parties<-call.voters(npar, mu, Mu, Mu1, Mu2, Mu3, r, sigma1, sigma2, Sigma, a, b, method)  ##sets up 2 random parties with specified method and parameters
  require(animation)
  
  ##DISTANCE WITH MULTI PARTIES PROBABILISTIC
  distance.multi.prob<-function(voters,parties){
    require(pdist)
    mat.distance<-as.matrix(pdist(voters, parties))  ##matrix of distances from voter to party - rows are voters, columns are parties
    part.names<-as.character(1:nrow(parties))
    colnames(mat.distance)<-part.names
    min.diff<-function(x){  ##FUNCTION to find the differences between the two lowest values
      y<-sort(x)    #sort a vector from min to max
      diff<-abs(y[1]-y[2])   #difference between lowest and second lowest distance
      if (diff<=0.5){        #Criteria of indifference: difference less than 0.5
        selec<-sample(c(y[1],y[2]),1)  #Voter voting wrong (voting for his second best option)
      }
      if(diff>0.5){          #Voting correctly if difference > 0.5
        selec<-y[1]
      }
      return(selec)
    }
    diffs<-cbind(mat.distance,apply(mat.distance, 1, min.diff))  #matrix with the "selected" distance
    affil<-as.numeric(colnames(diffs)[apply(diffs,1,FUN=function(x) which(x[1:nrow(parties)]==x[nrow(parties)+1]))])
    return(affil)
  }
  
  ###VISUALIZE
  visualize.multi<-function(voters,parties){
    #Function to determine affiliation
    affiliate<-distance.multi.prob(voters,parties)  ##returns a vector with the number indicating affiliation
    # Plot voters
    col.pal<-rainbow(nrow(parties))
    palette(col.pal)
    plot(voters[,1],voters[,2],col=affiliate,pch=20, ,xlim=c(min(voters[,1])-1,max(voters[,1])+1 ), ylim=c(min(voters[,2])-1,max(voters[,2])+1 ))  
    points(parties[,1],parties[,2],col="black",bg=1:nrow(parties),pch=23,cex=2,lwd=2)  ##plot parties as diamonds - party 1 is blue, 2 is red
    abline(h=0)
    abline(v=0)
  }
  
  ##RELOCATE
  relocate.multi<-function(voters,parties){
    affiliate<-distance.multi.prob(voters,parties)  ##returns a vector with 0's indicating affiliation with party 1
    voters.parties<-new.parties<-list()
    for (i in 1:npar) {
      voters.parties[[i]]<-assign(paste("voters.party",i,sep=""), as.matrix(voters[affiliate==i,]))
      new.parties[[i]]<-assign(paste("newparty",i,sep=""), c(mean(voters.parties[[i]][,1]),
                                                             mean(voters.parties[[i]][,2])))
    }
    return(matrix(unlist(new.parties),byrow=TRUE,ncol=2))  ##return matrix of new party - row 1 corresponding to party 1, row 2 to party 2
  }
  
  ####ITERATIONS
  out.mats.list<-list()
  for (i in 1:npar){
    out.mats.list[[i]]<-assign(paste("out.mat",i,sep=""), matrix(ncol=2, nrow=iter))
  }
  if(iter>15){
    saveLatex(expr=   ##creates animation of first 15 iterations and creates a pdf
                for(i in 1:15){
                  for(j in 1:npar){
                    out.mats.list[[j]][i,]<-parties[j,]}
                  affiliate<-distance.multi.prob(voters,parties)  ##returns a vector with 0's indicating affiliation with party 1
                  visualize.multi(voters,parties)  ##visualize iterations in animation
                  parties<-relocate.multi(voters,parties) ##reassign parties to means of voters that supported them
                },img.name="Rplot",overwrite=TRUE)
    
    for(k in 16:iter){  ##continues simulation for remaining iterations
      for(j in 1:npar){
        out.mats.list[[j]][k,]<-parties[j,]}
      affiliate<-distance.multi.prob(voters,parties)  ##returns a vector with 0's indicating affiliation with party 1
      parties<-relocate.multi(voters,parties) ##reassign parties to means of voters that supported them
    }
  }else{
    saveLatex(expr=   ##creates animation of all iterations and creates a pdf
                for(l in 1:iter){
                  for (j in 1:npar){
                    out.mats.list[[j]][l,]<-parties[j,]}
                  affiliate<-distance.multi.prob(voters,parties)  ##returns a vector with 0's indicating affiliation with party 1
                  visualize.multi(voters,parties)  ##visualize iterations in animation
                  parties<-relocate.multi(voters,parties) ##reassign parties to means of voters that supported them
                },img.name="Rplot",overwrite=TRUE)
  }
  return(out.mats.list)  ##return party positions as list. First element is matrix of party 1's positions, second element is matrix of party 2's
}

#4. Laver (2005) (\Policy and dynamics of political competition") explores several addi-
#tional heuristics parties might use to choose their position. Add at least one heuristic
#to the model (i.e, the party heuristic chosen should be a parameter for each party).
#How does that change the behavior of the model?

###PREDATOR
master.heuristics<-function(iter=1500,n=1000, mu=0, Mu=c(0,0), Mu1=c(0,0), Mu2=c(0,0), 
                            Mu3=c(0,0), r=3, sigma1=2, sigma2=2, Sigma=matrix(c(1,0,0,1),nrow=2), 
                            a=0, b=1, method="normal",seed=.Random.seed[2], npar=2, heur="aggregate"){
#iter<-iter
  if(heur=="aggregate"){
  master.multi.prob(iter=iter,n=n, mu=mu, Mu=Mu, Mu1=Mu1, Mu2=Mu2, 
                    Mu3=Mu3, r=r, sigma1=sigma1, sigma2=sigma2, Sigma=Sigma, 
                    a=a, b=b, method=method,seed=seed, npar=npar)
}
if(heur=="predator"){
  set.seed(seed)
  voters<-call.voters(n, mu, Mu, Mu1, Mu2, Mu3, r, sigma1, sigma2, Sigma, a, b, method)  ##sets up random voters with specified method and parameters
  
  #wITH DIFFERENT NUMBER OF PARTIES 
  parties<-call.voters(npar, mu, Mu, Mu1, Mu2, Mu3, r, sigma1, sigma2, Sigma, a, b, method)  ##sets up 2 random parties with specified method and parameters
  require(animation)
  
  ##Distance: Initial position of parties and voters
  #### Distance with multiple parties and probabilisti voting
  distance.multi.prob<-function(voters,parties){
    require(pdist)
    mat.distance<-as.matrix(pdist(voters, parties))  ##matrix of distances from voter to party - rows are voters, columns are parties
    part.names<-as.character(1:nrow(parties))
    colnames(mat.distance)<-part.names
    min.diff<-function(x){  ##FUNCTION to find the differences between the two lowest values
      y<-sort(x)              #sort a vector from min to max
      diff<-abs(y[1]-y[2])    #difference between lowest and second lowest distance
      if (diff<=0.5){         #Criteria of indifference: difference less than 0.5
        selec<-sample(c(y[1],y[2]),1)   #Voter voting wrong (voting randomly from his two best options)
      }
      if(diff>0.5){                 #Voting correctly if difference > 0.5
        selec<-y[1]
      }
      return(selec)
    }
    diffs<-cbind(mat.distance,apply(mat.distance, 1, min.diff))  #matrix with the "selected" distance
    #Name of the party selected 
    library(plyr)
    affil<-as.numeric(colnames(diffs)[aaply(.data=diffs,.mar=1,.fun=function(x) which(x[1:nrow(parties)]==x[nrow(parties)+1]))])
    return(affil)
  }
  #Distances
  dist.pred<-function(voters,parties){
  require(pdist)
  mat.distance<-as.matrix(pdist(voters, parties))  ##matrix of distances from voter to party - rows are voters, columns are parties
  part.names<-as.character(1:nrow(parties))
  colnames(mat.distance)<-part.names
  ###Winner (name of the party with the highest number of voters )
  winner<-colnames(mat.distance)[which.max(table(distance.multi.prob(voters,parties)))]
  return(mat.distance)
  }
  
  ###VISUALIZE
  visualize.multi<-function(voters,parties){
    #Function to determine affiliation
    affiliate<-distance.multi.prob(voters,parties)  ##returns a vector with the number indicating affiliation
    # Plot voters
    col.pal<-rainbow(nrow(parties))
    palette(col.pal)
    plot(voters[,1],voters[,2],col=affiliate,pch=20, ,xlim=c(min(voters[,1])-1,max(voters[,1])+1 ), ylim=c(min(voters[,2])-1,max(voters[,2])+1 ))  
    points(parties[,1],parties[,2],col="black",bg=1:nrow(parties),pch=23,cex=2,lwd=2)  ##plot parties as diamonds - party 1 is blue, 2 is red
    abline(h=0)
    abline(v=0)
  }

  ##RELOCATE
  relocate.multi<-function(voters,parties){
    affiliate<-distance.multi.prob(voters,parties)  ##returns a vector with the number of party selected by the voter
    mat.distance2<-dist.pred(voters,parties)
    winner<-as.numeric(colnames(mat.distance2)[which.max(table(affiliate))]) #winner party with the highest proportion
   #Distance winner
  w.dist<-parties[winner,]
    voters.parties<-new.parties<-list()
    for (i in 1:npar) {
      voters.parties[[i]]<-assign(paste("voters.party",i,sep=""), as.matrix(voters[affiliate==i,]))
      
      winner<-colnames(mat.distance2)[which.max(table(affiliate))]
      
      new.parties[[i]]<-assign(paste("newparty",i,sep=""), c((parties[i,1]-(0.5*(parties[i,1]-w.dist[1]))),
                                                             (parties[i,2]-(0.5*(parties[i,2]-w.dist[2])))))                                                        
    }
    return(matrix(unlist(new.parties),byrow=TRUE,ncol=2))  ##return matrix of new party positions (each row represents a party)
  }
  ####ITERATIONS
  out.mats.list<-list()
  for (i in 1:npar){
    out.mats.list[[i]]<-assign(paste("out.mat",i,sep=""), matrix(ncol=2, nrow=iter))
  }
  if(iter>15){
    saveLatex(expr=   ##creates animation of first 15 iterations and creates a pdf
                for(i in 1:15){
                  for(j in 1:npar){
                    out.mats.list[[j]][i,]<-parties[j,]}
                  affiliate<-distance.multi.prob(voters,parties)  ##returns a vector with 0's indicating affiliation with party 1
                  visualize.multi(voters,parties)  ##visualize iterations in animation
                  parties<-relocate.multi(voters,parties) ##reassign parties to means of voters that supported them
                },img.name="Rplot",overwrite=TRUE)
    
    for(k in 16:iter){  ##continues simulation for remaining iterations
      for(j in 1:npar){
        out.mats.list[[j]][k,]<-parties[j,]}
      affiliate<-distance.multi.prob(voters,parties)  ##returns a vector with 0's indicating affiliation with party 1
      parties<-relocate.multi(voters,parties) ##reassign parties to means of voters that supported them
    }
  }else{
    saveLatex(expr=   ##creates animation of all iterations and creates a pdf
                for(l in 1:iter){
                  for (j in 1:npar){
                    out.mats.list[[j]][l,]<-parties[j,]}
                  affiliate<-distance.multi.prob(voters,parties)  ##returns a vector with 0's indicating affiliation with party 1
                  visualize.multi(voters,parties)  ##visualize iterations in animation
                  parties<-relocate.multi(voters,parties) ##reassign parties to means of voters that supported them
                },img.name="Rplot",overwrite=TRUE)
    return(out.mats.list)
  }
}
}
master.heuristics(heur="predator", npar=3)

