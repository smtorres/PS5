####CLASS ACTIVITY
#1
voters.normal<-function(n, mu=0, sigma1=1, sigma2=1){
  id<-1:n
  d1<-rnorm(n, mu, sd=sigma1)
  d2<-rnorm(n, mu, sd=sigma2)
  mat<-cbind(id, d1, d2)
return(mat)  
}

voters.uniform<-function(n, a=0, b=1){
  id<-1:n
  d1<-runif(n, sd=sigma1)
  d2<-runif(n, sd=sigma2)
  mat<-cbind(id, d1, d2)
  return(mat)    
}

voters.multi<-function(n, Mu, Sigma){
 require(MASS)
  id<-1:n
  mat<-mvrnorm(n, Mu, Sigma)
 mat<-cbind(id,mat) 
 return(mat)    
}

voters.multi2<-function(n, Mu1, Mu2, Mu3, Sigma, r=3){
  require(MASS)
  id<-1:n
  if(r==2){
    mat1<-mvrnorm(n%/%2, Mu1, Sigma)
    mat2<-mvrnorm(n-n%/%2, Mu2, Sigma)
    mat.voters<-rbind(mat1,mat2)
  }
  if(r==3){
    mat1<-mvrnorm(n%/%3, Mu1, Sigma)
    mat2<-mvrnorm(n%/%3, Mu2, Sigma)
    mat3<-mvrnorm(n-2*(n%/%3),Mu3,Sigma)
    mat.voters<-rbind(mat1,mat2,mat3)
  }
  mat.voters<-cbind(id,mat.voters)
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
  colnames(mat.voters)<-c("ID", "Dimension 1", "Dimension 2")
  return(mat.voters)
}


distance<-function(voters,parties){
  require(pdist)
  mat.distance<-as.matrix(pdist(voters[,2:3], parties[,2:3]))  ##matrix of distances from voter to party - rows are voters, columns are parties
  return(as.numeric(mat.distance[,1]>mat.distance[,2]))  ##returns a vector of 0's for voters closer to party 1, and 1 for voters closer to party 2
}

visualize<-function(voters,parties){
  affiliate<-distance(voters,parties)  ##returns a vector with 0's indicating affiliation with party 1
  plot(voters[,2],voters[,3],col=ifelse(affiliate,"red","blue"),pch=20)  ##plot voters - affiliation with party 1 is blue, party 2 is red
  points(parties[,2],parties[,3],col="black",bg=c("blue","red"),pch=23,cex=2,lwd=2)  ##plot parties as diamonds - party 1 is blue, 2 is red
  abline(h=0)
  abline(v=0)
}


relocate<-function(voters,parties){
  affiliate<-distance(voters,parties)  ##returns a vector with 0's indicating affiliation with party 1
  voters.party1<-voters[affiliate==0,]  ##matrix of voters affiliating with party 1
  voters.party2<-voters[affiliate==1,]  ##matrix of voters affiliating with party 2
  newparty1<-c(1,mean(voters.party1[,2]),mean(voters.party1[,3])) ##reassigns party 1 to mean of supporters along both dimensions
  newparty2<-c(2,mean(voters.party2[,2]),mean(voters.party2[,3])) ##reassigns party 1 to mean of supporters along both dimensions
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
                  out.mat1[i,]<-parties[1,2:3]  ##assigns i-th row of output matrix for party 1 the i-th party position
                  out.mat2[i,]<-parties[2,2:3]  ##assigns i-th row of output matrix for party 2 the i-th party position
                  affiliate<-distance(voters,parties)  ##returns a vector with 0's indicating affiliation with party 1
                  visualize(voters,parties)  ##visualize iterations in animation
                  parties<-relocate(voters,parties) ##reassign parties to means of voters that supported them
                },img.name="Rplot",overwrite=TRUE)
 
    for(k in 16:iter){  ##continues simulation for remaining iterations
      out.mat1[k,]<-parties[1,2:3]
      out.mat2[k,]<-parties[2,2:3]
      affiliate<-distance(voters,parties)  ##returns a vector with 0's indicating affiliation with party 1
      parties<-relocate(voters,parties) ##reassign parties to means of voters that supported them
    }
  }else{
    saveLatex(expr=   ##creates animation of all iterations and creates a pdf
                for(i in 1:iter){
                  out.mat1[i,]<-parties[1,2:3]  ##assigns i-th row of output matrix for party 1 the i-th party position
                  out.mat2[i,]<-parties[2,2:3]  ##assigns i-th row of output matrix for party 2 the i-th party position
                  affiliate<-distance(voters,parties)  ##returns a vector with 0's indicating affiliation with party 1
                  visualize(voters,parties)  ##visualize iterations in animation
                  parties<-relocate(voters,parties) ##reassign parties to means of voters that supported them
                },img.name="Rplot",overwrite=TRUE)
    
  }

  return(list(out.mat1,out.mat2))  ##return party positions as list. First element is matrix of party 1's positions, second element is matrix of party 2's
}

##TEST BRANCHING



















