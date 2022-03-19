install.packages('igraphdata')
library("igraph")
library("igraphdata")

set.seed(42)

data(karate)

n = 34
plot(karate)

A <- as.matrix(as_adjacency_matrix(karate))
A

D <- colSums(A)
D
T <- A/D
T
(p0 =rbinom(n,1,0.3))
(p1 =T%*%p0)
(p2 =T%*%p1)
(p3 =T%*%p2)



Nsim = 50

p = p0
P = p

for(s in 2:Nsim){
  
  p = T%*%p
  P = cbind(P,p)
  
  plot(1:s, P[1,], type = 'l')
  
  for(i in 2:n){
    lines(1:s, P[i,], type = 'l')
  }
  
}

#-----------------------------------------------------------
#
# INDEPENDENT CASCADE MODEL
#
#-----------------------------------------------------------

l <- layout_with_fr(karate)
l
Prob <- 1/degree(karate)

Vertex.col <- rep(0,n)

Initial.member <- c(2,3,4)
Vertex.col[Initial.member] <- 4
V(karate)$color <- Vertex.col
plot(karate, layout= l, vertex.size=20, vertex.label = NA)



Active <- Initial.member

Explore <- TRUE
i <- 1
while(i <= n && Explore){
  
  if (i <= length(Active) && length(Active) < n){
    
    Candidate.nodes <- which(X[Active[i],]>0)
    
    New.Active <- NULL
    for(j in Candidate.nodes){
      Add.node <- rbinom(1,1,Prob[j])
      if(Add.node > 0){
        New.Active <- c(New.Active, j)
      }
    }
    
    print(c(Candidate.nodes, New.Active))
    
    Active <- unique(c(Active, New.Active))
    Vertex.col[Active] <- 4
    V(g)$color <- Vertex.col
    plot(g, layout= l, vertex.size=20, vertex.label = NA)
  }else{
    Explore <- FALSE
  }
  i <- i+1
  Sys.sleep(1)
  
}

#-----------------------------------------------------------
#
# simulate the process function
#
#-----------------------------------------------------------

simulate <- function(Seeds, g, Prob, display){
  
  # Plot the initial state
  
  if(display){
    Vertex.col <- rep(0,n)
    Vertex.col[Seeds] <- 4
    V(g)$color <- Vertex.col
    plot(g, layout= l, vertex.size=20, vertex.label = NA)
  }
  
  # Build the adjacency matrix
  
  X <- as.matrix(get.adjacency(g))
  
  Active <- Seeds
  Explore <- TRUE
  i <- 1
  while(i <= n && Explore){
    
    if (i <= length(Active) && length(Active) < n){
      
      Candidate.nodes <- which(X[Active[i],]>0)
      
      New.Active <- NULL
      for(j in Candidate.nodes){
        Add.node <- rbinom(1,1,Prob[j])
        if(Add.node > 0){
          New.Active <- c(New.Active, j)
        }
      }
      
      #print(c(Candidate.nodes, New.Active))
      
      Active <- unique(c(Active, New.Active))
      
      if(display){
        Vertex.col[Active] <- 4
        V(g)$color <- Vertex.col
        plot(g, layout= l, vertex.size=20, vertex.label = NA)
      }
      
    }else{
      Explore <- FALSE
    }
    i <- i+1
    Sys.sleep(0)
    
  }
  
  return(length(Active))
  
}



#----------------------------------------------
# Simulate the system multiple times starting 
# from the same initial seed
#----------------------------------------------

simulate(Initial.member, karate, Prob, TRUE )

Nsim = 500
MyResults = rep(0,Nsim)

for(s in 1:Nsim){
  MyResults[s] = simulate(Initial.member, g, Prob, FALSE )
}

hist(MyResults, breaks = 100)

#----------------------------------------------
# Find the best initial node to maximize the 
# number of influenced nodes
#----------------------------------------------

Nsim = 1000

ICM_MeanCentrality = rep(0,n)
ICM_VarCentrality = rep(0,n)

for(MyNode in 1:n){
  
  Initial.member <- MyNode
  
  MyResults = rep(0,Nsim)
  
  for(s in 1:Nsim){
    MyResults[s] = simulate(Initial.member, karate, Prob, FALSE )
  }
  
  hist(MyResults, breaks = 100, main = paste("Node", MyNode))
  
  ICM_MeanCentrality[MyNode] = mean(MyResults)
  ICM_VarCentrality[MyNode] = sd(MyResults)
  
}

ord = order(ICM_VarCentrality)

plot(ICM_VarCentrality[ord], ICM_MeanCentrality[ord], type = "o", lwd = 3)




Initial.member <- 27
n.infected <- 0
for(i in 1:10){
  n.infected <- n.infected + simulate(Initial.buyers,g, Prob, FALSE)
}
n.infected/n


