# Required Libraries. In order to execute this script, 
# these packages must be loaded.

# install.packages("mc2d")         delete diez symbol for installing packages
# install.packages("bindata")

# Load Libraries
library("mc2d")
library("bindata")

# Number of Trials
n=50000

# Variables for Sim and Model
sim.loss.mean<-NULL         	# Mean Loss for Simulation
model.loss.mean<-NULL       	# Mean Loss for Model
sim.loss.sd<-NULL           	# Standard Deviation of Loss  for Simulation
model.loss.sd<-NULL         	# Standard Deviation of Loss  for Model
sim.no.loss.prob<-NULL         	# Probability of Zero Loss for Simulation
model.no.loss.prob<-NULL       	# Probability of Zero Loss for Model
sim.95.quantile<-NULL          	# 95th percentile of loss for Simulation
model.95.quantile<-NULL        	# 95th percentile of loss for Model
offensive<-NULL

######################## Simulation Part ############################
# 1 - Probability p?? of offensive missile success is varied across #
#     3 values: 50%, 67%, and 83%. The same 3 values are separately #
#     used for the probability pz of defensive interception.        #
#                  							        #
# 2 - The simulation tries 3 levels of correlation: 0.0, 0.2, and   #
#     0.4. Only offensive missile correlation is included.          #
#                                                                   #
# 3 - The simulation tries 3 differently shaped probability         # 
#     distributions for the damage per missile.                     #
#####################################################################

# Reading csv file of created design points for simulation
StocSalvo <- read.csv(file.choose(),header = T)

#Replications for Each Design Point for Simulation
for (i in 1:dim(StocSalvo)[1])
{
  print(i)
  # Parameters input
  A=StocSalvo[i,1]       # Starting Force Strength
  B=StocSalvo[i,2]  
  na=StocSalvo[i,3]      # Number of Offensive SSMs per Ship per Salvo
  nz=StocSalvo[i,4]      # Number of Defensive SAMs per Ship per Salvo
  cor=StocSalvo[i,5]
  po=StocSalvo[i,6]      # Probability of Succesful Offensive Missile
  pz=StocSalvo[i,9]      # Probability of Succesful Defense
  dist=StocSalvo[i,10]
  offensive<-NULL

  if(0!=cor)        
  {                                
    pa=rep(po,A)
    rho=cor
    off<-rmvbin(n, pa, bincorr=(1-rho)*diag(A)+rho)  
    
    for (j in 1:n) {
      
      offensive[j]<-sum(off[j,])
    }
  }
  
  else
  {
    offensive<-rbinom(n,A*na,po)
  }
  
  defensive<-rbinom(n,B*nz,pz)                     # Defensive Power
  net<-offensive-defensive                         # Net Strength
  #Temporary variable
  damage_v<-NULL                # Nominal number of ships surviving: mean
  
  # Normal Distribution
  if(1==dist)                 # in data file, Column 12 stands for distibutions
  {                           # 1: Norm, 2:Uniform, 3:Triangular
    for (j in 1:n) 
    {
      if(net[j]>0)                                        
      {                                                  
        damage_v[j]<-sum(rnorm(net[j],.33,.11))   
        if(damage_v[j]<0)
        {
          damage_v[j]<-0 
        }
      }                                      
      else if(net[j]<=0)                    
      {                                   
        damage_v[j]<-0             
      }                                            
    }
  }
  
  # Uniform Distibution
  else if(2==dist)
  {
    for (j in 1:n) 
    {
      if(net[j]>0)
      {
        damage_v[j]<-sum(runif(net[j],0.1395, 0.5205))
      }
      else if(net[j]<=0)
      {
        damage_v[j]<-0
      }
    }
  }

  #Triangular Distribution
  else if(3==dist)
  {
    for (j in 1:n) 
    {
      if(net[j]>0)
      {
        damage_v[j]<-sum(rtriang(net[j],0.1744,0.1744,0.6411))
      }
      else if(net[j]<=0)
      {
        damage_v[j]<-0
      }
    }
  } 
  
  sim.loss.mean[i]<-mean(damage_v)
  sim.loss.sd[i]<-sd(damage_v)
  sim.no.loss.prob[i]<-sum((damage_v)==0)/n
  sim.95.quantile[i]<-quantile(damage_v,c(0.95))
}  
  ############# Model Part ###############
  
  #Reading csv file of created design points for Model
  StocSalvoMod <- read.csv("~/Desktop/Thesis/StocSalvo.csv")
  
  #Replications for Each Design Point for Model
for (i in 1:dim(StocSalvo)[1])
{
  # Parameters input
  # Starting Force Strength
  A=StocSalvo[i,1]
  B=StocSalvo[i,2]
  # Number of Offensive SSMs per Ship per Salvo
  na=StocSalvo[i,3]
  # Number of Defensive SAMs per Ship per Salvo
  nz=StocSalvo[i,4]
  ## Probability of success offensive missile 
  pa=StocSalvo[i,6]
  # Number of Defensive SAMs per Ship per Salvo
  pz=StocSalvo[i,9]
  #Loss Suffered per non-intercepted SSM:Mean
  mu.v=0.33
  # Loss Suffered per non-intercepted SSM:Standard Deviation
  sd.v=0.11
  
  # Model Part
  B.E.Off=A*na*pa                                 # Offensive Firepower:Mean
  B.E.Off.sd=sqrt(A*na*pa*(1-pa))                 # Offensive Firepower:Sd
  
  B.E.Def=B*nz*pz                                 # Defensive Firepower:Mean
  B.E.Def.sd=sqrt(B*nz*pz*(1-pz))                 # Defensive Firepower:Sd
  
  B.E.Net=B.E.Off-B.E.Def                             # NetAB Firepower:Mean
  B.E.Net.sd=sqrt(B.E.Off.sd^2+B.E.Def.sd^2)          # NetAB Firepower:Sd
  
  Nominal.Number.of.Surviving.B.ship=B-B.E.Net*mu.v
  
  Nominal.Number.of.Surviving.B.ship.sd=sqrt(B.E.Net*sd.v^2+mu.v^2*B.E.Net.sd^2-2*
      sd.v^2*(B.E.Net*pnorm(0,B.E.Net,B.E.Net.sd)-B.E.Net.sd^2*
      dnorm(0,B.E.Net, B.E.Net.sd)))
  
  Actual.number.of.surviving.B.ship.Mean<-Nominal.Number.of.Surviving.B.ship.sd^2*
      (dnorm(B-mu.v/2, Nominal.Number.of.Surviving.B.ship,Nominal.Number.of.Surviving.B.ship.sd)
      -dnorm(mu.v/2,Nominal.Number.of.Surviving.B.ship,Nominal.Number.of.Surviving.B.ship.sd))
      +Nominal.Number.of.Surviving.B.ship*(pnorm(A-mu.v/2,Nominal.Number.of.Surviving.B.ship,Nominal.Number.of.Surviving.B.ship.sd)
      -pnorm(mu.v/2,Nominal.Number.of.Surviving.B.ship, Nominal.Number.of.Surviving.B.ship.sd))
      +B*(1-pnorm(A-mu.v/2, Nominal.Number.of.Surviving.B.ship,Nominal.Number.of.Surviving.B.ship.sd))
    
  Actual.number.of.surviving.B.ship.Sd<-sqrt((Nominal.Number.of.Surviving.B.ship^2
      +Nominal.Number.of.Surviving.B.ship.sd^2)*(pnorm(B-mu.v/2,Nominal.Number.of.Surviving.B.ship
      ,Nominal.Number.of.Surviving.B.ship.sd)- pnorm(mu.v/2,Nominal.Number.of.Surviving.B.ship
      ,Nominal.Number.of.Surviving.B.ship.sd))+B^2*(1-pnorm(B-mu.v/2, Nominal.Number.of.Surviving.B.ship
      ,Nominal.Number.of.Surviving.B.ship.sd))-Actual.number.of.surviving.B.ship.Mean^2
      -Nominal.Number.of.Surviving.B.ship.sd^2* ((B-mu.v/2+Nominal.Number.of.Surviving.B.ship)
      *dnorm(B-mu.v/2,Nominal.Number.of.Surviving.B.ship,Nominal.Number.of.Surviving.B.ship.sd)
      -(mu.v/2+Nominal.Number.of.Surviving.B.ship)* dnorm(mu.v/2,Nominal.Number.of.Surviving.B.ship
      ,Nominal.Number.of.Surviving.B.ship.sd)))
    
    model.loss.mean[i]<-mean(B-Actual.number.of.surviving.B.ship.Mean)
    
    model.loss.sd[i]<-Actual.number.of.surviving.B.ship.Sd
    
    model.no.loss.prob[i]<-1-pnorm(B-mu.v/2,Nominal.Number.of.Surviving.B.ship
      ,Nominal.Number.of.Surviving.B.ship.sd)
    
    model.95.quantile[i]<-B-(min(B,max(0,Nominal.Number.of.Surviving.B.ship
      -Nominal.Number.of.Surviving.B.ship.sd*1.645)))}
}
  
# Saves results as a data frame
SalvoResult<-data.frame(StocSalvoMod,ModelMeanLoss=model.loss.mean, ModelSdLoss=model.loss.sd,ModelNoLossProb=model.no.loss.prob, =model.95.quantile, SimMeanLoss=sim.loss.mean, SimSdLoss=sim.loss.sd, SimNoLossProb=sim.no.loss.prob,Sim95=sim.95.quantile)
  
# Saves csv file on main diroctory
write.csv(SalvoResult, file = "SalvoResult.csv",row.names =F,quote = F)
  