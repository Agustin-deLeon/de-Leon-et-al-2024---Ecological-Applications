################################################################
####### de León et al. 2024
################################################################
# Data manipulation for analysis

library(openxlsx)
Filter.river<- read.csv("Wetland.matrix.csv") #Unzip elements from "Wetland" and "Stream" before runing this
Filter.river<- Filter.river[,-1]
Filter.just.river<- read.csv("Stream.matrix.csv")
Filter.just.river<- Filter.just.river[,-1]
colnames(Filter.river)<- c("x", "y", "Water.body", "dist.to.efl.1", "dist.to.efl.2", "dist.to.efl.3", "dist.to.efl.4")
colnames(Filter.just.river)<- c("x", "y", "dist.to.efl.1", "dist.to.efl.2", "dist.to.efl.3", "dist.to.efl.4")

coord.river<- cbind(Filter.river$x, Filter.river$y)
coord.just.river<- cbind(Filter.just.river$x, Filter.just.river$y)
rbind(c(-6114504,-4139378), coord.river,c(-6107598, -4152710 ))-> coord.river
rbind(c(-6114504,-4139378), coord.just.river,c(-6107598, -4152710 ))-> coord.just.river

#library(sf)
Filter.river<-as.data.frame(Filter.river)
#Filter.river<- st_as_sf(Filter.river, coords=c("x","y"), crs = 4326)
Filter.just.river<-as.data.frame(Filter.just.river)
#Filter.just.river<- st_as_sf(Filter.just.river, coords=c("x","y"), crs = 4326)

as.matrix(dist(coord.river))-> distancia
ifelse(distancia>400,1000000000,distancia)-> M.distancia
dist_01<- ifelse(M.distancia>400,0, M.distancia)
dist_01<- ifelse(dist_01>0,1,0)
((Filter.river$dist.to.efl.1)*1)-> dist.to.cont.1
((Filter.river$dist.to.efl.2)*3)-> dist.to.cont.2
((Filter.river$dist.to.efl.3)*3)-> dist.to.cont.3
((Filter.river$dist.to.efl.4)*15)-> dist.to.cont.4 #15 is a magnitude value, higher values increases distances between effluents and communities, decreasing the impact (k value in main text)
rbind(dist.to.cont.1,dist.to.cont.2,dist.to.cont.3,dist.to.cont.4)-> dist.to.cont
cbind(rep(100000,4), dist.to.cont, rep(100000,4))-> dist.to.cont #Adding high distances between fixed communities and effluents

as.matrix(dist(coord.just.river))-> distancia.river
ifelse(distancia.river>400,1000000000,distancia.river)->M.distancia.river
distancia.river[131,70]->M.distancia.river[131,70] 
distancia.river[70,131]->M.distancia.river[70,131]
dist_01_river<- ifelse(M.distancia.river>400,0, M.distancia.river)
dist_01_river<- ifelse(dist_01_river>0,1,0)
dist_01_river[131,70]<-1
dist_01_river[70,131]<-1
((Filter.just.river$dist.to.efl.1)*1)-> dist.to.cont.1
((Filter.just.river$dist.to.efl.2)*3)-> dist.to.cont.2
((Filter.just.river$dist.to.efl.3)*3)-> dist.to.cont.3
((Filter.just.river$dist.to.efl.4)*15)-> dist.to.cont.4 #15 is a magnitude value, higher values increases distances between effluents and communities, decreasing the impact (k value in main text)
rbind(dist.to.cont.1,dist.to.cont.2,dist.to.cont.3,dist.to.cont.4)-> dist.to.cont.river
cbind(rep(100000,4), dist.to.cont.river, rep(100000,4))-> dist.to.cont.river #Adding high distances between fixed communities and effluents

sna::closeness(dist_01,gmode="graph",ignore.eval=TRUE, 
               cmode="undirected")->clos
sna::closeness(dist_01_river,gmode="graph",ignore.eval=TRUE,
               cmode="undirected")->clos_river
(1-(clos))-> clos
(1-(clos_river))-> clos_river
(clos[2:1889])/max(clos[2:1889])-> Filter.river$clos #Standarize closeness values between 0 and 1
(clos_river[2:516])/max(clos_river[2:516])-> Filter.just.river$clos

###################################################################
#Elements of the function: wetland landscape
#Filter matrix
ifelse(Filter.river$Water.body=="River (1)",1,0.9)-> Filtro1
Filtro1<- do.call("rbind", replicate (100, Filtro1, simplify=FALSE))
ifelse(Filter.river$Water.body=="Wetland (2-3)",1,0.9)-> Filtro2
Filtro2<- do.call("rbind", replicate (100, Filtro2, simplify=FALSE))
Filtro3<- matrix(data=1, nrow=100, ncol = 1888)
Matriz_Filtro<- rbind(Filtro1,Filtro2,Filtro3) # Permanent water spp, flood zone and water-independent, in that ordern 
Matriz_Filtro<- cbind((rep(1,300)), Matriz_Filtro, (rep(1,300))) #fixed performance in fixed communities
#Affected communities
id.impacto.b<- c(2:1889)
#Regional Pool
Meta.pool <- c(rep(10000,300))
#Impacted Species
id.spp.impactadas<- c(1:45, 51:95, 101:145, 151:195, 201:245, 251:295)
#Communities abundances
Js<-rep(x = 3000,1890) 
Js=as.matrix(Js)
#Modules (not used)
id.module<- c(rep(1,1038),rep(2,36), rep(1,1), rep(2,38), rep(1,1), rep(2,41), rep(1,1), rep(2,42), rep(1,1), rep(2,41), rep(1,1), rep(2,649))
#Fixed communities identity
id.fixed<- 1
id.fixed2<- 1890
#Fixed communities composition 
comm.fixed<- c(rep(20,50), rep(0,50), rep(20,50), rep(0,50), rep(20,50), rep(0,50)) 
comm.fixed2<- c(rep(0,50), rep(20,50), rep(0,50), rep(20,50), rep(0,50), rep(20,50))

#Function used:
impacto.2<- function(Meta.pool, m.pool, id.spp.impactadas,id.impacto.b, 
                     Js, id.module, filter.env, id.impacto.a,
                     M.dist, D50, m.max , id.fixed, 
                     D50.fixed , m.max.fixed , comm.fixed, 
                     D50.fixed2, m.max.fixed2, comm.fixed2, id.fixed2,
                     Lottery , it , prop.dead.by.it = 0.05, id.obs, dist.to.cont,
                     dispersion, cont.Filtro, cont.J, parallel.imp) {
  out<-NULL
  for(m in dispersion){
    for(e in cont.Filtro){
      for(c in cont.J){
        cat("va en dispersion: ", m, "\n")
        cat("Va en cont.Filtro: ", e, "\n")
        cat("Va en cont.J: ", c, "\n")  
        J.temp=max(Js)
        J.imp=NULL
        for (a in id.impacto.b){
          J.imp.t <- as.matrix(((exp(-1)/c)*min(dist.to.cont[,a]))) 
          J.imp<- rbind(J.imp,J.imp.t)}
        ifelse(J.imp>1, 1, J.imp) -> J.impactada
        (Js[1:(length(id.impacto.b)),] * J.impactada[1:(length(id.impacto.b)),]) -> J.temp.t 
        as.matrix(J.temp.t)-> J.temp.t
        J.temp<- rbind(J.temp,J.temp.t)
        J.temp<- rbind(J.temp, max(Js))
        View(J.temp)
        
        filter=NULL
        filter2=NULL
        filter.env<- Matriz_Filtro
        for (a in id.impacto.b){ 
          filter.t<- as.matrix(((exp(-1)/e)*min(dist.to.cont[,a]))) #change e to 2250, 3500 or 5000 for different propagation scenarios
          filter<-rbind(filter,filter.t)}
        ifelse(filter>1, 1.0, filter) -> filter2
        as.matrix(filter2)-> filter2
        filter2<- rbind(1, filter2, 1)
        filter2<- (1 - filter2) * (0.8)  
        for(a in id.impacto.b){(filter.env[id.spp.impactadas,a]) - (filter2[a,])-> filter.env[id.spp.impactadas,a]} 
        ifelse(filter.env==1, 0.999999, filter.env)-> filter.env
        View(filter.env)
        
        # Parallel computing
        if(parallel.imp==F){
          b<-list()
          for(f in repeticiones){
            b[[f]]<-H2020_Coalescent.and.lottery.exp.Kernel.J(Meta.pool = Meta.pool, m.pool = m.pool, 
                                                              Js = J.temp, id.module = id.module, filter.env = filter.env,
                                                              M.dist = M.dist, D50 = m, m.max = m.max, id.fixed = id.fixed, id.fixed2=id.fixed2,
                                                              D50.fixed = m, m.max.fixed = m.max.fixed, comm.fixed = comm.fixed, 
                                                              D50.fixed2 = m, m.max.fixed2 = m.max.fixed2, comm.fixed2 = comm.fixed2,
                                                              Lottery = Lottery, it = it, 
                                                              prop.dead.by.it = prop.dead.by.it, id.obs = id.obs)
            as.matrix(b[[f]])-> result.1
            out.t<-cbind("Nrep"=f,"D50"=m,"efecto.filtro"=e,"efecto.J"=c,"J.max"=max(J.temp),"J.min"=min(J.temp),result.1) 
            
            
          }  
    
          out<-rbind(out,out.t) 
        }
        
        if(parallel.imp==T){
          out.t<-foreach(f = 1:length(repeticiones), .combine=rbind)%dopar% {
            cat("va en dispersion: ", f, "\n")
            b<-H2020_Coalescent.and.lottery.exp.Kernel.J(Meta.pool = Meta.pool, m.pool = m.pool, 
                                                         Js = J.temp, id.module = id.module, filter.env = filter.env,
                                                         M.dist = M.dist, D50 = m, m.max = m.max, id.fixed = id.fixed, id.fixed2=id.fixed2,
                                                         D50.fixed = m, m.max.fixed = m.max.fixed, comm.fixed = comm.fixed, 
                                                         D50.fixed2 = m, m.max.fixed2 = m.max.fixed2, comm.fixed2 = comm.fixed2,
                                                         Lottery = Lottery, it = it, 
                                                         prop.dead.by.it = prop.dead.by.it, id.obs = id.obs)
            
            out.t<-cbind("Nrep"=f,"D50"=m,"efecto.filtro"=e,"efecto.J"=c,"J.max"=max(J.temp),"J.min"=min(J.temp),b) 
            
            #res<-c(1:ncol(out.t))
            #J.loc=NULL
            #for(r in res){
            #  J.loc.t<- sum(out.t[(nrow(out.t)-nrow(filter.env)-3):(nrow(out.t)-4),r])
            #  J.loc<- cbind(J.loc,J.loc.t)}  
            #out.t<-rbind(out.t, J.loc) 
          }
          out<-rbind(out,out.t) 
        }
      }
    }
  }
  out
}

####READY FOR SIMULATIONS 
#It will last a few days, parallel function reduce the simulating time, we recomend to leave 2 Cores with no use so
#operational use of the computer will go on
library(doParallel)
registerDoParallel(cores = detectCores()-2)

#All scenarios can be made at once, but it will last a lot and you wont see the simulation progress until it is finished
#because of this, a different element was created for each e50, but it can be done together perfectly
#Both effects at the same time simulations were not include in this script

#########################################################################
#No pollution
dispersion <- c(35,85,200) #Setted d50 values, low, medium and high dispersal capacity
cont.Filtro <- c(10) 
cont.J <- c(10)
repeticiones<-c(1:20) 

WWTP.10F<-impacto.2(Meta.pool=Meta.pool, m.pool=0.01, m.max=1,D50=m,
                    id.spp.impactadas=id.spp.impactadas,id.impacto.b = id.impacto.b,
                    Js, id.module = NULL, filter.env = Matriz_Filtro, id.impacto.a = 1:1890,
                    M.dist=M.distancia,id.fixed=id.fixed, D50.fixed =m, m.max.fixed = 1, comm.fixed=comm.fixed, 
                    id.fixed2 = id.fixed2, D50.fixed2 =m, m.max.fixed2 = 1, comm.fixed2 = comm.fixed2, 
                    Lottery=T , it =1000,
                    prop.dead.by.it = 0.05, id.obs =1:1890, dist.to.cont= dist.to.cont,
                    dispersion = dispersion, cont.Filtro = cont.Filtro, cont.J = cont.J, parallel.imp = T)


################################################################################################
# Filter effect, e50 = 2250
dispersion <- c(35,85,200) 
cont.Filtro <- c(2250)
cont.J <- c(10)
repeticiones<-c(1:20)

WWTP.2250F<-impacto.2(Meta.pool=Meta.pool, m.pool=0.01, m.max=1,D50=m,
                      id.spp.impactadas=id.spp.impactadas,id.impacto.b = id.impacto.b,
                      Js, id.module= NULL, filter.env = Matriz_Filtro, id.impacto.a = 1:1890,
                      M.dist=M.distancia,id.fixed=id.fixed, D50.fixed =m, m.max.fixed = 1, comm.fixed=comm.fixed, 
                      id.fixed2 = id.fixed2, D50.fixed2 =m, m.max.fixed2 = 1, comm.fixed2 = comm.fixed2, 
                      Lottery=T , it =1000,
                      prop.dead.by.it = 0.05, id.obs =1:1890, dist.to.cont= dist.to.cont,
                      dispersion = dispersion, cont.Filtro = cont.Filtro, cont.J = cont.J, parallel.imp = T)


# Filter effect, e50 = 3500
dispersion <- c(35,85,200) 
cont.Filtro <- c(3500)
cont.J <- c(10)
repeticiones<-c(1:20)

WWTP.3500F<-impacto.2(Meta.pool=Meta.pool, m.pool=0.01, m.max=1,D50=m,
                      id.spp.impactadas=id.spp.impactadas,id.impacto.b = id.impacto.b,
                      Js, id.module= NULL, filter.env = Matriz_Filtro, id.impacto.a = 1:1890,
                      M.dist=M.distancia,id.fixed=id.fixed, D50.fixed =m, m.max.fixed = 1, comm.fixed=comm.fixed, 
                      id.fixed2 = id.fixed2, D50.fixed2 =m, m.max.fixed2 = 1, comm.fixed2 = comm.fixed2, 
                      Lottery=T , it =1000,
                      prop.dead.by.it = 0.05, id.obs =1:1890, dist.to.cont= dist.to.cont,
                      dispersion = dispersion, cont.Filtro = cont.Filtro, cont.J = cont.J, parallel.imp = T)


# Filter effect, e50 = 5000
dispersion <- c(35,85,200) 
cont.Filtro <- c(5000)
cont.J <- c(10)
repeticiones<-c(1:20)

WWTP.5000F <-impacto.2(Meta.pool=Meta.pool, m.pool=0.01, m.max=1,D50=m,
                       id.spp.impactadas=id.spp.impactadas,id.impacto.b = id.impacto.b,
                       Js, id.module= NULL, filter.env = Matriz_Filtro, id.impacto.a = 1:1890,
                       M.dist=M.distancia,id.fixed=id.fixed, D50.fixed =m, m.max.fixed = 1, comm.fixed=comm.fixed, 
                       id.fixed2 = id.fixed2, D50.fixed2 =m, m.max.fixed2 = 1, comm.fixed2 = comm.fixed2, 
                       Lottery=T , it =1000,
                       prop.dead.by.it = 0.05, id.obs =1:1890, dist.to.cont= dist.to.cont,
                       dispersion = dispersion, cont.Filtro = cont.Filtro, cont.J = cont.J, parallel.imp = T)


#############################################################################################
################################################################################################
#  Abundance effect, e50 = 2250
dispersion <- c(35,85,200) 
cont.Filtro <- c(10)
cont.J <- c(2250)
repeticiones<-c(1:20)

WWTP.2250J<-impacto.2(Meta.pool=Meta.pool, m.pool=0.01, m.max=1,D50=m,
                      id.spp.impactadas=id.spp.impactadas,id.impacto.b = id.impacto.b,
                      Js, id.module= NULL, filter.env = Matriz_Filtro, id.impacto.a = 1:1890,
                      M.dist=M.distancia,id.fixed=id.fixed, D50.fixed =m, m.max.fixed = 1, comm.fixed=comm.fixed, 
                      id.fixed2 = id.fixed2, D50.fixed2 =m, m.max.fixed2 = 1, comm.fixed2 = comm.fixed2, 
                      Lottery=T , it =1000,
                      prop.dead.by.it = 0.05, id.obs =1:1890, dist.to.cont= dist.to.cont,
                      dispersion = dispersion, cont.Filtro = cont.Filtro, cont.J = cont.J, parallel.imp = T)

# Abundance effect, e50 = 3500
dispersion <- c(35,85,200) 
cont.Filtro <- c(10)
cont.J <- c(3500)
repeticiones<-c(1:20)

WWTP.3500J<-impacto.2(Meta.pool=Meta.pool, m.pool=0.01, m.max=1,D50=m,
                      id.spp.impactadas=id.spp.impactadas,id.impacto.b = id.impacto.b,
                      Js, id.module= NULL, filter.env = Matriz_Filtro, id.impacto.a = 1:1890,
                      M.dist=M.distancia,id.fixed=id.fixed, D50.fixed =m, m.max.fixed = 1, comm.fixed=comm.fixed, 
                      id.fixed2 = id.fixed2, D50.fixed2 =m, m.max.fixed2 = 1, comm.fixed2 = comm.fixed2, 
                      Lottery=T , it =1000,
                      prop.dead.by.it = 0.05, id.obs =1:1890, dist.to.cont= dist.to.cont,
                      dispersion = dispersion, cont.Filtro = cont.Filtro, cont.J = cont.J, parallel.imp = T)

# Abundance effect, e50 = 5000
dispersion <- c(35,85,200) 
cont.Filtro <- c(10)
cont.J <- c(5000)
repeticiones<-c(1:20)

WWTP.5000J <-impacto.2(Meta.pool=Meta.pool, m.pool=0.01, m.max=1,D50=m,
                       id.spp.impactadas=id.spp.impactadas,id.impacto.b = id.impacto.b,
                       Js, id.module= NULL, filter.env = Matriz_Filtro, id.impacto.a = 1:1890,
                       M.dist=M.distancia,id.fixed=id.fixed, D50.fixed =m, m.max.fixed = 1, comm.fixed=comm.fixed, 
                       id.fixed2 = id.fixed2, D50.fixed2 =m, m.max.fixed2 = 1, comm.fixed2 = comm.fixed2, 
                       Lottery=T , it =1000,
                       prop.dead.by.it = 0.05, id.obs =1:1890, dist.to.cont= dist.to.cont,
                       dispersion = dispersion, cont.Filtro = cont.Filtro, cont.J = cont.J, parallel.imp = T)

################################################
# Calculate vectors of filter and abudance 
# effect for each community (Wetland landscape)
################################################

id.impacto.b<- c(2:1889)
filter=NULL
filter2=NULL
filter.env<- Matriz_Filtro
for (a in id.impacto.b){ 
  filter.t<- as.matrix(((exp(-1)/2250)*min(dist.to.cont[,a]))) #change e to 2250, 3500 or 5000 for different propagation scenarios
  filter<-rbind(filter,filter.t)}
ifelse(filter>1, 1.0, filter) -> filter2
as.matrix(filter2)-> filter2
filter2<- rbind(1, filter2, 1)
filter2<- (1 - filter2) * (0.8)  
for(a in id.impacto.b){(filter.env[id.spp.impactadas,a]) - (filter2[a,])-> filter.env[id.spp.impactadas,a]} 
ifelse(filter.env==1, 0.999999, filter.env)-> filter.env
filter.env[min(id.spp.impactadas),]-> WWTP.impact.F2250

id.impacto.b<- c(2:1889)
filter=NULL
filter2=NULL
filter.env<- Matriz_Filtro
for (a in id.impacto.b){ 
  filter.t<- as.matrix(((exp(-1)/3500)*min(dist.to.cont[,a]))) #change e to 2250, 3500 or 5000 for different propagation scenarios
  filter<-rbind(filter,filter.t)}
ifelse(filter>1, 1.0, filter) -> filter2
as.matrix(filter2)-> filter2
filter2<- rbind(1, filter2, 1)
filter2<- (1 - filter2) * (0.8)  
for(a in id.impacto.b){(filter.env[id.spp.impactadas,a]) - (filter2[a,])-> filter.env[id.spp.impactadas,a]} 
ifelse(filter.env==1, 0.999999, filter.env)-> filter.env
filter.env[min(id.spp.impactadas),]-> WWTP.impact.F3500

id.impacto.b<- c(2:1889)
filter=NULL
filter2=NULL
filter.env<- Matriz_Filtro
for (a in id.impacto.b){ 
  filter.t<- as.matrix(((exp(-1)/5000)*min(dist.to.cont[,a]))) #change e to 2250, 3500 or 5000 for different propagation scenarios
  filter<-rbind(filter,filter.t)}
ifelse(filter>1, 1.0, filter) -> filter2
as.matrix(filter2)-> filter2
filter2<- rbind(1, filter2, 1)
filter2<- (1 - filter2) * (0.8)  
for(a in id.impacto.b){(filter.env[id.spp.impactadas,a]) - (filter2[a,])-> filter.env[id.spp.impactadas,a]} 
ifelse(filter.env==1, 0.999999, filter.env)-> filter.env
filter.env[min(id.spp.impactadas),]-> WWTP.impact.F5000


J.temp=max(Js)
J.imp=NULL
for (a in id.impacto.b){
  J.imp.t <- as.matrix(((exp(-1)/2250)*min(dist.to.cont[,a]))) 
  J.imp<- rbind(J.imp,J.imp.t)}
ifelse(J.imp>1, 1, J.imp) -> J.impactada
(Js[1:(length(id.impacto.b)),] * J.impactada[1:(length(id.impacto.b)),]) -> J.temp.t 
as.matrix(J.temp.t)-> J.temp.t
J.temp<- rbind(J.temp,J.temp.t)
J.temp<- rbind(J.temp, max(Js))
round(J.temp,0)-> J.temp
J.temp-> WWTP.impact.J2250


J.temp=max(Js)
J.imp=NULL
for (a in id.impacto.b){
  J.imp.t <- as.matrix(((exp(-1)/3500)*min(dist.to.cont[,a]))) 
  J.imp<- rbind(J.imp,J.imp.t)}
ifelse(J.imp>1, 1, J.imp) -> J.impactada
(Js[1:(length(id.impacto.b)),] * J.impactada[1:(length(id.impacto.b)),]) -> J.temp.t 
as.matrix(J.temp.t)-> J.temp.t
J.temp<- rbind(J.temp,J.temp.t)
J.temp<- rbind(J.temp, max(Js))
round(J.temp,0)-> J.temp
J.temp-> WWTP.impact.J3500

J.temp=max(Js)
J.imp=NULL
for (a in id.impacto.b){
  J.imp.t <- as.matrix(((exp(-1)/500)*min(dist.to.cont[,a]))) 
  J.imp<- rbind(J.imp,J.imp.t)}
ifelse(J.imp>1, 1, J.imp) -> J.impactada
(Js[1:(length(id.impacto.b)),] * J.impactada[1:(length(id.impacto.b)),]) -> J.temp.t 
as.matrix(J.temp.t)-> J.temp.t
J.temp<- rbind(J.temp,J.temp.t)
J.temp<- rbind(J.temp, max(Js))
round(J.temp,0)-> J.temp
J.temp-> WWTP.impact.J5000


#############################################################################################
#############################################################################################

#Elements of the function: stream landscape
#Filter matrix
Matriz_river<- matrix(data=1, nrow = 300, ncol= (nrow(Filter.just.river)+2))
#Affected communities
id.impacto.b<- c(2:516)
#Regional pool
Meta.pool <- c(rep(10000,300))
#Affected species
id.spp.impactadas<- c(1:45, 51:95, 101:145, 151:195, 201:245, 251:295)
#Communities abundances
Js<-rep(x = 3000,517)
Js=as.matrix(Js)
#Modules (not used)
ifelse(coord.just.river[1:515,2]<(-4146900),1,2)-> modules
#Fixed communities identities
id.fixed<- 1
id.fixed2<- 517
#Fixed communities composition
comm.fixed<- c(rep(20,50), rep(0,50), rep(20,50), rep(0,50), rep(20,50), rep(0,50)) 
comm.fixed2<- c(rep(0,50), rep(20,50), rep(0,50), rep(20,50), rep(0,50), rep(20,50))

#Function used:
impacto.2<- function(Meta.pool, m.pool, id.spp.impactadas,id.impacto.b, 
                     Js, id.module, filter.env, id.impacto.a,
                     M.dist, D50, m.max , id.fixed, 
                     D50.fixed , m.max.fixed , comm.fixed, 
                     D50.fixed2, m.max.fixed2, comm.fixed2, id.fixed2,
                     Lottery , it , prop.dead.by.it = 0.05, id.obs, dist.to.cont,
                     dispersion, cont.Filtro, cont.J, parallel.imp) {
  out<-NULL
  for(m in dispersion){
    for(e in cont.Filtro){
      for(c in cont.J){
        cat("va en dispersion: ", m, "\n")
        cat("Va en cont.Filtro: ", e, "\n")
        cat("Va en cont.J: ", c, "\n")  
        J.temp=max(Js)
        J.imp=NULL
        for (a in id.impacto.b){
          J.imp.t <- as.matrix(((exp(-1)/c)*min(dist.to.cont.river[,a]))) 
          J.imp<- rbind(J.imp,J.imp.t)}
        ifelse(J.imp>1, 1, J.imp) -> J.impactada
        (Js[1:(length(id.impacto.b)),] * J.impactada[1:(length(id.impacto.b)),]) -> J.temp.t 
        as.matrix(J.temp.t)-> J.temp.t
        J.temp<- rbind(J.temp,J.temp.t)
        J.temp<- rbind(J.temp, max(Js))
        View(J.temp)
        
        filter=NULL
        filter2=NULL
        filter.env<- Matriz_river
        for (a in id.impacto.b){ 
          filter.t<- as.matrix(((exp(-1)/e)*min(dist.to.cont.river[,a]))) #change e to 2250, 3500 or 5000 for different propagation scenarios
          filter<-rbind(filter,filter.t)}
        ifelse(filter>1, 1.0, filter) -> filter2
        as.matrix(filter2)-> filter2
        filter2<- rbind(1, filter2, 1)
        filter2<- (1 - filter2) * (0.8)  
        for(a in id.impacto.b){(filter.env[id.spp.impactadas,a]) - (filter2[a,])-> filter.env[id.spp.impactadas,a]} 
        ifelse(filter.env==1, 0.999999, filter.env)-> filter.env
        View(filter.env)
        
        # Parallel computing
        if(parallel.imp==F){
          b<-list()
          for(f in repeticiones){
            b[[f]]<-H2020_Coalescent.and.lottery.exp.Kernel.J(Meta.pool = Meta.pool, m.pool = m.pool, 
                                                              Js = J.temp, id.module = id.module, filter.env = filter.env,
                                                              M.dist = M.dist, D50 = m, m.max = m.max, id.fixed = id.fixed, id.fixed2=id.fixed2,
                                                              D50.fixed = m, m.max.fixed = m.max.fixed, comm.fixed = comm.fixed, 
                                                              D50.fixed2 = m, m.max.fixed2 = m.max.fixed2, comm.fixed2 = comm.fixed2,
                                                              Lottery = Lottery, it = it, 
                                                              prop.dead.by.it = prop.dead.by.it, id.obs = id.obs)
            as.matrix(b[[f]])-> result.1
            out.t<-cbind("Nrep"=f,"D50"=m,"efecto.filtro"=e,"efecto.J"=c,"J.max"=max(J.temp),"J.min"=min(J.temp),result.1) 
            
            
          }  
          out<-rbind(out,out.t) 
        }
        
        if(parallel.imp==T){
          out.t<-foreach(f = 1:length(repeticiones), .combine=rbind)%dopar% {
            cat("va en dispersion: ", f, "\n")
            b<-H2020_Coalescent.and.lottery.exp.Kernel.J(Meta.pool = Meta.pool, m.pool = m.pool, 
                                                         Js = J.temp, id.module = id.module, filter.env = filter.env,
                                                         M.dist = M.dist, D50 = m, m.max = m.max, id.fixed = id.fixed, id.fixed2=id.fixed2,
                                                         D50.fixed = m, m.max.fixed = m.max.fixed, comm.fixed = comm.fixed, 
                                                         D50.fixed2 = m, m.max.fixed2 = m.max.fixed2, comm.fixed2 = comm.fixed2,
                                                         Lottery = Lottery, it = it, 
                                                         prop.dead.by.it = prop.dead.by.it, id.obs = id.obs)
            
            out.t<-cbind("Nrep"=f,"D50"=m,"efecto.filtro"=e,"efecto.J"=c,"J.max"=max(J.temp),"J.min"=min(J.temp),b) 
                       
          }
          out<-rbind(out,out.t) 
        }
      }
    }
  }
  out
}

####READY FOR SIMULATIONS 
#It will last a few days, parallel function reduce the simulating time, we recomend to leave 2 Cores with no use so
#operational use of the computer will go on
library(doParallel)
registerDoParallel(cores = detectCores()-2)

#########################################################################
#No pollution
dispersion <- c(35,85,200) 
cont.Filtro <- c(10) 
cont.J <- c(10)
repeticiones<-c(1:20)

River.10F<-impacto.2(Meta.pool=Meta.pool, m.pool=0.01, m.max=1,D50=m,
                     id.spp.impactadas=id.spp.impactadas,id.impacto.b = id.impacto.b,
                     Js, id.module= NULL, filter.env = Matriz_river, id.impacto.a = 1:517,
                     M.dist= M.distancia.river, id.fixed=id.fixed, D50.fixed =m, m.max.fixed = 1, comm.fixed=comm.fixed, 
                     id.fixed2 = id.fixed2, D50.fixed2 =m, m.max.fixed2 = 1, comm.fixed2 = comm.fixed2, 
                     Lottery=T , it =1000,
                     prop.dead.by.it = 0.05, id.obs =1:517, dist.to.cont= dist.to.cont.river,
                     dispersion = dispersion, cont.Filtro = cont.Filtro, cont.J = cont.J, parallel.imp = T)

################################################################################################
# Filter effect, e50 = 2250
dispersion <- c(35,85,200) 
cont.Filtro <- c(2250)
cont.J <- c(10)
repeticiones<-c(1:20)

River.2250F<-impacto.2(Meta.pool=Meta.pool, m.pool=0.01, m.max=1,D50=m,
                       id.spp.impactadas=id.spp.impactadas,id.impacto.b = id.impacto.b,
                       Js, id.module = NULL, filter.env = Matriz_river, id.impacto.a = 1:517,
                       M.dist= M.distancia.river, id.fixed=id.fixed, D50.fixed =m, m.max.fixed = 1, comm.fixed=comm.fixed, 
                       id.fixed2 = id.fixed2, D50.fixed2 =m, m.max.fixed2 = 1, comm.fixed2 = comm.fixed2, 
                       Lottery=T , it =1000,
                       prop.dead.by.it = 0.05, id.obs =1:517, dist.to.cont= dist.to.cont,
                       dispersion = dispersion, cont.Filtro = cont.Filtro, cont.J = cont.J, parallel.imp = T)

# Filter effect, e50 = 3500
dispersion <- c(35,85,200) 
cont.Filtro <- c(3500)
cont.J <- c(10)
repeticiones<-c(1:20)

River.3500F<-impacto.2(Meta.pool=Meta.pool, m.pool=0.01, m.max=1,D50=m,
                       id.spp.impactadas=id.spp.impactadas,id.impacto.b = id.impacto.b,
                       Js, id.module= NULL, filter.env = Matriz_river, id.impacto.a = 1:517,
                       M.dist= M.distancia.river, id.fixed=id.fixed, D50.fixed =m, m.max.fixed = 1, comm.fixed=comm.fixed, 
                       id.fixed2 = id.fixed2, D50.fixed2 =m, m.max.fixed2 = 1, comm.fixed2 = comm.fixed2, 
                       Lottery=T , it =1000,
                       prop.dead.by.it = 0.05, id.obs =1:517, dist.to.cont= dist.to.cont,
                       dispersion = dispersion, cont.Filtro = cont.Filtro, cont.J = cont.J, parallel.imp = T)


# Filter effect, e50 = 5000
dispersion <- c(35,85,200) 
cont.Filtro <- c(5000)
cont.J <- c(10)
repeticiones<-c(1:20)

River.5000F <-impacto.2(Meta.pool=Meta.pool, m.pool=0.01, m.max=1,D50=m,
                        id.spp.impactadas=id.spp.impactadas,id.impacto.b = id.impacto.b,
                        Js, id.module= NULL, filter.env = Matriz_river, id.impacto.a = 1:517,
                        M.dist= M.distancia.river, id.fixed=id.fixed, D50.fixed =m, m.max.fixed = 1, comm.fixed=comm.fixed, 
                        id.fixed2 = id.fixed2, D50.fixed2 =m, m.max.fixed2 = 1, comm.fixed2 = comm.fixed2, 
                        Lottery=T , it =1000,
                        prop.dead.by.it = 0.05, id.obs =1:517, dist.to.cont= dist.to.cont,
                        dispersion = dispersion, cont.Filtro = cont.Filtro, cont.J = cont.J, parallel.imp = T)

#############################################################################################
################################################################################################
# Abundance effect, e50 = 2250
dispersion <- c(35,85,200) 
cont.Filtro <- c(10)
cont.J <- c(2250)
repeticiones<-c(1:20)

River.2250J<-impacto.2(Meta.pool=Meta.pool, m.pool=0.01, m.max=1,D50=m,
                       id.spp.impactadas=id.spp.impactadas,id.impacto.b = id.impacto.b,
                       Js, id.module= NULL, filter.env = Matriz_river, id.impacto.a = 1:517,
                       M.dist= M.distancia.river, id.fixed=id.fixed, D50.fixed =m, m.max.fixed = 1, comm.fixed=comm.fixed, 
                       id.fixed2 = id.fixed2, D50.fixed2 =m, m.max.fixed2 = 1, comm.fixed2 = comm.fixed2, 
                       Lottery=T , it =1000,
                       prop.dead.by.it = 0.05, id.obs =1:517, dist.to.cont= dist.to.cont,
                       dispersion = dispersion, cont.Filtro = cont.Filtro, cont.J = cont.J, parallel.imp = T)

# Abundance effect, e50 = 3500
dispersion <- c(35,85,200) 
cont.Filtro <- c(10)
cont.J <- c(3500)
repeticiones<-c(1:20)

River.3500J<-impacto.2(Meta.pool=Meta.pool, m.pool=0.01, m.max=1,D50=m,
                       id.spp.impactadas=id.spp.impactadas,id.impacto.b = id.impacto.b,
                       Js, id.module= NULL, filter.env = Matriz_river, id.impacto.a = 1:517,
                       M.dist= M.distancia.river, id.fixed=id.fixed, D50.fixed =m, m.max.fixed = 1, comm.fixed=comm.fixed, 
                       id.fixed2 = id.fixed2, D50.fixed2 =m, m.max.fixed2 = 1, comm.fixed2 = comm.fixed2, 
                       Lottery=T , it =1000,
                       prop.dead.by.it = 0.05, id.obs =1:517, dist.to.cont= dist.to.cont,
                       dispersion = dispersion, cont.Filtro = cont.Filtro, cont.J = cont.J, parallel.imp = T)

# Abundance effect, e50 = 5000
dispersion <- c(35,85,200) 
cont.Filtro <- c(10)
cont.J <- c(5000)
repeticiones<-c(1:20)

River.5000J <-impacto.2(Meta.pool=Meta.pool, m.pool=0.01, m.max=1,D50=m,
                        id.spp.impactadas=id.spp.impactadas,id.impacto.b = id.impacto.b,
                        Js, id.module= NULL, filter.env = Matriz_river, id.impacto.a = 1:517,
                        M.dist= M.distancia.river, id.fixed=id.fixed, D50.fixed =m, m.max.fixed = 1, comm.fixed=comm.fixed, 
                        id.fixed2 = id.fixed2, D50.fixed2 =m, m.max.fixed2 = 1, comm.fixed2 = comm.fixed2, 
                        Lottery=T , it =1000,
                        prop.dead.by.it = 0.05, id.obs =1:517, dist.to.cont= dist.to.cont,
                        dispersion = dispersion, cont.Filtro = cont.Filtro, cont.J = cont.J, parallel.imp = T)


################################################
# Calculate vectors of filter and abudance 
# effect for each community (Stream landscape)
################################################

id.impacto.b<- c(2:516)
filter=NULL
filter2=NULL
filter.env<- Matriz_river
for (a in id.impacto.b){ 
  filter.t<- as.matrix(((exp(-1)/2250)*min(dist.to.cont.river[,a]))) #change e to 2250, 3500 or 5000 for different propagation scenarios
  filter<-rbind(filter,filter.t)}
ifelse(filter>1, 1.0, filter) -> filter2
as.matrix(filter2)-> filter2
filter2<- rbind(1, filter2, 1)
filter2<- (1 - filter2) * (0.8)  
for(a in id.impacto.b){(filter.env[id.spp.impactadas,a]) - (filter2[a,])-> filter.env[id.spp.impactadas,a]} 
ifelse(filter.env==1, 0.999999, filter.env)-> filter.env
filter.env[min(id.spp.impactadas),]-> River.impact.F2250


id.impacto.b<- c(2:516)
filter=NULL
filter2=NULL
filter.env<- Matriz_river
for (a in id.impacto.b){ 
  filter.t<- as.matrix(((exp(-1)/3500)*min(dist.to.cont.river[,a]))) #change e to 2250, 3500 or 5000 for different propagation scenarios
  filter<-rbind(filter,filter.t)}
ifelse(filter>1, 1.0, filter) -> filter2
as.matrix(filter2)-> filter2
filter2<- rbind(1, filter2, 1)
filter2<- (1 - filter2) * (0.8)  
for(a in id.impacto.b){(filter.env[id.spp.impactadas,a]) - (filter2[a,])-> filter.env[id.spp.impactadas,a]} 
ifelse(filter.env==1, 0.999999, filter.env)-> filter.env
filter.env[min(id.spp.impactadas),]-> River.impact.F3500


id.impacto.b<- c(2:516)
filter=NULL
filter2=NULL
filter.env<- Matriz_river
for (a in id.impacto.b){ 
  filter.t<- as.matrix(((exp(-1)/5000)*min(dist.to.cont.river[,a]))) #change e to 2250, 3500 or 5000 for different propagation scenarios
  filter<-rbind(filter,filter.t)}
ifelse(filter>1, 1.0, filter) -> filter2
as.matrix(filter2)-> filter2
filter2<- rbind(1, filter2, 1)
filter2<- (1 - filter2) * (0.8)  
for(a in id.impacto.b){(filter.env[id.spp.impactadas,a]) - (filter2[a,])-> filter.env[id.spp.impactadas,a]} 
ifelse(filter.env==1, 0.999999, filter.env)-> filter.env
filter.env[min(id.spp.impactadas),]-> River.impact.F5000



J.temp=max(Js)
J.imp=NULL
for (a in id.impacto.b){
  J.imp.t <- as.matrix(((exp(-1)/2250)*min(dist.to.cont.river[,a]))) 
  J.imp<- rbind(J.imp,J.imp.t)}
ifelse(J.imp>1, 1, J.imp) -> J.impactada
(Js[1:(length(id.impacto.b)),] * J.impactada[1:(length(id.impacto.b)),]) -> J.temp.t 
as.matrix(J.temp.t)-> J.temp.t
J.temp<- rbind(J.temp,J.temp.t)
J.temp<- rbind(J.temp, max(Js))
round(J.temp,0)-> J.temp
J.temp-> River.impact.J2250

J.temp=max(Js)
J.imp=NULL
for (a in id.impacto.b){
  J.imp.t <- as.matrix(((exp(-1)/3500)*min(dist.to.cont.river[,a]))) 
  J.imp<- rbind(J.imp,J.imp.t)}
ifelse(J.imp>1, 1, J.imp) -> J.impactada
(Js[1:(length(id.impacto.b)),] * J.impactada[1:(length(id.impacto.b)),]) -> J.temp.t 
as.matrix(J.temp.t)-> J.temp.t
J.temp<- rbind(J.temp,J.temp.t)
J.temp<- rbind(J.temp, max(Js))
round(J.temp,0)-> J.temp
J.temp-> River.impact.J3500

J.temp=max(Js)
J.imp=NULL
for (a in id.impacto.b){
  J.imp.t <- as.matrix(((exp(-1)/5000)*min(dist.to.cont.river[,a]))) 
  J.imp<- rbind(J.imp,J.imp.t)}
ifelse(J.imp>1, 1, J.imp) -> J.impactada
(Js[1:(length(id.impacto.b)),] * J.impactada[1:(length(id.impacto.b)),]) -> J.temp.t 
as.matrix(J.temp.t)-> J.temp.t
J.temp<- rbind(J.temp,J.temp.t)
J.temp<- rbind(J.temp, max(Js))
round(J.temp,0)-> J.temp
J.temp-> River.impact.J5000



#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################

#Data manipulation:

#Wetland landscape:

##################################################################
################ NO POLLUTION SCENARIO ###########################
##################################################################

#D50 = 35, mean and variance richness
apply(WWTP.10F[which(WWTP.10F[,2]==35),17:1906],2, mean)-> result.35.mean
apply(WWTP.10F[which(WWTP.10F[,2]==35),17:1906],2, var)-> result.35.var

#D50 = 85, mean and variance richness
apply(WWTP.10F[which(WWTP.10F[,2]==85),17:1906],2, mean)-> result.85.mean
apply(WWTP.10F[which(WWTP.10F[,2]==85),17:1906],2, var)-> result.85.var

#D50 = 200, mean and variance richness
apply(WWTP.10F[which(WWTP.10F[,2]==200),17:1906],2, mean)-> result.200.mean
apply(WWTP.10F[which(WWTP.10F[,2]==200),17:1906],2, var)-> result.200.var

##################################################################
################ Filter effect, e50= 2250 ########################
##################################################################

#D50 = 35, mean and variance richness
apply(WWTP.2250F[which(WWTP.2250F[,2]==35),17:1906],2, mean)-> result.F2250.35.mean
apply(WWTP.2250F[which(WWTP.2250F[,2]==35),17:1906],2, var)-> result.F2250.35.var

#D50 = 85, mean and variance richness
apply(WWTP.2250F[which(WWTP.2250F[,2]==85),17:1906],2, mean)-> result.F2250.85.mean
apply(WWTP.2250F[which(WWTP.2250F[,2]==85),17:1906],2, var)-> result.F2250.85.var

#D50 = 200, mean and variance richness
apply(WWTP.2250F[which(WWTP.2250F[,2]==200),17:1906],2, mean)-> result.F2250.200.mean
apply(WWTP.2250F[which(WWTP.2250F[,2]==200),17:1906],2, var)-> result.F2250.200.var


##################################################################
################ Filter effect, e50= 3500 ########################
##################################################################

#D50 = 35, mean and variance richness
apply(WWTP.2250F[which(WWTP.2250F[,2]==35),17:1906],2, mean)-> result.F3500.35.mean
apply(WWTP.2250F[which(WWTP.2250F[,2]==35),17:1906],2, var)-> result.F3500.35.var

#D50 = 85, mean and variance richness
apply(WWTP.2250F[which(WWTP.2250F[,2]==85),17:1906],2, mean)-> result.F3500.85.mean
apply(WWTP.2250F[which(WWTP.2250F[,2]==85),17:1906],2, var)-> result.F3500.85.var

#D50 = 200, mean and variance richness
apply(WWTP.2250F[which(WWTP.2250F[,2]==200),17:1906],2, mean)-> result.F3500.200.mean
apply(WWTP.2250F[which(WWTP.2250F[,2]==200),17:1906],2, var)-> result.F3500.200.var

##################################################################
################ Filter effect, e50= 5000 ########################
##################################################################

#D50 = 35, mean and variance richness
apply(WWTP.2250F[which(WWTP.2250F[,2]==35),17:1906],2, mean)-> result.F5000.35.mean
apply(WWTP.2250F[which(WWTP.2250F[,2]==35),17:1906],2, var)-> result.F5000.35.var

#D50 = 85, mean and variance richness
apply(WWTP.2250F[which(WWTP.2250F[,2]==85),17:1906],2, mean)-> result.F5000.85.mean
apply(WWTP.2250F[which(WWTP.2250F[,2]==85),17:1906],2, var)-> result.F5000.85.var

#D50 = 200, mean and variance richness
apply(WWTP.2250F[which(WWTP.2250F[,2]==200),17:1906],2, mean)-> result.F5000.200.mean
apply(WWTP.2250F[which(WWTP.2250F[,2]==200),17:1906],2, var)-> result.F5000.200.var

##################################################################
################ Abundance effect, e50= 2250 ########################
##################################################################

#D50 = 35, mean and variance richness
apply(WWTP.2250J[which(WWTP.2250J[,2]==35),17:1906],2, mean)-> result.J2250.35.mean
apply(WWTP.2250J[which(WWTP.2250J[,2]==35),17:1906],2, var)-> result.J2250.35.var

#D50 = 85, mean and variance richness
apply(WWTP.2250J[which(WWTP.2250J[,2]==85),17:1906],2, mean)-> result.J2250.85.mean
apply(WWTP.2250J[which(WWTP.2250J[,2]==85),17:1906],2, var)-> result.J2250.85.var

#D50 = 200, mean and variance richness
apply(WWTP.2250J[which(WWTP.2250J[,2]==200),17:1906],2, mean)-> result.J2250.200.mean
apply(WWTP.2250J[which(WWTP.2250J[,2]==200),17:1906],2, var)-> result.J2250.200.var


##################################################################
################ Abundance effect, e50= 3500 #####################
##################################################################

#D50 = 35, mean and variance richness
apply(WWTP.2250J[which(WWTP.2250J[,2]==35),17:1906],2, mean)-> result.J3500.35.mean
apply(WWTP.2250J[which(WWTP.2250J[,2]==35),17:1906],2, var)-> result.J3500.35.var

#D50 = 85, mean and variance richness
apply(WWTP.2250J[which(WWTP.2250J[,2]==85),17:1906],2, mean)-> result.J3500.85.mean
apply(WWTP.2250J[which(WWTP.2250J[,2]==85),17:1906],2, var)-> result.J3500.85.var

#D50 = 200, mean and variance richness
apply(WWTP.2250J[which(WWTP.2250J[,2]==200),17:1906],2, mean)-> result.J3500.200.mean
apply(WWTP.2250J[which(WWTP.2250J[,2]==200),17:1906],2, var)-> result.J3500.200.var

##################################################################
################ Abundance effect, e50= 5000 #####################
##################################################################

#D50 = 35, mean and variance richness
apply(WWTP.2250J[which(WWTP.2250J[,2]==35),17:1906],2, mean)-> result.J5000.35.mean
apply(WWTP.2250J[which(WWTP.2250J[,2]==35),17:1906],2, var)-> result.J5000.35.var

#D50 = 85, mean and variance richness
apply(WWTP.2250J[which(WWTP.2250J[,2]==85),17:1906],2, mean)-> result.J5000.85.mean
apply(WWTP.2250J[which(WWTP.2250J[,2]==85),17:1906],2, var)-> result.J5000.85.var

#D50 = 200, mean and variance richness
apply(WWTP.2250J[which(WWTP.2250J[,2]==200),17:1906],2, mean)-> result.J5000.200.mean
apply(WWTP.2250J[which(WWTP.2250J[,2]==200),17:1906],2, var)-> result.J5000.200.var





#Stream landscape:
##################################################################
################ NO POLLUTION SCENARIO ###########################
##################################################################

#D50 = 35, mean and variance richness
apply(River.10F[which(River.10F[,2]==35),17:533],2, mean)-> result.river.35.mean
apply(River.10F[which(River.10F[,2]==35),17:533],2, var)-> result.river.35.var

#D50 = 85, mean and variance richness
apply(River.10F[which(River.10F[,2]==85),17:533],2, mean)-> result.river.85.mean
apply(River.10F[which(River.10F[,2]==85),17:533],2, var)-> result.river.85.var

#D50 = 200, mean and variance richness
apply(River.10F[which(River.10F[,2]==200),17:533],2, mean)-> result.river.200.mean
apply(River.10F[which(River.10F[,2]==200),17:533],2, var)-> result.river.200.var

##################################################################
################ Filter effect, e50= 2250 ########################
##################################################################

#D50 = 35, mean and variance richness
apply(River.2250F[which(River.2250F[,2]==35),17:533],2, mean)-> result.river.F2250.35.mean
apply(River.2250F[which(River.2250F[,2]==35),17:533],2, var)-> result.river.F2250.35.var

#D50 = 85, mean and variance richness
apply(River.2250F[which(River.2250F[,2]==85),17:533],2, mean)-> result.river.F2250.85.mean
apply(River.2250F[which(River.2250F[,2]==85),17:533],2, var)-> result.river.F2250.85.var

#D50 = 200, mean and variance richness
apply(River.2250F[which(River.2250F[,2]==200),17:533],2, mean)-> result.river.F2250.200.mean
apply(River.2250F[which(River.2250F[,2]==200),17:533],2, var)-> result.river.F2250.200.var


##################################################################
################ Filter effect, e50= 3500 ########################
##################################################################

#D50 = 35, mean and variance richness
apply(River.2250F[which(River.2250F[,2]==35),17:533],2, mean)-> result.river.F3500.35.mean
apply(River.2250F[which(River.2250F[,2]==35),17:533],2, var)-> result.river.F3500.35.var

#D50 = 85, mean and variance richness
apply(River.2250F[which(River.2250F[,2]==85),17:533],2, mean)-> result.river.F3500.85.mean
apply(River.2250F[which(River.2250F[,2]==85),17:533],2, var)-> result.river.F3500.85.var

#D50 = 200, mean and variance richness
apply(River.2250F[which(River.2250F[,2]==200),17:533],2, mean)-> result.river.F3500.200.mean
apply(River.2250F[which(River.2250F[,2]==200),17:533],2, var)-> result.river.F3500.200.var

##################################################################
################ Filter effect, e50= 5000 ########################
##################################################################

#D50 = 35, mean and variance richness
apply(River.2250F[which(River.2250F[,2]==35),17:533],2, mean)-> result.river.F5000.35.mean
apply(River.2250F[which(River.2250F[,2]==35),17:533],2, var)-> result.river.F5000.35.var

#D50 = 85, mean and variance richness
apply(River.2250F[which(River.2250F[,2]==85),17:533],2, mean)-> result.river.F5000.85.mean
apply(River.2250F[which(River.2250F[,2]==85),17:533],2, var)-> result.river.F5000.85.var

#D50 = 200, mean and variance richness
apply(River.2250F[which(River.2250F[,2]==200),17:533],2, mean)-> result.river.F5000.200.mean
apply(River.2250F[which(River.2250F[,2]==200),17:533],2, var)-> result.river.F5000.200.var

##################################################################
################ Abundance effect, e50= 2250 ########################
##################################################################

#D50 = 35, mean and variance richness
apply(River.2250J[which(River.2250J[,2]==35),17:533],2, mean)-> result.river.J2250.35.mean
apply(River.2250J[which(River.2250J[,2]==35),17:533],2, var)-> result.river.J2250.35.var

#D50 = 85, mean and variance richness
apply(River.2250J[which(River.2250J[,2]==85),17:533],2, mean)-> result.river.J2250.85.mean
apply(River.2250J[which(River.2250J[,2]==85),17:533],2, var)-> result.river.J2250.85.var

#D50 = 200, mean and variance richness
apply(River.2250J[which(River.2250J[,2]==200),17:533],2, mean)-> result.river.J2250.200.mean
apply(River.2250J[which(River.2250J[,2]==200),17:533],2, var)-> result.river.J2250.200.var


##################################################################
################ Abundance effect, e50= 3500 #####################
##################################################################

#D50 = 35, mean and variance richness
apply(River.2250J[which(River.2250J[,2]==35),17:533],2, mean)-> result.river.J3500.35.mean
apply(River.2250J[which(River.2250J[,2]==35),17:533],2, var)-> result.river.J3500.35.var

#D50 = 85, mean and variance richness
apply(River.2250J[which(River.2250J[,2]==85),17:533],2, mean)-> result.river.J3500.85.mean
apply(River.2250J[which(River.2250J[,2]==85),17:533],2, var)-> result.river.J3500.85.var

#D50 = 200, mean and variance richness
apply(River.2250J[which(River.2250J[,2]==200),17:533],2, mean)-> result.river.J3500.200.mean
apply(River.2250J[which(River.2250J[,2]==200),17:533],2, var)-> result.river.J3500.200.var

##################################################################
################ Abundance effect, e50= 5000 #####################
##################################################################

#D50 = 35, mean and variance richness
apply(River.2250J[which(River.2250J[,2]==35),17:533],2, mean)-> result.river.J5000.35.mean
apply(River.2250J[which(River.2250J[,2]==35),17:533],2, var)-> result.river.J5000.35.var

#D50 = 85, mean and variance richness
apply(River.2250J[which(River.2250J[,2]==85),17:533],2, mean)-> result.river.J5000.85.mean
apply(River.2250J[which(River.2250J[,2]==85),17:533],2, var)-> result.river.J5000.85.var

#D50 = 200, mean and variance richness
apply(River.2250J[which(River.2250J[,2]==200),17:533],2, mean)-> result.river.J5000.200.mean
apply(River.2250J[which(River.2250J[,2]==200),17:533],2, var)-> result.river.J5000.200.var

