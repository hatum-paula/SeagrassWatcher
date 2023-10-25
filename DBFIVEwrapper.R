# --------------------------------------------------------------------------------------------------
#
# createEvidScenario
#
# --------------------------------------------------------------------------------------------------
# Use this function to create evidence for a scenario
# Arguments:
# I is DBFIVE object "I"
# tend = length of simulation window
# dreadgeduration & heatduration is duration of stress event in months
# dredgelight is light level, probability of above saturation light, during dredging event
# heatstress is temperature level, probability of optimal temperature, during heat stress
# dreadgestart & heatstart = month of stress event start specified as 1-12 (Jan-Dec)
#
# Returns DBFIVE evid_t structure
# 
# Assumes: starting month of simulation (not heat stress) is always Jan
# Assumes: if baseline temperature is worse than heat stress temperature, use baseline temperature 

createEvidScenario = function(I,tend,heatduration,heatstress,heatstart,
                              dredgeduration,dredgelight,dredgestart,site) {
  evid_t = vector('list',length(I$nnames)*tend)
  dim(evid_t) = c(tend,length(I$nnames))
  
  # Add genera and location
  if(site==1) {# Zostera Gladstone
    evid_t[,I$nnames %in% 'Genera_Presence'] = list(c(0,0,0,0,0,0,0,0,0,0,1,0,0))
    evid_t[,I$nnames %in% 'Location_Type'] = list(c(0,0,0,0,0,0,0,1,0,0,0,0))
    evid_t[1,I$nnames%in%'Seed_Density']=list(c(0.75,0.25,0))
    light = c(0.6613,0.5536,0.5806,0.8667,0.4677,0.4667,0.8226,0.9032,0.9167,0.9677,0.95,0.9194)
    temperature = c(0.06,0.04,0.1,0.2,0.97,1,1,1,1,0.84,0.37,0) 
    heat = rep(0, 12) 
  } else {
    stop('This site is not supported; please choose 1\n')
  }
  evid_t[,I$nnames %in% 'Immigrant_Seed_Density'] = list(c(0,0,1))
  
  # Add dredging (time masked)
  # ASSUME: no burial, sediment quality effect or light impact outside dredging windows
  evid_t[,I$nnames %in% 'Sediment_Quality'] = list(c(0,1))
  evid_t[,I$nnames%in%'Accumulated_Burial']= list(c(0,1))
  
  # Write temperature pattern
  tempbaseline = matrix(c(temperature,1-temperature),nrow=12,ncol=2)
  # Write heat pattern
  heatbaseline = matrix(c(heat,1-heat),nrow=12,ncol=2) 
  
  if(heatduration>0) { # protect against if no heat stress
    writeind = heatstart:(heatstart+heatduration-1) + 24 # where to write heatstress
    
    if(length(heatstress)==1) { # heatstress is a scalar
      heatstress = c(heatstress,1-heatstress)
      for(k in 1:length(writeind)) {
        x = min(tempbaseline[(writeind[k]+11)%%12+1,1],heatstress[1]) # Take min of baseline and heatstress
        evid_t[writeind[k],I$nnames %in% 'Temperature'] = list(c(x,1-x))
      }
      for(k in 1:length(writeind)) { # New
        x = min(tempbaseline[(writeind[k]+11)%%12+1,1],heatstress[1]) 
        evid_t[writeind[k],I$nnames %in% 'Heat_Stress'] =  list(c(1-x,x)) 
      }
    } else { # heatstress is a vector for Jan to Dec
      readind = (writeind-1) %% 12 + 1 # because sim always starts in Jan, convert writeind to months of yr
      for(k in 1:length(writeind)) {
        x = min(tempbaseline[(writeind[k]+11)%%12+1,1],heatstress[readind[k]]) 
        evid_t[writeind[k],I$nnames %in% 'Temperature'] = list(c(x,1-x))
      }
      for(k in 1:length(writeind)) { # New
        x = min(tempbaseline[(writeind[k]+11)%%12+1,1],heatstress[readind[k]]) 
        evid_t[writeind[k],I$nnames %in% 'Heat_Stress'] = list(c(1-x,x))
      } 
    }
    
    writeind = setdiff(1:tend,writeind) # For baseline/non-heatstress
  } else { # no heatstress
    writeind = 1:tend
  }
  
  for(k in 1:length(writeind)) { # Write baseline temperature; sim start always Jan, so readind=writeind
    evid_t[writeind[k],I$nnames %in% 'Temperature'] = list(tempbaseline[(writeind[k]+11)%%12+1,]) # K=1 (Jan), K=2 (Feb) (...)
  }
  for(k in 1:length(writeind)) { 
    evid_t[writeind[k],I$nnames %in% 'Heat_Stress'] = list(heatbaseline[(writeind[k]+11)%%12+1,]) 
  }
  
  # Write light pattern
  lightbaseline = matrix(c(light,1-light),nrow=12,ncol=2)
  
  if(dredgeduration>0) { # protect against if no dredging
    writeind = dredgestart:(dredgestart+dredgeduration-1) + 24 # where to write dredge light
    
    if(length(dredgelight)==1) { # dredgelight is a scalar
      lightdredge = c(dredgelight,1-dredgelight)
      for(k in 1:length(writeind)) {
        x = min(lightbaseline[(writeind[k]+11)%%12+1,1],lightdredge[1]) # Take min of baseline and dredge light
        evid_t[writeind[k],I$nnames %in% 'Accumulated_Light'] = list(c(x,1-x))
      }
    } else { # dredgelight is a vector for Jan to Dec
      readind = (writeind-1) %% 12 + 1 # because sim always starts in Jan, convert writeind to months of yr
      for(k in 1:length(writeind)) {
        x = min(lightbaseline[(writeind[k]+11)%%12+1,1],dredgelight[readind[k]]) # Take min of baseline and dredge light
        evid_t[writeind[k],I$nnames %in% 'Accumulated_Light'] = list(c(x,1-x))
      }
    }
    writeind = setdiff(1:tend,writeind) # For baseline/non-dredging
  } else { # no dredging
    writeind = 1:tend
  }
  
  for(k in 1:length(writeind)) { # Write baseline light; sim start always Jan, so readind=writeind
    evid_t[writeind[k],I$nnames %in% 'Accumulated_Light'] = list(lightbaseline[(writeind[k]+11)%%12+1,])
  }
  
  # Write time of year pattern
  for(k in 1:tend) {
    x = rep(0,12)
    x[(k+11)%%12+1] = 1 # because simulations always start with Jan
    evid_t[k,I$nnames %in% 'Time_of_Year'] = list(x)
  }
  return(evid_t)
}


# --------------------------------------------------------------------------------------------------
# Get pshootd and xbarshootd from DBFIVE Y object
getpandxbar = function(I,Y) {
  # Extract pshootd and xbarshootd (calculated on median)
  pshootd = matrix(0,nrow=tend,ncol=I$numstates[I$nnames%in%'Realised_Shoot_Density'])
  for(k in 1:tend) {
    p = Y$J[[k,which(I$nnames%in%'Realised_Shoot_Density')]]$p
    s = Y$J[[k,which(I$nnames%in%'Realised_Shoot_Density')]]$s[[1]]
    pshootd[k,s] = p
  }
  xbarshootd = sapply(1:nrow(pshootd),function(X,pshootd,w)sum(w*pshootd[X,]),
                      pshootd=pshootd,w=c(0.905,0.505,0.105,0))
  thresh = c(81,100,21,80,1,20,0,0,1)/100
  threshmed = (thresh[c(2,4,6,8)]-thresh[c(1,3,5,7)])*0.5 + thresh[c(1,3,5,7)]
  xbarshootd = rowSums(pshootd[,1:4]*matrix(threshmed,nrow=nrow(pshootd),ncol=4,byrow=T),na.rm=T)
  xbarshootd = as.numeric(!(xbarshootd<=thresh[9]))*(1-pshootd[,4])*xbarshootd*100
  return(list(pshootd=pshootd,xbarshootd=xbarshootd))
}


# --------------------------------------------------------------------------------------------------
# Pre-compute and store baseline scenarios (private function)
#
# I is DBFIVE initialised object
prepbaselines = function(I,tend=96) {
  evid = list()
  Y = list()
  pandxbar = list()
  I = initupdateI(I,tend,maxdt=2)
  for(i in 1) { 
    evid[[i]]=createEvidScenario(I,tend=tend,heatduration=0,heatstress=0,heatstart=0,
                                 dredgeduration=0,dredgelight=0,dredgestart=0,i) # no heatstress
    # Update I with evidence from evid_t
    system.time({
      I = initdbfiveevid(evid_t=evid[[i]],I,tend=tend,maxdt=2)
      Y[[i]] = fwdonly(tend=tend,I$Jcpt,I$J,I$nodeorder2,I$cptaddch[1,],I$cptaddcht[1,],
                       I$lknodeorder,I$lkcptaddch,I$lkcptaddcht,I$nnames,I$statenames,I$W,I$evid_t)
      pandxbar[[i]] = getpandxbar(I,Y[[i]])
    })
  }  
  save(list=ls(),file='baselines.Rdata')
}


# --------------------------------------------------------------------------------------------------
# runDBFIVE
#
# Evidence E
# I = DBFIVE initialisation object
# tend = simulation length in months
# heatstart/dredgestart = month of disturbance commencement Jan-Dec (1-12)
# heatduration/dredgeduration = months of disturbance
# pandxbar = baselines obtained from prepbaselines function
runDBFIVE = function(E,I,tend=96,heatstart,heatduration,
                     dredgestart,dredgeduration,pandxbar) {
  # Run scenario
  system.time({
    I = initdbfiveevid(evid_t=E,I,tend=tend,maxdt=2)
    Y = fwdonly(tend=tend,I$Jcpt,I$J,I$nodeorder2,I$cptaddch[1,],I$cptaddcht[1,],
                I$lknodeorder,I$lkcptaddch,I$lkcptaddcht,I$nnames,I$statenames,I$W,I$evid_t)
  })
  # Compute resistance, recovery and persistence
  px = getpandxbar(I,Y)
  
  t_heat = heatstart + 24 + heatduration - 1
  t_dredge = dredgestart+ 24 + dredgeduration - 1
  t = max(sort(unique(c(t_heat, t_dredge))))
  dxbarratio = px$xbarshootd[t] # last month of disturbance
  if(dxbarratio==0 && pandxbar$xbarshootd[t]==0) {
    dxbarratio = 1 
  } else {
    dxbarratio = dxbarratio / pandxbar$xbarshootd[t] # last month of disturbance
  }
  
  # calculate recov which is time to recover to within 20% of baseline median xbar
  diff2 = (px$xbarshootd - 0.8*pandxbar$xbarshootd)>=0
  tr = length(diff2) - which.max(rev(!diff2)) + 1
  tr[tr>=nrow(diff2)] = NA # Filter cases where last FALSE is last element in diff
  tr[all(diff2)] = 0 # Filter cases with all TRUE (i.e. 0 recovery time)
  
  return(list(Y=Y,resistance=dxbarratio,recovery=tr,
              persistence=sum(px$pshootd[,4])/sum(pandxbar$pshootd[,4]),pshootd=px$pshootd,xbarshootd=px$xbarshootd))
}


# --------------------------------------------------------------------------------------------------
# runDBCPN
#
# d = DBPCN object
# Evidence E
# I = DBFIVE initialisation object
# tend = simulation length in months
# heatstart/dredgestart = month of disturbance commencement Jan-Dec (1-12)
# heatduration/dredgeduration = months of disturbance
# pandxbar = baselines obtained from prepbaselines function
runDBCPN = function(d,E,I,tend=96,heatstart,heatduration,
                    dredgestart,dredgeduration,pandxbar) {
  # Run scenario
  E2 = list() # update evidence
  l = 1;
  for(j in 1:ncol(E)) { # foreach node
    for(k in 1:tend) { # foreach time
      if(!is.null(E[[k,j]])) {
        E2[[l]] = list(stois=c(0,0,0,k-1,0),x=I$nnames[j],delta=E[[k,j]])
        l = l + 1
      }
    } # end foreach time
  } # end foreach node
  d$reset_cptstois_Pfbx()
  d$init_evid(E2)
  
  print(system.time(d$nhfwdback() ))
  
  # get p and xbar for shoot density
  px = getpandxbar2(d)
  
  # Create a vector of time values based on heat and dredge
  t_heat = heatstart + 24 + (0:heatduration) - 1
  t_dredge = dredgestart + 24 + (0:dredgeduration) - 1
  t = max(sort(unique(c(t_heat, t_dredge))))
  
  # Calculate dxbarratio based on the last month of disturbance
  dxbarratio = px$xbarshootd[t] # last month of disturbance
  if (dxbarratio == 0 && pandxbar$xbarshootd[t] == 0) {
    dxbarratio = 1 
  } else {
    dxbarratio = dxbarratio / pandxbar$xbarshootd[t] # last month of disturbance
  }
  
  # Calculate recov which is time to recover to within 20% of baseline median xbar
  diff2 = (px$xbarshootd - 0.8 * pandxbar$xbarshootd) >= 0
  tr = length(diff2) - which.max(rev(!diff2)) + 1
  tr[tr >= nrow(diff2)] = NA # Filter cases where last FALSE is last element in diff
  tr[all(diff2)] = 0 # Filter cases with all TRUE (i.e. 0 recovery time)
  
  return(list(d = d, resistance = dxbarratio, recovery = tr,
              persistence = sum(px$pshootd[, 4]) / sum(pandxbar$pshootd[, 4]), pshootd = px$pshootd, xbarshootd = px$xbarshootd))
}


# ------------------------------------------------------------------------------
# Get pshootd and xbarshootd from DBPCN object
getpandxbar2 = function(d) {
  # Obtain px from dbpcn object
  px = d$cpl("px")
  nodes = sapply(px,function(X)d$names[d$nodes[X$xstois+1]+1]) # node names vec
  sxr = sapply(px,function(X)d$statenames[d$nodes[X$xstois+1]+1])
  sxi = sapply(px,function(X)X$xkx[[1]]+1)
  sx = mapply(function(X,Y)X[Y],sxr,sxi) # extract list of state names foreach node-slice
  x = sapply(px,function(X)unlist(X$x)) # extract list of state probabilities foreach node-slice
  t = sapply(d$stois,function(X)X[4])
  t = unlist(mapply(function(X,Y)rep(Y,length(X)),sx,t)) # extract stois t foreach node-slice
  
  D = data.frame(Node = unlist(mapply(function(X,Y)rep(Y,length(X)),sx,nodes)),
                 State = unlist(sx), P = unlist(x), Slice = t)
  DR = D[D$Node %in% 'Realised_Shoot_Density',]
  
  # ggplot(D,aes(x=Slice,y=P,colour=State)) + geom_line() + facet_wrap(~Node)
  # Extract pshootd and xbarshootd (calculated on median)
  pshootd = dcast(DR,Slice~State,value.var='P',fun.aggregate=mean,fill=0)
  pshootd = pshootd[,setdiff(colnames(pshootd),'Slice')]
  
  thresh = c(81,100,21,80,1,20,0,0,1)/100
  threshmed = (thresh[c(2,4,6,8)]-thresh[c(1,3,5,7)])*0.5 + thresh[c(1,3,5,7)]
  xbarshootd = rowSums(pshootd[,1:4]*matrix(threshmed,nrow=nrow(pshootd),ncol=4,byrow=T),na.rm=T)
  xbarshootd = as.numeric(!(xbarshootd<=thresh[9]))*(1-pshootd[,4])*xbarshootd*100
  return(list(pshootd=pshootd,xbarshootd=xbarshootd))
}
