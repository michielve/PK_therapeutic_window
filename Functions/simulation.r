simulation <- function(inp,mod){
  

  
  
  ###############################
  ## Simulation info
  
  dose <- as.numeric(inp$dose)
  


  ## Oral or IV
  if(inp$admin == 'oral'){
    oral = T
  }else{
    oral=F
  }
  
  

  
  ###################################################
  ############# Set Dosing objects
  
  ## Oral dose
  if(oral){
    data <-  as.data.frame(ev(ID=1,ii=inp$interval, cmt=1, addl=999, amt=dose*inp$F, rate = 0,time=0,evid=1)) 
  }else{
    ## IV BOLUS
    data <-  as.data.frame(ev(ID=1,ii=inp$interval, cmt=2, addl=999, amt=dose, rate = 0,time=0,evid=1)) 
  }
  
  
  

  ## Set parameters in dataset
  
  data$TVKA <- inp$ka
  data$TVCL <- inp$cl
  data$TVVC <- inp$vd*inp$wgt

  
    #################################################################
    ###### Perform simulation
    df <- mod %>%
      data_set(data) %>%
      mrgsim(obsonly=T,end=(inp$sim_time*24),delta=0.1) %>% as.data.frame() 
    
    
    
  hline_1 <- 300
  hline_2 <- 1000
  
  
  
  ## Sum stats
  df_summary <- df%>%
    filter(time > (inp$sim_time*24 -inp$interval)) %>% # Select last interval
    mutate(above=ifelse(CONCENTRATION*1000 > hline_1 &CONCENTRATION*1000 < hline_2,TRUE,FALSE))
  
  time_within_range <- paste(round(mean(df_summary$above)*100,1),"%")
  
    ################################### Graphs
    ## Create concentratoin time profile without placebo data
    p1<-ggplot(df, aes(x=time,y=CONCENTRATION*1000,group=ID)) +
 
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = hline_1, ymax = hline_2, fill = "palegreen", alpha = 0.2) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = hline_2, ymax = Inf, fill = "red", alpha = 0.2) +
      
    
      geom_line(size=1) +
      
      ylab(paste("Concentration (ng/mL)",sep=""))+
      xlab("Time after start treatment (days)")+
      theme_bw(base_size = 14)+
      theme(legend.position="none")+
      scale_x_continuous(breaks=seq(0,(inp$sim_time*24),24),labels=seq(0,inp$sim_time,1),expand=c(0,0))+
      #scale_y_continuous(limits=c(0,1500),expand=c(0,0))+
      
      geom_hline(yintercept = hline_1,lty='dashed')+
      geom_text(aes((inp$sim_time*24 - 2),hline_1,label = 'Minimal effective concentration', vjust = 1, hjust=1),size=5,check_overlap = T)+
    
      geom_hline(yintercept = hline_2,lty='dashed',col='red')+
      geom_text(aes((inp$sim_time*24-2),hline_2,label = 'Maximal tolerable concentration', vjust = 1, hjust=1),size=5,check_overlap = T)+
      ggtitle("Time within therapeutic window at steady state:",time_within_range)
      
      
    
    
    
    ## Plot log y axis
    if(inp$plotylog == 'yes'){
    p2 <- p1 + scale_y_log10() + annotation_logticks(sides = "l") 
    
    return_object <- p2
    
    }else{
      return_object <- p1
      
    }
    
    
    
  
  
  return(return_object)
  
  
  
  
  
  
  
  
  
  
  
}