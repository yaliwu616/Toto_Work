#########################################################
##### Helper Functions Start #######################
getTotal <- function(data,index){
  
  if(index < 1 || index > ncol(data)){
    return("")
  }
  col <- data[,index]
  col <- suppressWarnings(as.numeric(col))
  if(all(is.na(col))){
    return("")
  }
  sum(col)
}

fix_names <- function(x){
  y <-  strsplit(x, ".", fixed = TRUE)
  map_chr(y, 1)
}  

# Helper Function Ends ############################
###############################################################################

###################################################################
retained_earnings_PTD <- function(df){
  
  Retained_Earnings_Opening <- c()

  for(i in seq_along(df$Fiscal_Period)){
    
    if( i == 1 ){
      # assume ppv alwasy put adjustment at the 1st entry 
      Retained_Earnings_Opening[i] <-  -(df$Ppv_Adjustment_Opening[1])
      
      
    }else {
        
        Retained_Earnings_Opening[i] <- sum(Retained_Earnings_Opening[i-1],
                                            df$PTD_Depn[i-1],
                                            df$PTD_Interest[i-1],
                                            df$PTD_ARO_Interest[i-1],
                                            df$PTD_TI_Amort[i-1],
                                            df$PTD_Rent_Payments[i-1],
                                            na.rm = TRUE)
    
    }
   }
  return(Retained_Earnings_Opening)
}

# Summary table YTD
retained_earnings_YTD <- function(df){
  
  Retained_Earnings_Opening <- c()

  for(i in seq_along(df$Fiscal_Period)){
    
    if( i == 1 ){
      # assume ppv alwasy put adjustment at the 1st entry 
      Retained_Earnings_Opening[i] <-  -(df$Ppv_Adjustment_Opening[1])
      
      
    }else if(df$Fiscal_Period[i] ==1){
        
        Retained_Earnings_Opening[i] <- sum(Retained_Earnings_Opening[i-1],
                                            df$YTD_Depn[i-1],
                                            df$YTD_Interest[i-1],
                                            df$YTD_ARO_Interest[i-1],
                                            df$YTD_TI_Amort[i-1],
                                            df$YTD_Rent_Payments[i-1],
                                            na.rm = TRUE)
    
    }else{
    
      Retained_Earnings_Opening[i] <-Retained_Earnings_Opening[i-1] 
     }
   }
  return(Retained_Earnings_Opening)
}
#######################################################################
retained_earnings_QTD <- function(df){
  
  Retained_Earnings_Opening <- c()

  for(i in seq_along(df$Fiscal_Quarter)){
    
    if(i == 1){
      
      Retained_Earnings_Opening[i] <- -(df$Ppv_Adjustment_Opening[1])
      
    }else if(df$quarter_row_num[i] ==1){
        
        Retained_Earnings_Opening[i] <- sum(Retained_Earnings_Opening[i-1],
                                            df$QTD_Depn[i-1],
                                            df$QTD_Interest[i-1],
                                            df$QTD_ARO_Interest[i-1],
                                            df$QTD_TI_Amort[i-1],
                                            df$QTD_Rent_Payments[i-1],
                                            na.rm = TRUE)
    
    }else{
    
      Retained_Earnings_Opening[i] <-Retained_Earnings_Opening[i-1] 
     }
   }
  return(Retained_Earnings_Opening)
}

#######################################################################
# For new lease forecasting 
rownumtoremove <- function(df){
  
  duplicatevalue <- df$Remaining_Lease_Term[duplicated(df$Remaining_Lease_Term)]
  
  if(is.null(duplicatevalue) == FALSE){
    position  <- c()
    for (i in seq_along(duplicatevalue)){
  
      rownum <- which(df$Remaining_Lease_Term == duplicatevalue[i])
  
      for (j in seq_along(rownum)){
        
        if(isTruthy(df$payment_date[rownum[j]]) & df$payment_date[rownum[j]] != df$lse_term_start_dt[rownum[j]]){
          
         #newdf <- df[-rownum[j],,drop = FALSE]
         position[i]  <- rownum[j]
        } else if (df$lse_term_start_dt[rownum[j]] < df$CORP_PD_STRT_DT[rownum[j]]){

         position[i]  <- rownum[j]
        } else { next}
        
      }
    }
  return(position)
  } else{ return(df) }
  
  #return(df)
}

###################################################################################################################
# calculate how many renew years based on user inputs
NoRenewYrs <- function(renew_option_years){
  
  if(!isTruthy(renew_option_years)){
    
    0
  } else if(length(unlist(strsplit(renew_option_years,","))) == 1){
    
    sum(as.numeric(renew_option_years))
    
  } else{sapply(strsplit(renew_option_years,","), function(x) sum(as.numeric(x)))}
  
}


#################################################################################################################
# remainLeaseTerm <- function(lease_new_end_yr, lockYear,lockPeriod, lease_new_end_period, PO_dt){
#   if(isTruthy(PO_dt) == FALSE){
  
#   total_remain_LeaseTerm <- length(newdf$Fiscal_Period)   #(lease_new_end_yr - lockYear)*13 + (13-lockPeriod)*1 - (13-lease_new_end_period)
  
#   remain_LeaseTerm <- seq(length(newdf$Fiscal_Period), 1, by=-1)
  
#   return(remain_LeaseTerm)
#   } else {
  
#     # Find termination period and year 
#       new_lease_end_dt <- PO_dt
#       position <- which(lease_pd_cal$CORP_PD_STRT_DT <= new_lease_end_dt & 
#                         lease_pd_cal$CORP_PD_END_DT >= new_lease_end_dt)
#       new_lease_end_yr <-lease_pd_cal$CORP_YR_NUM[position]
#       new_lease_end_period <- lease_pd_cal$CORP_PD_NUM[position]
#       total_remain_LeaseTerm <- (new_lease_end_yr - lockYear)*13 + (13-lockPeriod)*1 - (13-new_lease_end_period)
#       remain_LeaseTerm <- seq(total_remain_LeaseTerm, 1, by=-1)
#       return(remain_LeaseTerm)
#   }
  
# }
########################################################################################
# extend lease end year & period if there is a renew option 

# extend_yr_period <- function(lease_end_period, lease_end_yr, renew_yr){
#       startPeriod <- lease_end_period + 1 
#       startYear <-  lease_end_yr
#       y <- data.frame()
#       while(startYear <= sum(lease_end_yr, renew_yr)){
#            while(startPeriod <=13)
#               {
#                 Period <- startPeriod
#                 x <-  data.frame(fiscal_year = startYear, fiscal_period = Period)
#                 y <- rbind(y,x)
#                 if(startYear == sum(lease_end_yr, renew_yr) & startPeriod == lease_end_period){break}
#                 startPeriod <- startPeriod +1
#               }
#         startYear <- startYear + 1
#         startPeriod <-  1
#    
#       }
#       return(y)
# }

#########################################################################
# allow users can either put new lease end date or renew yr 
# extend_yr_period <- function(lease_end_dt, renew_yr, new_lease_end_dt_input){
  
#   if(isTruthy(renew_yr) == TRUE & renew_yr > 0){
    
#     lease_end_year <- lease_pd_cal$CORP_YR_NUM[which(lease_pd_cal$CORP_PD_STRT_DT <= lease_end_dt & 
#                                  lease_pd_cal$CORP_PD_END_DT >= lease_end_dt)]

#     lease_end_period <- lease_pd_cal$CORP_PD_NUM[which(lease_pd_cal$CORP_PD_STRT_DT <= lease_end_dt & 
#                                                          lease_pd_cal$CORP_PD_END_DT >= lease_end_dt)]
    
#     # in case of leap year --> use AddMonths function
#      if(isTruthy(new_lease_end_dt_input) == FALSE){
#           new_lease_end_dt <- DescTools::AddMonths(lease_end_dt, 12*renew_yr)  #lease_end_dt + lubridate::years(renew_yr) 
#      } else{ new_lease_end_dt <-  new_lease_end_dt_input }
#     position <- which(lease_pd_cal$CORP_PD_STRT_DT <= new_lease_end_dt & 
#                         lease_pd_cal$CORP_PD_END_DT >= new_lease_end_dt)
#     new_lease_end_yr <- lease_pd_cal$CORP_YR_NUM[position]
#     new_lease_end_period <- lease_pd_cal$CORP_PD_NUM[position]
    
#     startPeriod <- lease_end_period + 1 
#     #startYear <-  lubridate::year(lease_end_dt)
    
#     periods = (lease_end_period + 1):(lease_end_period + (renew_yr * 13)) %% 13
#     periods[periods == 0] = 13
#     #years = lubridate::year(lease_end_dt) + (((1:(length(periods))-1) + lease_end_period ) %/% 13) 
#     years = lease_end_year + (((1:(length(periods))-1) + lease_end_period ) %/% 13) 
    
#     #data.frame(startYear = years, Period = periods) %>% filter(!(startYear == new_lease_end_yr & Period > new_lease_end_period))
#     data.frame(startYear = years, Period = periods) %>% filter(!( (startYear == new_lease_end_yr & 
#                                                                Period > new_lease_end_period)|(startYear > new_lease_end_yr)))

#   } else {data.frame()}
# }

extend_yr_period <- function(lease_end_dt, renew_yr){
  
  if(isTruthy(renew_yr) == TRUE & renew_yr > 0){
    
    lease_end_year <- lease_pd_cal$CORP_YR_NUM[which(lease_pd_cal$CORP_PD_STRT_DT <= lease_end_dt & 
                                 lease_pd_cal$CORP_PD_END_DT >= lease_end_dt)]

    lease_end_period <- lease_pd_cal$CORP_PD_NUM[which(lease_pd_cal$CORP_PD_STRT_DT <= lease_end_dt & 
                                                         lease_pd_cal$CORP_PD_END_DT >= lease_end_dt)]
    
    # in case of leap year --> use AddMonths function
  
    new_lease_end_dt <- DescTools::AddMonths(lease_end_dt, 12*renew_yr)  #lease_end_dt + lubridate::years(renew_yr) 
    
    position <- which(lease_pd_cal$CORP_PD_STRT_DT <= new_lease_end_dt & 
                        lease_pd_cal$CORP_PD_END_DT >= new_lease_end_dt)
    new_lease_end_yr <- lease_pd_cal$CORP_YR_NUM[position]
    new_lease_end_period <- lease_pd_cal$CORP_PD_NUM[position]
    
    startPeriod <- lease_end_period + 1 
    #startYear <-  lubridate::year(lease_end_dt)
    
    periods = (lease_end_period + 1):(lease_end_period + (renew_yr * 13)) %% 13
    periods[periods == 0] = 13
    #years = lubridate::year(lease_end_dt) + (((1:(length(periods))-1) + lease_end_period ) %/% 13) 
    years = lease_end_year + (((1:(length(periods))-1) + lease_end_period ) %/% 13) 
    
    #data.frame(startYear = years, Period = periods) %>% filter(!(startYear == new_lease_end_yr & Period > new_lease_end_period))
    data.frame(startYear = years, Period = periods) %>% filter(!( (startYear == new_lease_end_yr & 
                                                               Period > new_lease_end_period)|(startYear > new_lease_end_yr)))

  } else {data.frame()}
}

############################################################################################################################################
extend_18_period <- function(lease_end_year, lease_end_period){
  
    startPeriod <- lease_end_period + 1
    
    periods = (lease_end_period + 1):(lease_end_period + 18) %% 13
    periods[periods == 0] = 13
   
    years = lease_end_year + (((1:(length(periods))-1) + lease_end_period ) %/% 13) 
    
    #data.frame(startYear = years, Period = periods) %>% filter(!(startYear == new_lease_end_yr & Period > new_lease_end_period))
    data.frame(startYear = years, Period = periods) 
}
###############################################################################################
#  Determine Payment date (monthly payment)

mon_pay_dt <- function(df, IFRSENDDT, nth_day, PO_dt){
  payment_date <- c()
  period_start_1 <- c()
  period_start_2 <- c()
  if(!isTruthy(PO_dt)){
        for(i in seq_along(df$Remaining_Lease_Term)){
          if(i==1){

          period_start_1[i] <- as.Date(paste(format(df$CORP_PD_STRT_DT[i],"%Y-%m"), 
                                            as.character(nth_day), sep="-"))
          
          period_start_2[i] <- as.Date(paste(format(df$CORP_PD_END_DT[i],"%Y-%m"), 
                                            as.character(nth_day), sep="-"))
          
          if(period_start_1[i] != period_start_2[i] & i <= max(df$Remaining_Lease_Term)){
            
            if((df$CORP_PD_END_DT[i] - df$CORP_PD_STRT_DT[i]) >28){
              
              
              payment_date[i] <-  as.Date(paste(format(df$CORP_PD_STRT_DT[i]+28,"%Y-%m"), 
                                                as.character(nth_day), sep="-"))
              
            }
            else{payment_date[i] <- as.Date(max(period_start_1[i], period_start_2[i]),origin="1970-01-01")}
            
          } else if(period_start_1[i] == period_start_2[i] & (period_start_1[i] == df$CORP_PD_STRT_DT[i])){
            
            payment_date[i] <- period_start_1[i]
          } 
          
          else {payment_date[i] <- NA}
          
        }
        
        else { 
                    period_start_1[i] <- as.Date(paste(format(df$CORP_PD_STRT_DT[i],"%Y-%m"), 
                                                      as.character(nth_day), sep="-"))
                    
                    period_start_2[i] <- as.Date(paste(format(df$CORP_PD_END_DT[i],"%Y-%m"), 
                                                      as.character(nth_day), sep="-"))
                    
                    if(period_start_1[i] != period_start_2[i] & i <= max(df$Remaining_Lease_Term) ){
                      
                           if((df$CORP_PD_END_DT[i] - df$CORP_PD_STRT_DT[i]) >28){
                        
                        
                                payment_date[i] <-  as.Date(paste(format(df$CORP_PD_STRT_DT[i]+28,"%Y-%m"), 
                                                          as.character(nth_day), sep="-"))

                                if(is.na(payment_date[i]) == FALSE & IFRSENDDT[i] > payment_date[i]){ payment_date[i] <- payment_date[i] }else{payment_date[i] <- NA}                        


                        
                               }
                           else{  payment_date[i] <- as.Date(max(period_start_1[i], period_start_2[i]),origin="1970-01-01")
                           
                              if(is.na(payment_date[i]) == FALSE & IFRSENDDT[i] > payment_date[i]){ payment_date[i] <- payment_date[i] }else{payment_date[i] <- NA} 

                              }
                      
                    } else if(period_start_1[i] == period_start_2[i] & (is.na(payment_date[i-1])==TRUE |
                                                                        (period_start_1[i] == df$CORP_PD_STRT_DT[i]))){
                      
                      payment_date[i] <- period_start_1[i]

                      if(is.na(payment_date[i]) == FALSE & IFRSENDDT[i] > payment_date[i]){ payment_date[i] <- payment_date[i] }else{payment_date[i] <- NA} 

                    } 
                    
                    # else if(is.na(payment_date[i]) == FALSE & unique(df$New_IFRS_END_DATE[i]) > payment_date[i] ){

                    #    payment_date[i] <- payment_date[i]   #added on 5/8/2019

                    # }
                    
                    
                    else {  payment_date[i] <- NA}     
              }
      }
    return(as.Date(payment_date, origin="1970-01-01"))
  } else{
     for(i in seq_along(df$Remaining_Lease_Term)){
          if(i==1){
          
          period_start_1[i] <- as.Date(paste(format(df$CORP_PD_STRT_DT[i],"%Y-%m"), 
                                            as.character(nth_day), sep="-"))
          
          period_start_2[i] <- as.Date(paste(format(df$CORP_PD_END_DT[i],"%Y-%m"), 
                                            as.character(nth_day), sep="-"))
          
          if(period_start_1[i] != period_start_2[i] ){
            
            if((df$CORP_PD_END_DT[i] - df$CORP_PD_STRT_DT[i]) >28){
              
              
              payment_date[i] <-  as.Date(paste(format(df$CORP_PD_STRT_DT[i]+28,"%Y-%m"), 
                                                as.character(nth_day), sep="-"))
              
            }
            else{payment_date[i] <- as.Date(max(period_start_1[i], period_start_2[i]),origin="1970-01-01")}
            
          } else if(period_start_1[i] == period_start_2[i] & (period_start_1[i] == df$CORP_PD_STRT_DT[i])){
            
            payment_date[i] <- period_start_1[i]
          } else {payment_date[i] <- NA}
          
        } else { 
                    period_start_1[i] <- as.Date(paste(format(df$CORP_PD_STRT_DT[i],"%Y-%m"), 
                                                      as.character(nth_day), sep="-"))
                   
                    period_start_2[i] <- as.Date(paste(format(df$CORP_PD_END_DT[i],"%Y-%m"), 
                                                      as.character(nth_day), sep="-"))
                    
                    if(period_start_1[i] != period_start_2[i]){
                      
                      if((df$CORP_PD_END_DT[i] - df$CORP_PD_STRT_DT[i]) >28){
                        
                        
                        payment_date[i] <-  as.Date(paste(format(df$CORP_PD_STRT_DT[i]+28,"%Y-%m"), 
                                                          as.character(nth_day), sep="-"))

                        if(is.na(payment_date[i]) == FALSE & IFRSENDDT[i] > payment_date[i]){ payment_date[i] <- payment_date[i] }else{payment_date[i] <- NA}                                    
                        
                      }else{payment_date[i] <- as.Date(max(period_start_1[i], period_start_2[i]),origin="1970-01-01")
                      
                        if(is.na(payment_date[i]) == FALSE & IFRSENDDT[i] > payment_date[i]){ payment_date[i] <- payment_date[i] }else{payment_date[i] <- NA}  
                      
                      }
                      
                    } else if(period_start_1[i] == period_start_2[i] & (is.na(payment_date[i-1])==TRUE |
                                                                        (period_start_1[i] == df$CORP_PD_STRT_DT[i]))){
                      
                      payment_date[i] <- period_start_1[i]

                      if(is.na(payment_date[i]) == FALSE & IFRSENDDT[i] > payment_date[i]){ payment_date[i] <- payment_date[i] }else{payment_date[i] <- NA}  

                    }
                    
                    # else if(is.na(payment_date[i]) == FALSE & unique(df$New_IFRS_END_DATE[i]) > payment_date[i] ){

                    #    payment_date[i] <- payment_date[i]   #added on 5/8/2019
                    
                    #      }
                         
                         else {  payment_date[i] <- NA}     
                         
                  }
      }
return(as.Date(payment_date, origin="1970-01-01"))
  }
}
#################################################################################
####################################################################################
# Other dates calculation
# 1) days in period 
# 2) days before /after period
# 3) count periods

updateDateColumns <- function(df){
  
  
  count_period <- seq(1, max(df$Remaining_Lease_Term), 1)
  nodaysinperiod <- as.numeric(df$CORP_PD_END_DT) - as.numeric(df$CORP_PD_STRT_DT) +1
  no_days_in_period_before_payment <- ifelse(is.na(df$payment_date) == TRUE,
                                             28,
                                             df$payment_date - as.Date(df$CORP_PD_STRT_DT)
  )
  no_days_in_period_after_payment <- as.numeric(nodaysinperiod) -
    as.numeric(no_days_in_period_before_payment)
  
  data.frame(count_period = count_period, nodaysinperiod = nodaysinperiod,
             no_days_in_period_before_payment = no_days_in_period_before_payment,
             no_days_in_period_after_payment = no_days_in_period_after_payment)
  
}

###################################################################################
# update interest rate if needed
newIR <- function(new_IR, df){
        if(isTruthy(new_IR) == FALSE){
          last(df$INTEREST_RATE)
        } else {new_IR}
}

########################################################################################
# For renew option, find the row number where should apply new lease payment rate
# findrownum <- function(noRenewOption, yearNo, old_end_yr, old_end_period, PeriodNo, addyear ){
  
#   if(isTruthy(noRenewOption) & noRenewOption >=1)
#   {
#     row_num_list <- c()
#     for ( i in seq(1,noRenewOption)){
#       if(i==1){
#         row_num_list[i] <-which(yearNo == old_end_yr & PeriodNo == old_end_period, arr.ind=TRUE)
#       } else { row_num_list[i] <-which(yearNo == sum(old_end_yr,addyear[i-1]) & PeriodNo == old_end_period, arr.ind=TRUE) }
#     }
#     return(row_num_list)
    
#   } else{0}
# }

findrownum <- function( yearNo, old_end_yr, old_end_period, PeriodNo, addyear ){
  
  if(isTruthy(addyear) & length(addyear) >=1 & all(addyear >0))
  {
    row_num_list <- c()
    for ( i in seq(1,length(addyear))){
      if(i==1){
        row_num_list[i] <-which(yearNo == old_end_yr & PeriodNo == old_end_period, arr.ind=TRUE)
      } else { row_num_list[i] <-which(yearNo == sum(old_end_yr, sum(addyear[1:i-1])) & PeriodNo == old_end_period, arr.ind=TRUE) }
    }
    return(row_num_list)
    
  } else{0}
}

# extend rownum and interest rate list to the length of newremain2 
# fun2 <- function(positionUpdateInterestRate,Fiscal_Year){
#   resulst <- c()
#   for (i in seq_along(positionUpdateInterestRate)){
#     if(i == 1)
#     {
#       for (j in seq_along(Fiscal_Year)){
        
#         if (positionUpdateInterestRate[i] >= j){ resulst[j] <- positionUpdateInterestRate[i]}
#         else {resulst[j] <- j}
        
#       }
#     }else {
#       for (j in seq(positionUpdateInterestRate[i-1]+1, length(Fiscal_Year))){
        
#         if (positionUpdateInterestRate[i] > j ){ resulst[j] <- x[i-1]}
#         else {resulst[j] <- x[i]}
        
#       }
#     }
    
#   }
#   return(resulst)
# }
fun2 <- function(position,Fiscal_Year){
  result <- c()
  if(length(position) >1 ){
    for (i in 1:(length(position)+1)){
      if(i == 1)
      { 
        for (j in seq(1, position[i])){ 
          result[j] <- position[i] }
      }else{
        
        for(j in seq(position[i-1], length(Fiscal_Year))){
          result[j] <- position[i-1]
          
        }
      }
    }
  }else if(length(position) == 1 ) {
    
    for (i in 1:length(Fiscal_Year)){
       result[i] <- position[1]    
    }
  } else{ 
    
    for (i in 1:length(Fiscal_Year)){
      result[i] <- i
      
    }
  }
  
  return(result)
}

###########################################################################
# add TI_receipt column for calculation 
# add PO Termination Payment for calculation

rowNumByDate_TI  <- function(#count_period,
                          #period_start_dt,
                          #period_end_dt,
                          TI_receipt_dt,
                          payment_date){
  
  if(isTruthy(TI_receipt_dt)){
    
    #position <- which(period_end_dt >= TI_receipt_dt & period_start_dt <= TI_receipt_dt)

    position <- which( payment_date == as.Date(paste(format(unique(TI_receipt_dt),"%Y-%m"), 
              as.character(1), sep="-")))
    
  } else {
    
    position <- NA
    
  }
  
  return(position)
}
###############################################################################################################
rowNumByDate_Impair  <- function(#count_period,
                          #period_start_dt,
                          #period_end_dt,
                          Impair_receipt_dt,
                          payment_date){
  
  if(isTruthy(Impair_receipt_dt)){
    
        #position <- which(period_end_dt >= TI_receipt_dt & period_start_dt <= TI_receipt_dt)

        position <- which( payment_date == as.Date(paste(format(unique(Impair_receipt_dt),"%Y-%m"), 
                  as.character(1), sep="-")))
        
        if(unique(Impair_receipt_dt) > payment_date[position]){

          position <-  position + 1

        }

  } else {
    
    position <- NA
    
  }
  
  return(position)
}

########################################################################
AdjAmtCal <- function(count_period, rowNumByDate, adjval){
  y <- c()  
  for(i in seq_along(count_period)){
    
    if(is.na(rowNumByDate) == FALSE & i == rowNumByDate){
      y[i] <- adjval
    } else{y[i] <- 0}
  } 
  return(y)
}


############################################################################
# Interest rate for lease payment 

# LP_interestRate <- function(irforlp){
  
#   if(!isTruthy(irforlp)){ 0 } else {as.numeric(unlist((strsplit(irforlp, split=","))))}
  
# }

LP_change <- function(Same_rent_change_in_all_renewals, addyear, irforlp, amtforlp){
  
 if(isTruthy(Same_rent_change_in_all_renewals) & Same_rent_change_in_all_renewals == 'N'){
    
      if(!isTruthy(irforlp) & !isTruthy(amtforlp)){ 0 
      
      } else if (isTruthy(irforlp) & !isTruthy(amtforlp)) {
        
        as.numeric(unlist((strsplit(irforlp, split=","))))
      
      } else{

        as.numeric(unlist((strsplit(amtforlp, split=","))))

      }
} else if(isTruthy(Same_rent_change_in_all_renewals) & Same_rent_change_in_all_renewals == 'Y' & all(addyear > 0) & isTruthy(addyear)) {

      if(!isTruthy(irforlp) & !isTruthy(amtforlp)){
        0
      } else if (isTruthy(irforlp) & !isTruthy(amtforlp)){

        rep(as.numeric(irforlp), length(addyear))

      } else {
      
        rep(as.numeric(amtforlp), length(addyear))

      }

} else {

  0
}
  
}


add_year <- function(renewyrs){
  
  if(!isTruthy(renewyrs)){ 0 } else {as.numeric(unlist((strsplit(renewyrs, split=","))))}
  
}
##########################################################################################
# IRfun <- function(position,Fiscal_Year,LPinterestRate){
#   result <- c()
#   if(length(position) >1 ){
#       for (i in 1:(length(position)+1)){
#         if(i == 1)
#         { 
#           for (j in seq(1, position[i]-1)){ 
#             result[j] <- 0 }
#         }else{
          
#           for(j in seq(position[i-1], length(Fiscal_Year))){
#             result[j] <- LPinterestRate[i-1]
            
#           }
#         }
#       }
#   }else if(length(position) == 1 ) {
  
#           for (i in 1:length(Fiscal_Year)){
#                 if(i < position[1]){
#                    result[i] <- 0 } else  {  result[i] <- LPinterestRate[1]  }   
#             }
#   } else{ 

#       for (i in 1:length(Fiscal_Year)){
#                result[i] <- 0 
                
#            }
#    }
  
#  return(result)
# }
#############################################################################################
IRfun <- function(position,Fiscal_Year,LP_change){
  result <- c()
  if(length(position) >1 ){
      for (i in 1:(length(position)+1)){
        if(i == 1)
        { 
          for (j in seq(1, position[i]-1)){ 
            result[j] <- 0 }
        }else{
          
          for(j in seq(position[i-1], length(Fiscal_Year))){
            result[j] <- LP_change[i-1]
            
          }
        }
      }
  }else if(length(position) == 1 ) {
  
          for (i in 1:length(Fiscal_Year)){
                if(i < position[1]){
                   result[i] <- 0 } else  {  result[i] <- LP_change[1]  }   
            }
  } else{ 

      for (i in 1:length(Fiscal_Year)){
               result[i] <- 0 
                
           }
   }
  
 return(result)
}

########################################################################
# Lease payment 
LPfun_rate <- function(old_lease_payment, payment_date, nodaysinperiod, rownum, addition_LP){
  Lease_Payment <-  c()  
  if( !isTruthy(addition_LP) | all(addition_LP == 0) == FALSE )  #is.na(interestRate[1]) == FALSE )
  {
    for (i in seq_along(old_lease_payment)){
      
      
      # if rownum[i] = 1
      if(rownum[i] == 1 ){
        
          if(is.na(payment_date[i]) == TRUE){
            
            Lease_Payment[i] <-  0
          } else if(nodaysinperiod[i] ==28){
            #Lease_Payment[i] <- (1+interestRate[i]/100)*(remain$Financial_Liability_Lease_Payment[length(remain$Financial_Liability_Lease_Payment)-1])
            Lease_Payment[i] <- (1+addition_LP[i]/100)*(old_lease_payment[length(old_lease_payment)-1])
          } else{
            Lease_Payment[i] <- 2*(1+addition_LP[i]/100)*(old_lease_payment[length(old_lease_payment)-1])
            #Lease_Payment[i] <- 2*(1+interestRate[i]/100)*(remain$Financial_Liability_Lease_Payment[length(remain$Financial_Liability_Lease_Payment)-1])
       
          }
      } 
    
       else if(i < rownum[i]){
          Lease_Payment[i] <-  old_lease_payment[i]
        }
        else if(is.na(payment_date[i]) == TRUE)
        {
          
          Lease_Payment[i] <-  0
          
        } else if (nodaysinperiod[i] ==28){
           
             if(Lease_Payment[rownum[i] - 2] !=0){
          
              Lease_Payment[i] <- Lease_Payment[rownum[i] - 2]*(1+addition_LP[i]/100) }else{
              
              Lease_Payment[i] <- Lease_Payment[rownum[i] - 3]*(1+addition_LP[i]/100)
            }
        } else { 
          
             if( Lease_Payment[rownum[i] - 2] !=0){
              Lease_Payment[i] <- Lease_Payment[rownum[i] - 2]*(1+addition_LP[i]/100)*2 }else{
              
              Lease_Payment[i] <- Lease_Payment[rownum[i] - 3]*(1+addition_LP[i]/100)*2 
            }
        }
      }
      return(Lease_Payment) 
    } else {
      return(old_lease_payment)
    }
}

#######################################################################################################################################
LPfun_amt <- function(old_lease_payment, payment_date, nodaysinperiod, rownum, addition_LP){
  Lease_Payment <-  c()  
  if( !isTruthy(addition_LP) | all(addition_LP == 0) == FALSE )  #is.na(interestRate[1]) == FALSE )
  {
    for (i in seq_along(old_lease_payment)){
      
      
      # if rownum[i] = 1
      if(rownum[i] == 1 ){
        
          if(is.na(payment_date[i]) == TRUE){
            
            Lease_Payment[i] <-  0
          } else if(nodaysinperiod[i] ==28){
            #Lease_Payment[i] <- (1+interestRate[i]/100)*(remain$Financial_Liability_Lease_Payment[length(remain$Financial_Liability_Lease_Payment)-1])
            Lease_Payment[i] <- (old_lease_payment[length(old_lease_payment)-1]) + (-addition_LP[i])   #(1+interestRate[i]/100)*(old_lease_payment[length(old_lease_payment)-1])
          } else{
            Lease_Payment[i] <- 2*((old_lease_payment[length(old_lease_payment)-1])+ (-addition_LP[i]))
            #Lease_Payment[i] <- 2*(1+interestRate[i]/100)*(remain$Financial_Liability_Lease_Payment[length(remain$Financial_Liability_Lease_Payment)-1])
       
          }
      } 
    
       else if(i < rownum[i]){
          Lease_Payment[i] <-  old_lease_payment[i]
        }
        else if(is.na(payment_date[i]) == TRUE)
        {
          
          Lease_Payment[i] <-  0
          
        } else if (nodaysinperiod[i] ==28){
           
             if(Lease_Payment[rownum[i] - 2] !=0){
          
              Lease_Payment[i] <- Lease_Payment[rownum[i] - 2] + (-addition_LP[i]) }else{
              
              Lease_Payment[i] <- Lease_Payment[rownum[i] - 3] + (-addition_LP[i])
            }
        } else { 
          
             if( Lease_Payment[rownum[i] - 2] !=0){
              Lease_Payment[i] <- Lease_Payment[rownum[i] - 2] + (-addition_LP[i]) }else{
              
              Lease_Payment[i] <- Lease_Payment[rownum[i] - 3] + (-addition_LP[i])
            }
        }
      }
      return(Lease_Payment) 
    } else {
      return(old_lease_payment)
    }
}
#####################################################################################################################
# new_lease_pay <- function(df){
  
#   Lease_Payment <-  c()  
#   position <- which(df$CORP_PD_STRT_DT <= df$ASSET_IN_SERV_DT &
#                       df$CORP_PD_END_DT >= df$ASSET_IN_SERV_DT )
  
#   for(i in seq_along(df$count_period)){
#      if(i< position){
       
#        Lease_Payment[i] <- 0
       
#      }else if(i == position){
       
#        Lease_Payment[i] <- df$monthly_pay[i]
       
#      } else if((is.na(df$payment_date[i]) == TRUE & is.na(df$lse_term_start_dt[i]) == FALSE & df$lse_term_start_dt[i] <= df$CORP_PD_STRT_DT[i])| 
#               (is.na(df$payment_date[i]) == FALSE & is.na(df$lse_term_start_dt[i]) == FALSE & df$lse_term_end_dt[i] < df$payment_date[i])|
#               (is.na(df$lse_term_start_dt[i]) == TRUE)
#               ){
       
#        Lease_Payment[i] <- 0
       
#      } else if(df$nodaysinperiod[i] ==28){
       
#         Lease_Payment[i] <- df$monthly_pay[i]
        
#     } else { Lease_Payment[i] <- df$monthly_pay[i]*2}
  
    
#   }
#   return(Lease_Payment)
  
# }
#########################################################################################################
# lease payment depends on the recurr cost sheet 
new_lease_pay <- function(df, min_lse_pay_term_start_dt){
  
  Lease_Payment <-  c()  

  position <- which(df$CORP_PD_STRT_DT <= min_lse_pay_term_start_dt &
                     df$CORP_PD_END_DT >= min_lse_pay_term_start_dt )

  for(i in seq_along(df$count_period)){

     if(i< position){
       
       Lease_Payment[i] <- 0
       
     }else if(i == position){
       
       Lease_Payment[i] <- df$monthly_pay[i]
       
     } else if((is.na(df$payment_date[i]) == TRUE & is.na(df$lse_term_start_dt[i]) == FALSE & df$lse_term_start_dt[i] <= df$CORP_PD_STRT_DT[i])| 
              (is.na(df$payment_date[i]) == FALSE & is.na(df$lse_term_start_dt[i]) == FALSE & df$lse_term_end_dt[i] < df$payment_date[i])|
              (is.na(df$lse_term_start_dt[i]) == TRUE)
              ){
       
       Lease_Payment[i] <- 0
       
     } else if(df$nodaysinperiod[i] ==28){
       
        Lease_Payment[i] <- df$monthly_pay[i]
        
    } else { Lease_Payment[i] <- df$monthly_pay[i]*2}
  
    
  }
  return(Lease_Payment)
  
}
#########################################################################################
LiaAdj <-  function(countPeriods, PV, PriorEndLia){
  Lia_Adj <- c()
  for (i in seq_along(countPeriods))
  {
    if(i==1){
      Lia_Adj[i] <- sum(PV, na.rm=TRUE) - PriorEndLia
      
    } else {Lia_Adj[i] <- 0}
  }
  return(Lia_Adj)
}


lease_interest_exp <- function(countPeriods, PriorEndLia, LiaAdj, ir, noDaysinPeriodBeforePay,
                               noDaysinPeriod, LeasePayment, noDaysinPeriodAfterPay,
                               TI_receipt, PO_termination_payment ){
  #interest <-  c()  
  a <- c()
  b <- c()
  c <- c()
  d <- c()
  mylist <- list("interest" = c(), "Liability_Opening" = c(), 
                 "Liability_Ending" = c()
  )
  for (i in seq_along(countPeriods)){
    if(i==1){
      mylist$Liability_Opening[i] <- PriorEndLia
      a[i] <-  mylist$Liability_Opening[i] + LiaAdj[i]
      b[i] <- a[i]*ir[i]/100/13 *as.numeric(noDaysinPeriodBeforePay[i])/as.numeric(noDaysinPeriod[i])   
      c[i] <- mylist$Liability_Opening[i] + LiaAdj[i] - abs(LeasePayment[i]) - (-abs(TI_receipt[i])) - abs(PO_termination_payment[i]) 
      d[i] <- c[i]*ir[i]/100/13 *as.numeric(noDaysinPeriodAfterPay[i])/as.numeric(noDaysinPeriod[i])
      mylist$interest[i] <- b[i] + d[i]
      mylist$Liability_Ending[i] <- mylist$Liability_Opening[i] + LiaAdj[i] - abs(LeasePayment[i]) + mylist$interest[i]- (-abs(TI_receipt[i]))- abs(PO_termination_payment[i]) 
    }else{
      mylist$Liability_Opening[i] <- mylist$Liability_Ending[i-1]
      a[i] <-  mylist$Liability_Opening[i] + LiaAdj[i]
      b[i] <- a[i]*ir[i]/100/13 *as.numeric(noDaysinPeriodBeforePay[i])/as.numeric(noDaysinPeriod[i])   
      c[i] <- mylist$Liability_Opening[i] + LiaAdj[i] - abs(LeasePayment[i]) - (-abs(TI_receipt[i])) - abs(PO_termination_payment[i]) 
      d[i] <- c[i]*ir[i]/100/13 *as.numeric(noDaysinPeriodAfterPay[i])/as.numeric(noDaysinPeriod[i])
      mylist$interest[i] <- b[i] + d[i]
      mylist$Liability_Ending[i] <- mylist$Liability_Opening[i] + LiaAdj[i] - abs(LeasePayment[i]) + mylist$interest[i]- (-abs(TI_receipt[i]))- abs(PO_termination_payment[i]) 
    }
  } 
  return(mylist)
}
##############################################################################################################################################################
new_lease_interest_exp <- function(countPeriods, PV, LiaAdj, ir, noDaysinPeriodBeforePay,
                               noDaysinPeriod, LeasePayment, noDaysinPeriodAfterPay,
                               TI_receipt, PO_termination_payment ){
  #interest <-  c()  
  a <- c()
  b <- c()
  c <- c()
  d <- c()
  mylist <- list("interest" = c(), "Liability_Opening" = c(), 
                 "Liability_Ending" = c()
  )
  for (i in seq_along(countPeriods)){
    if(i==1){
      mylist$Liability_Opening[i] <- sum(PV,na.rm=TRUE)
      a[i] <-  mylist$Liability_Opening[i] + LiaAdj[i]
      b[i] <- a[i]*ir[i]/100/13 *as.numeric(noDaysinPeriodBeforePay[i])/as.numeric(noDaysinPeriod[i])   
      c[i] <- mylist$Liability_Opening[i] + LiaAdj[i] - abs(LeasePayment[i]) - (-abs(TI_receipt[i])) - abs(PO_termination_payment[i]) 
      d[i] <- c[i]*ir[i]/100/13 *as.numeric(noDaysinPeriodAfterPay[i])/as.numeric(noDaysinPeriod[i])
      mylist$interest[i] <- b[i] + d[i]
      mylist$Liability_Ending[i] <- mylist$Liability_Opening[i] + LiaAdj[i] - abs(LeasePayment[i]) + mylist$interest[i]- (-abs(TI_receipt[i]))- abs(PO_termination_payment[i]) 
    }else{
      mylist$Liability_Opening[i] <- mylist$Liability_Ending[i-1]
      a[i] <-  mylist$Liability_Opening[i] + LiaAdj[i]
      b[i] <- a[i]*ir[i]/100/13 *as.numeric(noDaysinPeriodBeforePay[i])/as.numeric(noDaysinPeriod[i])   
      c[i] <- mylist$Liability_Opening[i] + LiaAdj[i] - abs(LeasePayment[i]) - (-abs(TI_receipt[i])) - abs(PO_termination_payment[i]) 
      d[i] <- c[i]*ir[i]/100/13 *as.numeric(noDaysinPeriodAfterPay[i])/as.numeric(noDaysinPeriod[i])
      mylist$interest[i] <- b[i] + d[i]
      mylist$Liability_Ending[i] <- mylist$Liability_Opening[i] + LiaAdj[i] - abs(LeasePayment[i]) + mylist$interest[i]- (-abs(TI_receipt[i]))- abs(PO_termination_payment[i]) 
    }
  } 
  return(mylist)
}
#########################################################################################
PV_caln <- function(lease_payment,TI_receipt,ir,PO_termination_payment,count_period,
                    daysbeforepay, daysinperiod ){
  pv_amt <- c()
  a <- c()
  b <- c()
  for(i in seq_along(lease_payment)){
    a[i] <- abs(lease_payment[i])+(-abs(TI_receipt[i]))+abs(PO_termination_payment[i])
    b[i] <- (1 + ir[i]/100/13)^(count_period[i]-1+daysbeforepay[i]/daysinperiod[i])
    pv_amt[i] <- a[i]*1/b[i]
  }
  return(pv_amt)
}
########################################################################################
ARO <- function(PriorAROEnding, AROinitialValue, ir, remainLeaseTerm ){
  
  mylist <- list("Aro_interest" = c(), "ARO_Opening" = c(), 
                 "ARO_Ending" = c(), "revalue_aro" = c())

  if(isTruthy(PriorAROEnding)){               
  for (i in seq_along(remainLeaseTerm)){
    if(i==1){
      AROinitialValue <- ifelse(isTruthy(AROinitialValue) == FALSE,   #is.na(AROinitialValue) == TRUE, 
                               PriorAROEnding , #abs(last(change$ARO_Ending)), 
                               AROinitialValue)

      mylist$ARO_Opening[i] <- PriorAROEnding
      mylist$revalue_aro[i] <- AROinitialValue *(1/(1+ir[i]/100/13)^remainLeaseTerm[i]) -  abs(mylist$ARO_Opening[i])
      
      mylist$Aro_interest[i] <- (abs(mylist$ARO_Opening[i]) + mylist$revalue_aro[i])*(ir[i]/100/13)
      mylist$ARO_Ending[i] <- abs(mylist$ARO_Opening[i]) + mylist$revalue_aro[i] + abs(mylist$Aro_interest[i])
      
    }else{
      
      mylist$ARO_Opening[i] <- mylist$ARO_Ending[i-1]
      mylist$revalue_aro[i] <- 0
      
      mylist$Aro_interest[i] <- (abs(mylist$ARO_Opening[i]) + mylist$revalue_aro[i])*(ir[i]/100/13)
      mylist$ARO_Ending[i] <- mylist$ARO_Opening[i] + mylist$revalue_aro[i] + mylist$Aro_interest[i]     
    }
  }
}
# for ARO show up at 1st time 
else {
    for (i in seq_along(remainLeaseTerm)){
    if(i==1){
      AROinitialValue <- ifelse(isTruthy(AROinitialValue) == FALSE,   #is.na(AROinitialValue) == TRUE, 
                               0, AROinitialValue)

      mylist$ARO_Opening[i] <- AROinitialValue * (1/(1+ir[i]/100/13)^max(remainLeaseTerm))
      #mylist$revalue_aro[i] <- AROinitialValue *(1/(1+ir[i]/100/13)^remainLeaseTerm[i]) -  abs(mylist$ARO_Opening[i])
      mylist$revalue_aro[i] <- 0
      mylist$Aro_interest[i] <- (abs(mylist$ARO_Opening[i]) + mylist$revalue_aro[i])*(ir[i]/100/13)
      mylist$ARO_Ending[i] <- abs(mylist$ARO_Opening[i]) + mylist$revalue_aro[i] + abs(mylist$Aro_interest[i])
      
    }else{
      
      mylist$ARO_Opening[i] <- mylist$ARO_Ending[i-1]
      mylist$revalue_aro[i] <- 0
      
      mylist$Aro_interest[i] <- (abs(mylist$ARO_Opening[i]) + mylist$revalue_aro[i])*(ir[i]/100/13)
      mylist$ARO_Ending[i] <- mylist$ARO_Opening[i] + mylist$revalue_aro[i] + mylist$Aro_interest[i]     
    }
  }

}
return(mylist)
}



###########################################################################################
# for new lease only 

new_lse_rowNumByDate_depreci <- function(df){
  
  

  if(isTruthy(df$ASSET_IN_SERV_DT)){
    
    dep_position <- which(df$CORP_PD_STRT_DT <= unique(df$ASSET_IN_SERV_DT) & df$CORP_PD_END_DT >= unique(df$ASSET_IN_SERV_DT))

    

  } else {
    
    dep_position <- NA
    
  }
  
  return(dep_position)
}
##############################################################################################
ROU <- function(PriorROUEnding, Lia_adj, revalue_aro, remainLeaseTerm, TI_Adj, impair_amt, new_lse_rowNumByDate_depreci,
                opening_liability, ARO_opening, lease_pament){
  
  mylist <- list("additions" = c(), "depreciation" = c(), 
                 "ROU_opening" = c(), "ROU_ending" = c(),
                 "renewal_po_adj" = c(), "ROU_aro_adj" = c(),
                 "ROU_TI_adj" = c(), "ROU_Impaired_Adj" = c()
                )

  if(isTruthy(PriorROUEnding)){
      for (i in seq_along(remainLeaseTerm)){
        if(i==1){
          
          mylist$ROU_opening[i] <-  PriorROUEnding
          
          mylist$additions[i] <- Lia_adj[i] + (abs(revalue_aro[i])) + (-abs(impair_amt[i]))
          
         # mylist$depreciation[i] <- -abs((mylist$ROU_opening[i] + mylist$additions[i])/remainLeaseTerm[i])
          mylist$depreciation[i] <-  -((mylist$ROU_opening[i] + mylist$additions[i]))/remainLeaseTerm[i]
         # mylist$ROU_ending[i] <- mylist$ROU_opening[i] + mylist$additions[i] - abs(mylist$depreciation[i])
          mylist$ROU_ending[i] <- sum(mylist$ROU_opening[i], mylist$additions[i], mylist$depreciation[i], na.rm=TRUE)

          mylist$renewal_po_adj[i] <- Lia_adj[i]
          
          mylist$ROU_aro_adj[i] <- revalue_aro[i]
          
          mylist$ROU_TI_adj[i] <- TI_Adj[i]
          
          mylist$ROU_Impaired_Adj[i] <- -abs(impair_amt[i])
          
        }else{
          
          
          mylist$ROU_opening[i] <-  mylist$ROU_ending[i-1] 
          
          mylist$additions[i] <- Lia_adj[i] + (abs(revalue_aro[i])) + (-abs(impair_amt[i]))
          
         # mylist$depreciation[i] <-  -abs((mylist$ROU_opening[i] + mylist$additions[i])/remainLeaseTerm[i])

          mylist$depreciation[i] <-  -((mylist$ROU_opening[i] + mylist$additions[i]))/remainLeaseTerm[i]
          
          #mylist$ROU_ending[i] <- mylist$ROU_opening[i] + mylist$additions[i] - abs(mylist$depreciation[i])

          mylist$ROU_ending[i] <- sum(mylist$ROU_opening[i], mylist$additions[i], mylist$depreciation[i], na.rm=TRUE)
          
          mylist$renewal_po_adj[i] <- Lia_adj[i]
          
          mylist$ROU_aro_adj[i] <- revalue_aro[i]
          
          mylist$ROU_TI_adj[i] <- TI_Adj[i]
          
          mylist$ROU_Impaired_Adj[i] <- -abs(impair_amt[i])
        }
        
      }


      # return(mylist)
  
} else {

      for (i in seq_along(remainLeaseTerm)){

        # for new lease, the 1st depreciation starts when in-service-date starts 
        # find 1st position where in-service-date fall into 

        

        if(i==1){
          
          mylist$ROU_opening[i] <-  opening_liability[i] + abs(ARO_opening[i])
          
          mylist$additions[i] <- Lia_adj[i] + (abs(revalue_aro[i])) + (-abs(impair_amt[i]))

          if( i >=  new_lse_rowNumByDate_depreci){                             #min(which(lease_pament !=0)) ){

          #mylist$depreciation[i] <- -abs((mylist$ROU_opening[i] + mylist$additions[i])/remainLeaseTerm[i])}
          mylist$depreciation[i] <- -((mylist$ROU_opening[i] + mylist$additions[i]))/remainLeaseTerm[i]
          }
          else{

            mylist$depreciation[i] <- lease_pament[i]
          }
          
         # mylist$ROU_ending[i] <- mylist$ROU_opening[i] + mylist$additions[i] - abs(mylist$depreciation[i])
          mylist$ROU_ending[i] <- sum(mylist$ROU_opening[i], mylist$additions[i], mylist$depreciation[i], na.rm=TRUE)

          mylist$renewal_po_adj[i] <- Lia_adj[i]
          
          mylist$ROU_aro_adj[i] <- revalue_aro[i]
          
          mylist$ROU_TI_adj[i] <- TI_Adj[i]
          
          mylist$ROU_Impaired_Adj[i] <- -abs(impair_amt[i])
          
        }else{
          
          mylist$ROU_opening[i] <-  mylist$ROU_ending[i-1] 
          
          mylist$additions[i] <- Lia_adj[i] + (abs(revalue_aro[i])) + (-abs(impair_amt[i]))

          if( i >=  new_lse_rowNumByDate_depreci ){  #min(which(lease_pament !=0)) ){

         # mylist$depreciation[i] <- -abs((mylist$ROU_opening[i] + mylist$additions[i])/remainLeaseTerm[i])}

          mylist$depreciation[i] <- -((mylist$ROU_opening[i] + mylist$additions[i]))/remainLeaseTerm[i]
          }

          else{

            mylist$depreciation[i] <- lease_pament[i]
          }
          
         # mylist$depreciation[i] <-  -abs((mylist$ROU_opening[i] + mylist$additions[i])/remainLeaseTerm[i])
          
          # mylist$ROU_ending[i] <- mylist$ROU_opening[i] + mylist$additions[i] - abs(mylist$depreciation[i])
          
          mylist$ROU_ending[i] <- sum(mylist$ROU_opening[i], mylist$additions[i], mylist$depreciation[i], na.rm=TRUE)

          mylist$renewal_po_adj[i] <- Lia_adj[i]
          
          mylist$ROU_aro_adj[i] <- revalue_aro[i]
          
          mylist$ROU_TI_adj[i] <- TI_Adj[i]
          
          mylist$ROU_Impaired_Adj[i] <- -abs(impair_amt[i])
        }
        
      }



}

 return(mylist)
}
#####################################################################################
TI <- function(remainLeaseTerm, PriorTIEnding, TI_receipt){
  
  mylist <- list("amor" = c(), 
                 "TI_opening" = c(), "TI_ending" = c(),
                 "TI_Addition" = c())
  if(isTruthy(PriorTIEnding)){
      for (i in seq_along(remainLeaseTerm)){
        if(i==1){
          
          mylist$TI_Addition[i] <- TI_receipt[i]
          
          mylist$TI_opening[i] <-  PriorTIEnding
          
          mylist$amor[i] <-  (mylist$TI_opening[i] + abs(mylist$TI_Addition[i]))/remainLeaseTerm[i]
          
          mylist$TI_ending[i] <- mylist$TI_opening[i] + abs(mylist$TI_Addition[i]) + (-abs(mylist$amor[i]))
          
        }else{
          
          mylist$TI_Addition[i] <- TI_receipt[i]
          
          mylist$TI_opening[i] <-  mylist$TI_ending[i-1]
          
          mylist$amor[i] <-  (mylist$TI_opening[i] +  abs(mylist$TI_Addition[i]))/remainLeaseTerm[i]
          
          mylist$TI_ending[i] <- mylist$TI_opening[i] + abs(mylist$TI_Addition[i]) + (-abs(mylist$amor[i]))
        }
        
      }
}else{
        for (i in seq_along(remainLeaseTerm)){
        if(i==1){
          
          mylist$TI_Addition[i] <- TI_receipt[i]
          
          mylist$TI_opening[i] <-  TI_receipt[i]

          #mylist$TI_opening[i] <-  0
          
          mylist$amor[i] <-  (mylist$TI_opening[i] + abs(mylist$TI_Addition[i]))/(remainLeaseTerm[i])
          
          mylist$TI_ending[i] <- mylist$TI_opening[i] + abs(mylist$TI_Addition[i]) + (-abs(mylist$amor[i]))
          
        }else{
          
          mylist$TI_Addition[i] <- TI_receipt[i]
          
          mylist$TI_opening[i] <-  mylist$TI_ending[i-1]
          
          mylist$amor[i] <-  (mylist$TI_opening[i] +  abs(mylist$TI_Addition[i]))/remainLeaseTerm[i]
          
          mylist$TI_ending[i] <- mylist$TI_opening[i] + abs(mylist$TI_Addition[i]) + (-abs(mylist$amor[i]))
        }
        
      }
} 
  return(mylist)
  
}
#####################################################################################
# Combine all functions into a single one!
forecast_lease <- function(originalDF, 
                           changeYr, 
                           changePeriod,
                           newLeaseEndDt,
                           noRenewOption, 
                           Same_rent_change_in_all_renewals,
                           IR_for_LP, 
                           amt_for_lp,
                           inputNewIR, 
                           nth_payday, 
                           TI_Amt,
                           TI_receipt_dt, 
                           impair_amt,  
                           impair_dt, 
                           PO_amt, 
                           PO_dt,
                           AROValue){


  originalDF <- originalDF %>% filter(isTruthy(Remaining_Lease_Term) == TRUE & Remaining_Lease_Term > 0) # add this filter @20190429

  print(paste("last fiscal year of originalDF is ", last(originalDF$Fiscal_Year)))
  print(paste("last fiscal period of originalDF is ", last(originalDF$Fiscal_Period)))
  print(paste("originalDF unique interest rate are:", unique(originalDF$INTEREST_RATE)))
  print(paste("originalDF No rows  are:", length(originalDF$INTEREST_RATE))) 

  print(paste("originalDF lease ID is ", last(originalDF$Lease_ID)))

#  calculate new Lease End DT for payment date calcuation @ 2019/5/13 
  New_IFRS_END_DT <- if(isTruthy(newLeaseEndDt) & !isTruthy(noRenewOption) & !isTruthy(PO_dt)){
  
          newLeaseEndDt
          
        }else if (isTruthy(noRenewOption) & noRenewOption >=1 & !isTruthy(PO_dt)){
          
        new_lse_end_dt <-  lease_option %>% filter(LSE_OPT_TY_DESC == 'Renewal') %>% 
            select(UNQ_LSE_ID, LSE_TERM_STRT_DT, LSE_TERM_END_DT) %>% 
            inner_join(originalDF, by = c('UNQ_LSE_ID' = 'UNQ_LSE_ID')) %>% 
            filter(IFRS_END_DT < LSE_TERM_END_DT) %>% 
            select(Lease_ID , UNQ_LSE_ID,LSE_TERM_STRT_DT, LSE_TERM_END_DT, IFRS_END_DT) %>% distinct() %>% 
            arrange(Lease_ID ,UNQ_LSE_ID,LSE_TERM_STRT_DT) %>% group_by(Lease_ID) %>% 
            mutate(row_id = row_number()) %>% filter(row_id <= noRenewOption) %>% mutate(new_end_dt = max(LSE_TERM_END_DT)) %>% select(Lease_ID, new_end_dt) %>% distinct()
        
        unique(new_lse_end_dt$new_end_dt)
          
        }else if(isTruthy(PO_dt)) {
          
          PO_dt
          
          
        } else {
          
          unique(originalDF$IFRS_END_DT)
  
}

# for leases with no more valid renew option,  unique(new_lse_end_dt$new_end_dt) = 0 

  if(isTruthy(New_IFRS_END_DT) == T){
  
  originalDF$New_IFRS_END_DATE <- New_IFRS_END_DT
  
  print(paste("originalDF new IFRS end date is ", unique(originalDF$New_IFRS_END_DATE)))

  #############################################################################################################################
  lease_add_yr <- lease_option %>% filter(LSE_OPT_TY_DESC == 'Renewal') %>% 
                                    select(UNQ_LSE_ID, LSE_TERM_STRT_DT, LSE_TERM_END_DT) %>% 
                                    inner_join(originalDF, by = c('UNQ_LSE_ID' = 'UNQ_LSE_ID')) %>% 
                                    filter(IFRS_END_DT < LSE_TERM_END_DT) %>% 
                                    select(Lease_ID , UNQ_LSE_ID,LSE_TERM_STRT_DT, LSE_TERM_END_DT, IFRS_END_DT) %>% distinct() %>% 
                                    arrange(Lease_ID ,UNQ_LSE_ID,LSE_TERM_STRT_DT) %>% group_by(Lease_ID) %>% 
                                    mutate(row_id = row_number()) %>% filter(row_id <= noRenewOption) %>% 
                                    mutate(yr_add = (year(LSE_TERM_END_DT) - year(LSE_TERM_STRT_DT))) %>% 
                                    select(Lease_ID, yr_add)

  lease_add_yr_num <- if(isTruthy(noRenewOption) & noRenewOption >=1) {

               sum(lease_add_yr$yr_add)

        } else if(isTruthy(newLeaseEndDt) & !isTruthy(noRenewOption)){
          
          year(newLeaseEndDt) - year(unique(originalDF$IFRS_END_DT))
          
        } else { 0 }

  print(paste("lease_add_yr_num is:", lease_add_yr_num))
  
                  
  print(paste("change year is:", changeYr))
  print(paste("change period is:", changePeriod))
  remain <- subset(originalDF, ((Fiscal_Year < changeYr) |
                                     (Fiscal_Year == changeYr & Fiscal_Period < changePeriod))
                  )
  print(paste("last remain df interest rate is ", last(remain$INTEREST_RATE)))

  change <- 
  if(!isTruthy(PO_dt)){
    subset( originalDF, (Fiscal_Year > changeYr) |
                      ((Fiscal_Year == changeYr) & (Fiscal_Period >= changePeriod))
                   )
  }else{
    
    position <- which(lease_pd_cal$CORP_PD_STRT_DT <= PO_dt & 
                        lease_pd_cal$CORP_PD_END_DT >= PO_dt)
    new_lease_end_yr <-lease_pd_cal$CORP_YR_NUM[position]
    new_lease_end_period <- lease_pd_cal$CORP_PD_NUM[position]
    subset( originalDF, ((Fiscal_Year > changeYr) |
              ((Fiscal_Year == changeYr) & (Fiscal_Period  >= changePeriod)))
              &
              ((Fiscal_Year< new_lease_end_yr) | ((Fiscal_Year == new_lease_end_yr) & (Fiscal_Period <= new_lease_end_period)))
            
          )
    
  }
  print(paste("change df lease ID is ", last(change$Lease_ID)))
  print(paste("1st fiscal year of change is ", change$Fiscal_Year[1]))
  print(paste("1st fiscal period of change is ", change$Fiscal_Period[1]))
  print(paste("last fiscal year of change is ", last(change$Fiscal_Year)))
  print(paste("last fiscal period of change is ", last(change$Fiscal_Period)))
  print(paste("last change df interest rate is ", last(change$INTEREST_RATE)))
  print(paste("unique interest rate are:", unique(change$INTEREST_RATE)))

  # inputNoRenewYrs <- ifelse(!isTruthy(renew_option_years),
  #                           0,
  #                           sapply(strsplit(renew_option_years,","), function(x) sum(as.numeric(x)))
  #                          )
  
  #inputNoRenewYrs <- NoRenewYrs(renew_option_years)  
  inputNoRenewYrs <- lease_add_yr_num
  print(paste("inputNoRenewYrs is", inputNoRenewYrs))
  print(paste(" originalDF IFRS END DT IS", last(originalDF$IFRS_END_DT)))
  #print(paste("lease id is",originalDF$Lease_ID ))
  extendYrPeriod <- extend_yr_period(last(originalDF$IFRS_END_DT), 
                                      renew_yr=inputNoRenewYrs)
  
  extenddf <- data.frame(matrix(ncol = ncol(originalDF), nrow = nrow(extendYrPeriod)))
  colnames(extenddf)<- colnames(originalDF)
  extenddf$Fiscal_Year <-  extendYrPeriod$startYear
  extenddf$Fiscal_Period <- extendYrPeriod$Period
  extenddf$New_IFRS_END_DATE <- unique(originalDF$New_IFRS_END_DATE)

  print(paste("last fiscal year of extenddf is ", last(extenddf$Fiscal_Year)))
  print(paste("last fiscal Period of extenddf is ", last(extenddf$Fiscal_Period)))

  newdf <- rbind(change,extenddf)
  print(paste("unique newdf  New_IFRS_END_DATE  is ", unique(newdf$New_IFRS_END_DATE)))
  print(paste("unique newdf INTEREST_RATE  is ", unique(newdf$INTEREST_RATE)))
  print(paste("last fiscal year of newdf is ", last(newdf$Fiscal_Year)))
  print(paste("last fiscal period of newdf is ", last(newdf$Fiscal_Period)))

  newdf$Remaining_Lease_Term <- seq(length(newdf$Fiscal_Period),1, by=-1)
  print(paste("length of newdf Remaining_Lease_Term is", length(newdf$Remaining_Lease_Term)))

  newdf$INTEREST_RATE  <- newIR(inputNewIR, remain)
  
  print(paste("newIR(inputNewIR) is ", newIR(inputNewIR, remain)))
  print(paste("last(remain$INTEREST_RATE) is", last(remain$INTEREST_RATE)))

  newdf2 <- newdf %>% left_join(lease_pd_cal, by= (c(Fiscal_Year = "CORP_YR_NUM" , 
                                                     Fiscal_Period = "CORP_PD_NUM")))
  newdf2$payment_date <-  mon_pay_dt(newdf2, newdf2$New_IFRS_END_DATE, nth_payday, PO_dt)
  newdf3 <- cbind(updateDateColumns(newdf2), newdf2)
  
 # create a vector or add year 
  addyear <- if (lease_add_yr_num %/% 5 ==0) {
  
              c(lease_add_yr_num)
              
            } else if(lease_add_yr_num %% 5 >0 ){
              
              c(rep(5, lease_add_yr_num %/% 5), lease_add_yr_num %% 5) 
              
            } else{

              c(rep(5, lease_add_yr_num %/% 5))

                  }

  # LPinterestRate <- LP_interestRate(IR_for_LP)

  LP_change_value <- LP_change( Same_rent_change_in_all_renewals, addyear, IR_for_LP, amt_for_lp)

  positionUpdateInterestRate <- findrownum(newdf3$Fiscal_Year, last(change$Fiscal_Year), 
                                           last(change$Fiscal_Period),
                                           newdf3$Fiscal_Period, addyear)
  
  print(paste("Position Update Interest Rate are ", positionUpdateInterestRate))
  rownum <- fun2(positionUpdateInterestRate, newdf3$Fiscal_Year)
  
  #newLPInterestRate <- IRfun(positionUpdateInterestRate, newdf3$Fiscal_Year, LPinterestRate)
  add_LP <- IRfun(positionUpdateInterestRate, newdf3$Fiscal_Year, LP_change_value)
  
  # newdf3$Lease_Payment <-LPfun(newdf3$Financial_Liability_Lease_Payment,
  #                              newdf3$payment_date,
  #                              newdf3$nodaysinperiod, rownum ,newLPInterestRate)

  newdf3$Lease_Payment <- if(isTruthy(IR_for_LP) & !isTruthy(amt_for_lp)){
  
                          LPfun_rate(newdf3$Financial_Liability_Lease_Payment,
                                                    newdf3$payment_date,
                                                    newdf3$nodaysinperiod, rownum ,add_LP)}else{ 
                          
                          
                          LPfun_amt(newdf3$Financial_Liability_Lease_Payment,
                                                                  newdf3$payment_date,
                                                                  newdf3$nodaysinperiod, rownum ,add_LP)                         
  }
  
  newdf3$Financial_Liability_Lease_Payment <- newdf3$Lease_Payment
  
  print(paste("sum of new lease payment is", sum(newdf3$Financial_Liability_Lease_Payment,na.rm=TRUE)))
  # newdf3$TI_receipt <- AdjAmtCal(newdf3$count_period, rowNumByDate(newdf3$count_period,
  #                                                                  newdf3$CORP_PD_STRT_DT,
  #                                                                  newdf3$CORP_PD_END_DT, 
  #                                                                  TI_receipt_dt), 
  #                                                                  TI_Amt)
  
   newdf3$TI_receipt <- AdjAmtCal(newdf3$count_period, rowNumByDate_TI(TI_receipt_dt,
                                                                   newdf3$payment_date), 
                                                                   TI_Amt)


  newdf3$Financial_Liability_Tenant_Inducement_Receipt <-  abs(newdf3$TI_receipt)
  
  # newdf3$PO_amt <- AdjAmtCal(newdf3$count_period,  rowNumByDate(newdf3$count_period,
  #                                                               newdf3$CORP_PD_STRT_DT,
  #                                                               newdf3$CORP_PD_END_DT, 
  #                                                               PO_dt), PO_amt)

  newdf3$PO_amt <- AdjAmtCal(newdf3$count_period,  rowNumByDate_TI( 
                                                                PO_dt,
                                                                newdf3$payment_date), 
                                                                PO_amt)

  # newdf3$impair_amt <- AdjAmtCal(newdf3$count_period,  rowNumByDate(newdf3$count_period,
  #                                                                   newdf3$CORP_PD_STRT_DT,
  #                                                                   newdf3$CORP_PD_END_DT, 
  #                                                                   impair_dt), impair_amt)

  newdf3$impair_amt <- AdjAmtCal(newdf3$count_period,  rowNumByDate_Impair( 
                                                                    impair_dt,
                                                                    newdf3$payment_date), 
                                                                    impair_amt)
  newdf3$PV <- PV_caln(newdf3$Lease_Payment, 
                       newdf3$TI_receipt,
                       newdf3$INTEREST_RATE,
                       newdf3$PO_amt, 
                       newdf3$count_period, 
                       newdf3$no_days_in_period_before_payment,
                       newdf3$nodaysinperiod)     

  print(paste("sum of new lease PV is", sum(newdf3$PV,na.rm=TRUE)))
  #print(paste("New PV value is", sum(newdf3$PV)))
  
  newdf3$Lia_adj <- LiaAdj(newdf3$count_period, newdf3$PV, 
                           last(remain$Financial_Liability_Ending_Detail)
                           )
  
  newdf3$Financial_Liability_Renewal_PO_Adjustment <- newdf3$Lia_adj
  print(paste("sum of Financial_Liability_Renewal_PO_Adjustment is", sum(newdf3$Financial_Liability_Renewal_PO_Adjustment,na.rm=TRUE)))
  Update_Liability <- lease_interest_exp(newdf3$count_period, 
                                         last(remain$Financial_Liability_Ending_Detail),
                                         newdf3$Lia_adj, 
                                         newdf3$INTEREST_RATE  , newdf3$no_days_in_period_before_payment,
                                         newdf3$nodaysinperiod, newdf3$Lease_Payment, 
                                         newdf3$no_days_in_period_after_payment,
                                         newdf3$TI_receipt, newdf3$PO_amt )

  print(paste("update liability sum are:", map(Update_Liability,sum)))

  newdf3$Financial_Liability_Opening <-  Update_Liability$Liability_Opening  
  #newdf3$`Financial Liability Ending` <- Update_Liability$Liability_Ending
  newdf3$Financial_Liability_Ending_Detail <- Update_Liability$Liability_Ending
  newdf3$Financial_Liability_Interest_Expense <- Update_Liability$`interest`    
  
  print(paste("abs(last(change$ARO_Ending)) are:", abs(last(change$ARO_Ending))))

  print(paste("abs(last(remain$ARO_Ending)) are:", abs(last(remain$ARO_Ending))))

  update_ARO <- ARO(abs(last(remain$ARO_Ending)), AROValue, newdf3$INTEREST_RATE , newdf3$Remaining_Lease_Term )  

  newdf3$ARO_Opening  <- -abs(update_ARO$ARO_Opening)
  newdf3$ARO_Interest_Expense <- -abs(update_ARO$Aro_interest)
  newdf3$ARO_Revaluation_Adjustment <- abs(update_ARO$revalue_aro)
  newdf3$ARO_Ending <- -abs(update_ARO$ARO_Ending)       
  print(paste("update_ARO sum are:", map(update_ARO,sum)))

  update_TI <- TI(newdf3$Remaining_Lease_Term , abs(last(remain$Tenant_Inducement_Ending)), 
                  newdf3$TI_receipt)     
  newdf3$Tenant_Inducement_Opening <- update_TI$TI_opening
  newdf3$Tenant_Inducement_Addition  <- update_TI$TI_Addition
  newdf3$Tenant_Inducement_Period_Amortization <- -update_TI$amor
  newdf3$Tenant_Inducement_Ending <- update_TI$TI_ending 
  print(paste("update_TI sum are:", map(update_TI,sum)))


  update_ROU <- ROU(last(remain$Right_of_Use_Ending), newdf3$Lia_adj, newdf3$ARO_Revaluation_Adjustment, 
                    newdf3$Remaining_Lease_Term, newdf3$Tenant_Inducement_Addition, newdf3$impair_amt)
  
  newdf3$Right_of_Use_Opening   <- update_ROU$ROU_opening
  newdf3$Right_of_Use_Renewal_PO_Adjustment <- update_ROU$renewal_po_adj
  newdf3$Right_of_Use_Tenant_Inducement_Adjustment <- update_ROU$ROU_TI_adj
  newdf3$Right_of_Use_ARO_Adjustment <- abs(update_ROU$ROU_aro_adj)
  newdf3$Right_of_Use_Impaired_Adjustment <- update_ROU$ROU_Impaired_Adj
  newdf3$Right_of_Use_Amortization_Expense <- update_ROU$depreciation
  newdf3$Right_of_Use_Ending <- update_ROU$ROU_ending
  print(paste("update_ROU sum are:", map(update_ROU,sum)))
  ##########################################################
  # Update unchanged columns 
  newdf3$Lease_ID <- remain$Lease_ID[1]
  newdf3$Legal_Entity <- remain$Legal_Entity[1]
  newdf3$UNQ_LSE_ID <- remain$UNQ_LSE_ID[1]
  newdf3$LSE_TY_DESC <- remain$LSE_TY_DESC[1]
  newdf3$PRPTY_LSE_NM <- remain$PRPTY_LSE_NM[1]
  newdf3$IFRS_STRT_DT <- remain$IFRS_STRT_DT[1]
  newdf3$IFRS_END_DT <- remain$IFRS_END_DT[1]
  newdf3$IN_SERV_DT <- remain$IN_SERV_DT[1]
  newdf3$LSE_TY_DESC   <- remain$LSE_TY_DESC[1]  
  newdf3$PRPTY_NUM  <- remain$PRPTY_NUM [1]
  newdf3$Ppv_Adjustment_Opening  <- remain$Ppv_Adjustment_Opening [1]
  ###########################################################
  # combine remain and new data frames into final one 
  new <- newdf3[,colnames(newdf3) %in% colnames(remain)]

  print(paste("new last fiscal year is:", last(new$Fiscal_Year)))

  print(paste("new last fiscal period is:", last(new$Fiscal_Period)))

  final <- dplyr::bind_rows(remain, new)
  # replace all NAs to 0
  final[is.na(final)] <- 0
  #############################################################################
  ##################################################################################
  # print sum of numeric 
  # print(map(final[,c(9:30)], na.rm=TRUE, sum)) 
  return(final)
 }else(return(final <- data.frame()))
  
}
####################################################################################################
####################################################################################################
####################################################################################################
new_lease_forecast <- function(lease_info_df, recurring_cost_df){

# Step 1: Remaining Lease Term 
lease_info_df$Remaining_Lease_Term <- seq(length(lease_info_df$CORP_PD_NUM),1, by=-1)
# Step 2: update payment date
lease_info_df$payment_date <-  mon_pay_dt(lease_info_df, lease_info_df$IFRS_END_DT, 1, unique(lease_info_df$PO_Date))
# Step3: update other dates
lease_info_df <- cbind(updateDateColumns(lease_info_df), lease_info_df)

print(paste("new lease infor df number of rows are", nrow(lease_info_df)))
# Step 4: join with recurring cost df
combine_df <- lease_info_df %>% left_join(select(recurring_cost_df, PRPTY_NUM, CORP_PD_STRT_DT,CORP_PD_END_DT,
                                                    monthly_pay, lse_term_start_dt,lse_term_end_dt
                                                    ), by= c("PRPTY_NUM" = "PRPTY_NUM", 
                                                              "CORP_PD_STRT_DT"  = "CORP_PD_STRT_DT" ,
                                                              "CORP_PD_END_DT" = "CORP_PD_END_DT"  
                                                              ))


# step 5: remove duplicated remaining lease term due to recurring cost df 
row_num_to_remove <- rownumtoremove(combine_df)

indx <- !seq_len(nrow(combine_df)) %in% row_num_to_remove

combine_df_2 <- combine_df[indx,, drop=F]

print(paste("new combine df number of rows are", nrow(combine_df_2)))

# Step 6: update lease payment 

min_pay_term_dt <- min(recurring_cost_df$lse_term_start_dt)

combine_df_2$Financial_Liability_Lease_Payment <- new_lease_pay(combine_df_2, min_pay_term_dt)

print(paste("combine_df_2$lse_term_start_dt[1]", (combine_df_2$lse_term_start_dt[1])))

combine_df_2$Financial_Liability_Lease_Payment[is.na(combine_df_2$Financial_Liability_Lease_Payment) == TRUE] <- 0

# Step 7: update TI, Impairment, PO if any one exits 

combine_df_2$TI_receipt <- AdjAmtCal(combine_df_2$count_period, rowNumByDate_TI(unique(combine_df_2$TI_Date),
                                                                                 combine_df_2$payment_date),
                                       unique(combine_df_2$TI_Amt))

combine_df_2$Financial_Liability_Tenant_Inducement_Receipt <- abs(combine_df_2$TI_receipt)

# PO 
combine_df_2$PO_amt <- AdjAmtCal(combine_df_2$count_period,  rowNumByDate_TI(unique(combine_df_2$PO_Date),
                                                                              combine_df_2$payment_date), 
                                   unique(combine_df_2$PO_Amt))
# Impariment 
combine_df_2$impair_amt <- AdjAmtCal(combine_df_2$count_period,  rowNumByDate_Impair(
                                                                          unique(combine_df_2$Impairment_Date),
                                                                          combine_df_2$payment_date), 
                               unique(combine_df_2$Impairment_Amt))

##################################################################################################
# Step 8: update PV column 

combine_df_2$PV <- PV_caln(combine_df_2$Financial_Liability_Lease_Payment, 
                             combine_df_2$TI_receipt,
                             combine_df_2$INCR_BORR_RT,
                             combine_df_2$PO_amt, 
                             combine_df_2$count_period, 
                             combine_df_2$no_days_in_period_before_payment,
                             combine_df_2$nodaysinperiod)   

############################################################################################
# Step 9: assume no Liablity adjustment for new lease  

combine_df_2$Financial_Liability_Renewal_PO_Adjustment <- 0

#############################################################################################
# Step 10: update liability columns 

Update_Liability <- new_lease_interest_exp(combine_df_2$count_period, 
                                           combine_df_2$PV,
                                           combine_df_2$Financial_Liability_Renewal_PO_Adjustment, 
                                           combine_df_2$INCR_BORR_RT  ,
                                           combine_df_2$no_days_in_period_before_payment,
                                           combine_df_2$nodaysinperiod, 
                                           combine_df_2$Financial_Liability_Lease_Payment, 
                                           combine_df_2$no_days_in_period_after_payment,
                                           combine_df_2$TI_receipt, 
                                           combine_df_2$PO_amt )

combine_df_2$Financial_Liability_Opening <-  Update_Liability$Liability_Opening  
combine_df_2$Financial_Liability_Ending_Detail <- Update_Liability$Liability_Ending
combine_df_2$Financial_Liability_Interest_Expense <- Update_Liability$`interest` 

#########################################################################################
# Step 11: Update ARO

update_ARO <- ARO(NA, unique(combine_df_2$ASSET_RTR_OBLGTRY_AMT), 
                  combine_df_2$INCR_BORR_RT , 
                  combine_df_2$Remaining_Lease_Term )  

combine_df_2$ARO_Opening  <- -abs(update_ARO$ARO_Opening)
combine_df_2$ARO_Interest_Expense <- -abs(update_ARO$Aro_interest)
combine_df_2$ARO_Revaluation_Adjustment <- abs(update_ARO$revalue_aro)
combine_df_2$ARO_Ending <- -abs(update_ARO$ARO_Ending) 

####################################################################
# Step 12: Update TI
update_TI <- TI(combine_df_2$Remaining_Lease_Term , NA, 
                combine_df_2$TI_receipt)     

combine_df_2$Tenant_Inducement_Opening <- update_TI$TI_opening
combine_df_2$Tenant_Inducement_Addition  <- update_TI$TI_Addition
combine_df_2$Tenant_Inducement_Period_Amortization <- -update_TI$amor
combine_df_2$Tenant_Inducement_Ending <- update_TI$TI_ending 
######################################################################

depreciation_start_position <- new_lse_rowNumByDate_depreci(combine_df_2)

# Step 13: Update ROU
update_ROU <- ROU(NA, combine_df_2$Financial_Liability_Renewal_PO_Adjustment,
                  combine_df_2$ARO_Revaluation_Adjustment, 
                  combine_df_2$Remaining_Lease_Term, 
                  combine_df_2$Tenant_Inducement_Addition, 
                  combine_df_2$impair_amt ,
                  depreciation_start_position,
                  combine_df_2$Financial_Liability_Opening, 
                  combine_df_2$ARO_Opening,
                  combine_df_2$Financial_Liability_Lease_Payment )

combine_df_2$Right_of_Use_Opening   <- update_ROU$ROU_opening
combine_df_2$Right_of_Use_Renewal_PO_Adjustment <- update_ROU$renewal_po_adj
combine_df_2$Right_of_Use_Tenant_Inducement_Adjustment <- update_ROU$ROU_TI_adj
combine_df_2$Right_of_Use_ARO_Adjustment <- abs(update_ROU$ROU_aro_adj)
combine_df_2$Right_of_Use_Impaired_Adjustment <- update_ROU$ROU_Impaired_Adj
combine_df_2$Right_of_Use_Amortization_Expense <- update_ROU$depreciation
combine_df_2$Right_of_Use_Ending <- update_ROU$ROU_ending
###############################################################################
 #combine_df_2 <- combine_df_2 %>% mutate_if(is.numeric, round, digits = 0L)%>% mutate_if(is.numeric, formatC, digits = 0L, format="d", big.mark = ",")  
 return(combine_df_2)
}

###########################################################################################################
#############################################################################################################
# extend 18 periods to each lease #
extend_18P_df <- function(df){

    extend18period <- extend_18_period(lease_end_year = last(df$Fiscal_Year), lease_end_period = last(df$Fiscal_Period))

    extenddf_18P <- data.frame(matrix(ncol = ncol(df), nrow = nrow(extend18period)))
    colnames(extenddf_18P)<- colnames(df)
    extenddf_18P$Fiscal_Year <-  extend18period$startYear
    extenddf_18P$Fiscal_Period <- extend18period$Period

    extenddf_18P$ARO_Opening <- last(df$ARO_Ending)
    extenddf_18P$ARO_Ending <- last(df$ARO_Ending)

    # Update unchanged columns 
    #extenddf_18P$Lease_ID <- df$Lease_ID[1]
    extenddf_18P$Legal_Entity <- df$Legal_Entity[1]
    #extenddf_18P$UNQ_LSE_ID <- df$UNQ_LSE_ID[1]
    #extenddf_18P$LSE_TY_DESC <- df$LSE_TY_DESC[1]
    #extenddf_18P$PRPTY_LSE_NM <- df$PRPTY_LSE_NM[1]
    extenddf_18P$IFRS_STRT_DT <- df$IFRS_STRT_DT[1]
    extenddf_18P$IFRS_END_DT <- df$IFRS_END_DT[1]
    extenddf_18P$IN_SERV_DT <- df$IN_SERV_DT[1]
    #extenddf_18P$LSE_TY_DESC   <- df$LSE_TY_DESC[1]  
    extenddf_18P$PRPTY_NUM  <- df$PRPTY_NUM [1]
    extenddf_18P$Ppv_Adjustment_Opening  <- df$Ppv_Adjustment_Opening [1]
    extenddf_18P$INTEREST_RATE <- df$INTEREST_RATE[length(df$INTEREST_RATE)]

    return(extenddf_18P)
}
