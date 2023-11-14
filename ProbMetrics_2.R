#--------------------------------  NOTE  ----------------------------------------
# 1 This code is to evaluate probabilistic forecast accuracy;
# 2 The format of the input data is [Actual Forecast Q1 ... Q99];
# 3 Coder: Cong Feng         Date: 2020/07/19       @ NREL
#--------------------------------------------------------------------------------

ProbMetrics_2 <- function(anly) {
  library(scoringRules)
  library(magrittr)
  library(dplyr)
  #anly <- df_last[,c('Actual', 'Forecast', paste0('Q', seq(1,99,by = 1)))]
  # CRPS
  stand_dev <- (anly[, 'Q99']-anly[, 'Q50'])/qnorm(.99)
  stand_dev[stand_dev <= 0] <- 1e-10
  CRPS_all <- scoringRules::crps.numeric(anly[, 'Actual'], family = 'norm', mean = anly[, 'Q50'], 
                                         sd = stand_dev)
  nCRPS <- mean(CRPS_all)/max(anly$Actual)

  # reliability
  reliability <- rep(0,49)
  #anly_day <- anly[anly$Actual != 0,]
  #anly <- anly[anly$Actual != 0,]
  anly[anly<0] <- 0
  anly[anly > max(anly$Actual)] <- max(anly$Actual)
  
  for (i in 1:49) {
    lq <- 50 - 1*i
    uq <- 50 + 1*i
    forc_qt <- anly[, c(colnames(anly)[1],paste0('Q', c(lq, uq)))]
    #forc_qt_day <- anly_day[, c(colnames(anly_day)[1],paste0('Q', c(lq, uq)))]
    reliability[i] <- nrow(forc_qt[(forc_qt[,1] >= forc_qt[,2] & forc_qt[,1] <= forc_qt[,3]),])/nrow(anly)
  }
  
  # ACE
  ACE <- mean(abs(seq(0.02, 0.98, 0.02) - reliability))
  CE_98 <- .98 - reliability[length(reliability)]
  #reliability[49] <- 1
  # sharpness
    sharpness <- rep(0,49)
  for (i in 1:49) {
    lq <- 50 - 1*i
    uq <- 50 + 1*i
    forc_qt <- anly[, c(paste0('Q', c(lq, uq)))]
    sharpness[i] <- mean(anly[, paste0('Q', uq)] - anly[, paste0('Q', lq)])
  }
    
  # interval score
    # interval score
    IS_total <- rep(0,49)
    for (no_int in 1:length(IS_total)) {
      name_UB <- paste0('Q', no_int+50)
      name_LB <- paste0('Q', -no_int+50)
      UB <- anly[,name_UB]
      LB <- anly[,name_LB]
      I_sigma <- UB - LB
      I_alpha <- (50-no_int)*0.02
      y <- anly$Actual
      IS <- rep(0, nrow(anly))
      for (i in 1:nrow(anly)) {
        if (y[i]<LB[i]) {
          IS[i] <- -2*I_sigma[i]*I_alpha + 4*(LB[i]-y[i])
        }
        if (y[i]>UB[i]) {
          IS[i] <- -2*I_sigma[i]*I_alpha + 4*(y[i]-UB[i])
        }
        if (y[i]<=UB[i]&y[i]>=LB[i]) {
          IS[i] <- -2*I_sigma[i]*I_alpha
        }
        IS_total[no_int] <- mean(IS)
      }
    }
    ave_IS <- mean(IS_total)
    nave_IS <- mean(IS_total)/max(anly$Actual)
    
    
  
  # pinball loss
  z <- anly[,paste0('Q', seq(1, 99))]
  m2 <- anly$Actual      #observation
  
  pinball_total <- rep(0,nrow(anly))
  pb99 <- rep(0,99)  #build a vector to save pinball loss
  for (i in 1:nrow(anly)) {
    for (j in 1:ncol(z)) {
      if (((m2[i]-z[i,j])<0)) {
        pb99[j] <- (1-j/100)*(z[i,j]-m2[i])
      }else{pb99[j] <- (j/100)*(m2[i]-z[i,j])}
    }
    # print(sum(pb99))
    pinball_total[i] <- sum(pb99)
  }
  pb_ave <- sum(pinball_total)/(nrow(anly)*ncol(z))  #calculate pinball loss metrics
  npb_ave <- sum(pinball_total)/(nrow(anly)*ncol(z)*max(m2))   #calculate normalized pinball loss
  
  
  #result <- as.matrix(cbind(nCRPS, pb_ave, npb_ave, ACE, CE_98, reliability, sharpness))
  result <- list('nCRPS' = nCRPS,  'pb_ave' = pb_ave, 'npb_ave' = npb_ave, 
                 'ACE' =ACE, 'CE_98' = CE_98, 'IS_ave'=ave_IS, 'IS_nave' = nave_IS, 'reliability' = reliability, 'sharpness' = sharpness)
  
  #colnames(result) <- c('CRPS', 'PinballLoss')
  return(result)
}