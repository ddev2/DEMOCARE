scriptDir <- dirname(rstudioapi::getActiveDocumentContext()$path)
if (scriptDir != getwd()) {
  setwd(scriptDir)
}
library(reshape2)

# TWO HELPER FUNCTIONS (Copying and pasting a table from/to Excel)
#dataframetoExcel
te <- function (df, colFlag=TRUE, rowFlag=FALSE) {
  if ( .Platform$OS.type != "windows" ) { # Mac OS, Linux
    write.table(df, pipe("pbcopy"), sep="\t", col.names=colFlag, row.names=rowFlag) 
  } else { # Windows OS
    write.table(df, "clipboard-512", sep="\t", col.names=colFlag, row.names=rowFlag) 
  }
}

#dataframeFromExcel
fe <- function(headerFlag=TRUE, sepChar='\t') {
  if ( .Platform$OS.type != "windows" ) { # Mac OS, Linux
    y <- (read.table(pipe("pbpaste"), header=headerFlag, sep=sepChar)) 
  } else { # Windows OS
    y <- (read.table("clipboard", header=headerFlag, sep=sepChar))
  }
  if (length(y) == 1) {
    #if the data.frame has only one column, return the data as a vector
    y <- unlist(y)
  }
  return (y)
}

#loess is 1, Tukey's EDA is 2
loessORTukeyEDA <- 1

fmt_digits <- 10

#### Activity table by dependency status ####
#copy table from Excel (run this line ONLY when the data has just been copied from Excel)
#actData <- fe()
#save(actData, file="actividad.RData")
load(file="actividad.RData")

#change names
names(actData)[1] <- "Dep"
names(actData)[2] <- "Sex"
names(actData)[5] <- "Age"
names(actData)[6] <- "N"

#create a column for numerical age
actData$exactAge <- NA
levAge <- levels(actData$Age)
currAge <- 18
for (indAge in levAge) {
  if (indAge == "20-24") {currAge=22.5}
  actData$exactAge <- ifelse(actData$Age==indAge,currAge,actData$exactAge)
  currAge <- currAge + 5
}

#drop ages over 70
actData <- subset(actData, actData$exactAge < 70)

#simplify names (over a copied data.frame)
actData2 <- actData

levels(actData2$Dep) <- c("3", "1", "2", "0")
levels(actData2$Edu) <- c("Baix", "Mitg", "Alt", "Total")
levels(actData2$Sex) <- c("Female", "Total", "Male")
levels(actData2$Act) <- c("NoOcup", "OcupParcial", "OcupCompleto")

levels(actData2$Dep) <- c("3", "1", "2", "0")
levels(actData2$Edu) <- c("B", "M", "A", "Total")
levels(actData2$Sex) <- c("F", "Total", "M")
levels(actData2$Act) <- c("A", "P", "C")

levDep <- levels(actData2$Dep)
levEdu <- levels(actData2$Edu)
levSex <- levels(actData2$Sex)
levAct <- levels(actData2$Act)
ages <- c(0:105)
Nage <- length(ages)
ages_1 <- ages[1:(Nage - 1)]

#Fill with 1 and 0 before age 15 and after age 70
for (dep in levDep) {
  for (sex in levSex) {
    for (edu in levEdu) {
      for (act in levAct) {
        for (age in c(10, 15, 72.5, 80, 90, 100, 105)) {
          df <- data.frame(Dep=dep, Sex=sex, Act=act, Edu=edu, Age="NON", N=0, exactAge=age)
          if (act == "NoOcup") {
            df$N <- 1
          }
          actData2 <- rbind (actData2, df)
        }
      }
    }
  }
}

actData2 <- actData2[with(actData2, order(Dep, Sex, Edu, Act, exactAge)),]
spanValue <- 0.5

#for debugging purposes
dep <- "0"
edu <- "B"
sex <- "M"
act <- "A"

#compute yearly ages
actOut <- data.frame(Dep=NULL, Edu=NULL, Sex=NULL, Act=NULL, exactAge=NULL, N=NULL)
eps <- 0.00001
for (edu in levEdu) {
  for (sex in levSex) {
    for (dep in levDep) {
      sumCol <- rep(0, Nage)
      for (act in levAct) {
        print (paste(dep, edu, sex, act))
        df <- data.frame(Dep=rep(dep, Nage), Edu=rep(edu, Nage), Sex=rep(sex, Nage), Act=rep(act, Nage), exactAge=ages, N=rep(0, Nage))
        
        vectAct <- actData2[(actData2$Dep==dep)&(actData2$Edu==edu)&(actData2$Sex==sex)&(actData2$Act==act)&(actData2$Age!="Total"),]$N
        #we transform the data with logit
        #first we remove values outside the [0, 1] range
        vectAct <- ifelse(vectAct <= 0, eps, vectAct)
        vectAct <- ifelse(vectAct >= 1, 1-eps, vectAct)
        vectAct <- log (vectAct / ( 1 - vectAct ))
        
        if (loessORTukeyEDA==1) {
          #loess smoothing used for interpolating and extrapolating (over a short range)
          actData2[(actData2$Dep==dep)&(actData2$Edu==edu)&(actData2$Sex==sex)&(actData2$Act==act)&(actData2$Age!="Total"),]$N <- vectAct
          loessm <- loess(N~exactAge,
                          data=actData2[(actData2$Dep==dep)&(actData2$Edu==edu)&(actData2$Sex==sex)&(actData2$Act==act)&(actData2$Age!="Total"),]
                          , control = loess.control(surface = "direct"), span=spanValue
          )
          
           res <- predict(loessm, data.frame(exactAge=ages))
        } else {
          res <- smooth.4253H.twice(vectAct)
        }
        #convert back from logit
        res <- exp (res) / ( 1 + exp (res) )
        df$N <- res
        actOut <- rbind(actOut, df)
        
        sumCol <- sumCol + res
      }
      #normalize the sum to 1 (and adjust polynomial model of age afterward)
      for (act in levAct) {
        actOut[(actOut$Dep==dep)&(actOut$Edu==edu)&(actOut$Sex==sex)&(actOut$Act==act),]$N <-
          actOut[(actOut$Dep==dep)&(actOut$Edu==edu)&(actOut$Sex==sex)&(actOut$Act==act),]$N /
          sumCol
      }
    }
  }
}

#force values before age 15 and after age 75
actOut[((actOut$exactAge<16)|(actOut$exactAge>75))&(actOut$Act=="A"),]$N <- 1
actOut[((actOut$exactAge<16)|(actOut$exactAge>75))&(actOut$Act=="P"),]$N <- 0
actOut[((actOut$exactAge<16)|(actOut$exactAge>75))&(actOut$Act=="C"),]$N <- 0

#create csv of activity prevalence
actOut <- subset(actOut, (Edu != "Total") & (Sex != "Total"))
#==== Bug fix Daniel August 2026 ====
actOut$Dep <- factor(actOut$Dep, levels = c("3", "2", "1", "0"))
actOut$Edu <- factor(actOut$Edu, levels = c("B", "M", "A"))
actOut$Sex <- factor(actOut$Sex, levels = c("F", "M"))
actOut$Act <- factor(actOut$Act, levels = c("A", "P", "C"))
# End buf fix
write.csv(file="activity.csv", format ( actOut, digits=fmt_digits, nsmall=fmt_digits, scientific=TRUE ), quote=FALSE, row.names=FALSE)

#### activity table all transitions ####
## EPA transition table
# create trans
# EPA_survey <- fe()
# save(EPA_survey, file="EPA_survey.RData")

#==== Change Daniel August 2026 ====
# transitions are now computed in R instead of Excel

# EPA survey data: two surveys at one year distance
load (file="EPA_survey.RData")

source ("compute_activity_transitions.R")

trans <- compute_activity_transitions(
  EPA_survey,
  age_range  = 16:64
)
# End change transition

trans$Sex <- ifelse(trans$sex==1, "Male","Female")
trans$sex <- NULL
trans <- dplyr::rename(trans, Age=age)
trans <- trans |>
  tidyr::pivot_longer(cols=starts_with("p"), names_to = "Trans", values_to = "Value")
trans <- dplyr::relocate(trans, Trans)
trans <- dplyr::relocate(trans, Sex, .after=Trans)
# 2026-09-10 : BUG FIX. The line below referred to an object 'trans1' that is never
# created anywhere in this script, so the script aborted here with
# "object 'trans1' not found" and never reached the transition matrix.
# The line was also redundant: the arrange(Trans) on the next line does
# exactly the same sort. Original line, kept commented out for review:
#   trans <- trans[order(trans1$Trans), ]
trans <- trans |> dplyr::arrange(Trans)
#==== end BUG FIX =====

# copy table from Excel (run this line ONLY when the data has just been copied from Excel)
# trans <- fe()
# save (trans, file="EPA_trans.RData")
# load (file="EPA_trans.RData")

actMat <- acast(actOut, Sex~Edu~Dep~Act~exactAge)
EPA_transMat <- acast(trans, Trans~Sex~Age)
EPA_transMat <- ifelse(is.na(EPA_transMat), 0, EPA_transMat)

act_transMat <- array(NA, dim=c(2, 3, 4, 9, 105))
#dimnames(act_transMat) <- list(c(), "Edu", "Dep", "Proba", "Age"))

N_MAX <- 100 # number of iterations
calc_proba <- function(a=NULL, EPA_proba, SUMAPC=TRUE) {
  
  proba <- array(0, dim=c(9, 105))
  proba[1,] = 1
  proba[4,] = 0
  proba[7,] = 0
  proba[,c(17:65)] <- EPA_proba
  proba[,c(66:74)] <- c(1,0,0,0.95,0.05,0,0.95,0,0.05)
  proba[,75] <- c(1,0,0,1,0,0,1,0,0)
  
  iter <- function(p=proba, n=N_MAX) { # n increased from 7 to N_MAX Daniel August 2026
    ch <- array(0, dim=c(9, 105))
    
    #==== change August 2026 in the iteration process: we use min_test ====
    # min_test now actually drives convergence: it should decrease each
    # iteration as p settles toward a fixed point, then start increasing
    # again once it overshoots. We keep the p from whichever iteration had
    # the lowest error and stop as soon as the error turns back up, instead
    # of blindly running all n iterations regardless of what min_test says.
    best_p   <- p
    best_err <- Inf
    n_used   <- 0
    
    for (ind in (1:n)) {
      ch[1,] = a[1,c(1:105)] * p[1,]
      ch[2,] = a[1,c(1:105)] * p[2,]
      ch[3,] = a[1,c(1:105)] * p[3,]
      ch[4,] = a[2,c(1:105)] * p[4,]
      ch[5,] = a[2,c(1:105)] * p[5,]
      ch[6,] = a[2,c(1:105)] * p[6,]
      ch[7,] = a[3,c(1:105)] * p[7,]
      ch[8,] = a[3,c(1:105)] * p[8,]
      ch[9,] = a[3,c(1:105)] * p[9,]
      
      b <- array(0, dim=c(3, 105))
      b[1,] <- ch[1,] + ch[4,] + ch[7,]
      b[2,] <- ch[2,] + ch[5,] + ch[8,]
      b[3,] <- ch[3,] + ch[6,] + ch[9,]
      
      ch2 <- ch
      
      ch2[2,] <- ifelse(b[2,]>0,ch[2,]*a[2,c(2:106)]/b[2,],a[2,c(2:106)])
      ch2[3,] <- ifelse(b[3,]>0,ch[3,]*a[3,c(2:106)]/b[3,],a[3,c(2:106)])
      for (age in (2:105)) {
        #hack for negative values
        if (a[1,age]-(ch2[2,age] + ch2[3,age]) < 0) {
          ch2[2,age] <- ch2[2,age-1]
          ch2[3,age] <- ch2[3,age-1]
        }
      }
      ch2[1,] <- a[1,c(1:105)] - ch2[2,] - ch2[3,]
      
      ch2[4,] <- ifelse(b[1,]>0,ch[4,]*a[1,c(2:106)]/b[1,],a[1,c(2:106)])
      ch2[6,] <- ifelse(b[3,]>0,ch[6,]*a[3,c(2:106)]/b[3,],a[3,c(2:106)])
      for (age in (2:105)) {
        #hack for negative values
        if (a[2,age]-(ch2[4,age] + ch2[6,age]) < 0) {
          ch2[4,age] <- ch2[4,age-1]
          ch2[6,age] <- ch2[6,age-1]
        }
      }
      ch2[5,] <- a[2,c(1:105)] - ch2[4,] - ch2[6,]
      
      ch2[7,] <- ifelse(b[1,]>0,ch[7,]*a[1,c(2:106)]/b[1,],a[1,c(2:106)])
      ch2[8,] <- ifelse(b[2,]>0,ch[8,]*a[2,c(2:106)]/b[2,],a[2,c(2:106)])
      for (age in (2:105)) {
        #hack for negative values
        if (a[3,age]-(ch2[7,age] + ch2[8,age]) < 0) {
          ch2[7,age] <- ch2[7,age-1]
          ch2[8,age] <- ch2[8,age-1]
        }
      }
      ch2[9,] <- a[3,c(1:105)] - ch2[7,] - ch2[8,]
      
      b2 <- b
      b2[1,] <- ch2[1,] + ch2[4,] + ch2[7,]
      b2[2,] <- ch2[2,] + ch2[5,] + ch2[8,]
      b2[3,] <- ch2[3,] + ch2[6,] + ch2[9,]
      
      # min_test[age]: distance between what THIS iteration's updated p
      # (computed just below) implies for next year's stock (b2, built
      # from ch2) and the actual next-year stock in the health-survey
      # table (a[,2:106]). Summarise across ages into one number per
      # iteration so we can track whether it is still improving.
      min_test <- sqrt ((b2[1,]-a[1,c(2:106)])^2 + (b2[2,]-a[2,c(2:106)] )^2 + (b2[3,]-a[3,c(2:106)])^2)
      err <- sum(min_test, na.rm=TRUE)
      
      p[1,] <- ifelse(a[1,c(1:105)]>0, ch2[1,] / a[1,c(1:105)], 0)
      p[2,] <- ifelse(a[1,c(1:105)]>0, ch2[2,] / a[1,c(1:105)], 0)
      p[3,] <- ifelse(a[1,c(1:105)]>0, ch2[3,] / a[1,c(1:105)], 0)
      p[4,] <- ifelse(a[2,c(1:105)]>0, ch2[4,] / a[2,c(1:105)], 0)
      p[5,] <- ifelse(a[2,c(1:105)]>0, ch2[5,] / a[2,c(1:105)], 0)
      p[6,] <- ifelse(a[2,c(1:105)]>0, ch2[6,] / a[2,c(1:105)], 0)
      p[7,] <- ifelse(a[3,c(1:105)]>0, ch2[7,] / a[3,c(1:105)], 0)
      p[8,] <- ifelse(a[3,c(1:105)]>0, ch2[8,] / a[3,c(1:105)], 0)
      p[9,] <- ifelse(a[3,c(1:105)]>0, ch2[9,] / a[3,c(1:105)], 0)
      
      # err evaluates the p we just computed above (b2 comes from ch2, and
      # p <- ch2 / a, so b2 is exactly what this new p implies for next
      # year's stock). Keep it if it's the best so far; as soon as it gets
      # worse than the previous iteration, stop and fall back to the best
      # one seen (min_test diminishes then increases, per Daniel).
      if (err <= best_err) {
        best_err <- err
        best_p   <- p
        n_used   <- ind
      } else {
        break
      }
    } # for (ind in (1:n))
    
    attr(best_p, "n_iter")    <- n_used
    attr(best_p, "final_err") <- best_err
    return (best_p)
  }
  
  p <- iter(proba)
  n_iter_used <- attr(p, "n_iter")
  final_err   <- attr(p, "final_err")
  if (SUMAPC) {
    # NetLogo and R program need this: we use only one random draw to determine which transition takes place
    p[3,] <- p[2,] + p[3,]
    p[6,] <- p[4,] + p[6,]
    p[8,] <- p[7,] + p[8,]
  }
  # re-attach (row assignment above should preserve them anyway, but this
  # guarantees it regardless of how p was reassigned)
  attr(p, "n_iter")    <- n_iter_used
  attr(p, "final_err") <- final_err
  return (p)
}

# 2026-09-10 : BUG FIX (part 1 of 2): initialise the convergence log BEFORE the loop
# that writes into it (it used to be initialised after the loop).
convergence_log <- data.frame(sex=integer(), edu=integer(), dep=integer(),
                              n_iter=integer(), final_err=double())
#==== end BUG FIX =====
for (sex in c(1:2)) { # female, male
  for (edu in c(1:3)) {
    for (dep in c(1:4)) { # 3, 2, 1, 0
      res <- calc_proba (actMat[sex, edu, dep,,], EPA_transMat[,sex,])
      act_transMat[sex, edu, dep,,] <- res
      convergence_log <- rbind(convergence_log, data.frame(
        sex = sex, edu = edu, dep = dep,
        n_iter = attr(res, "n_iter"), final_err = attr(res, "final_err")
      ))
    }
  }
}

#==== convergence check August 2026 ====
# Convergence check (this used to be a TODO -- min_test was computed but
# never read). Flags any (sex, edu, dep) cell that used all 7 iterations
# without the error turning back up: for those the true minimum may sit
# beyond iteration 7, and n inside iter() may need to be raised.
# 2026-09-10 : BUG FIX (part 2 of 2). 'convergence_log' used to be created HERE, i.e.
# AFTER the (sex, edu, dep) loop that rbind()s into it. In a fresh R
# session the first rbind() in that loop failed with "object
# 'convergence_log' not found"; if the object happened to survive from an
# earlier run, the loop filled it and this line then emptied it again, so
# the convergence summary below always printed an empty table. The
# initialisation has been moved to just above the loop (see marker there).
#==== end BUG FIX =====

cat("\n#### calc_proba() convergence summary ####\n")
print(summary(convergence_log$n_iter))
print(summary(convergence_log$final_err))
still_improving <- subset(convergence_log, n_iter == 7)
if (nrow(still_improving) > 0) {
  cat(nrow(still_improving), "of", nrow(convergence_log),
      "(sex, edu, dep) cells used all 7 iterations without the error turning back up",
      "-- consider raising n in iter():\n")
  print(still_improving)
} else {
  cat("All", nrow(convergence_log), "(sex, edu, dep) cells converged (error turned back up) before iteration 7.\n")
}

source("check_convergence_unimodality.R")
# End convergence check

#hack to force correct level at age 74
act_transMat[,,,,75] <- act_transMat[,,,,76]

m_act_transMat <- melt(act_transMat)
names(m_act_transMat) <- c("sex", "edu", "dep", "origin", "age", "value")

mode(m_act_transMat$sex)<-"character"
m_act_transMat$sex<-factor(m_act_transMat$sex)
levels(m_act_transMat$sex) <- c("F", "M")

mode(m_act_transMat$edu)<-"character"
m_act_transMat$edu<-factor(m_act_transMat$edu)
levels(m_act_transMat$edu) <- c("B", "M", "A")

mode(m_act_transMat$dep)<-"character"
m_act_transMat$dep<-factor(m_act_transMat$dep)
levels(m_act_transMat$dep) <- c("3", "2", "1", "0")

mode(m_act_transMat$origin)<-"character"
m_act_transMat$origin<-factor(m_act_transMat$origin)
levels(m_act_transMat$origin) <- c("A_A", "A_P", "A_C",
                                  "P_A", "P_P", "P_C",
                                  "C_A", "C_P", "C_C")

m_act_transMat$age <- m_act_transMat$age - 1
m_act_transMat$dest <- substr (m_act_transMat$origin,3,3)
m_act_transMat$origin <- substr (m_act_transMat$origin,1,1)

m_act_transMat <- subset(m_act_transMat, (m_act_transMat$dest!=m_act_transMat$origin))
m_act_transMat <- m_act_transMat[,c(1,2,3,4,5,7,6)]

m_act_transMat$value <- ifelse((m_act_transMat$age==74)&(m_act_transMat$dest=="A"),1,m_act_transMat$value)
m_act_transMat$value <- ifelse((m_act_transMat$age==74)&(m_act_transMat$dest=="C")&(m_act_transMat$origin=="P"),1,m_act_transMat$value)
m_act_transMat$value <- ifelse((m_act_transMat$age==74)&(m_act_transMat$dest=="P")&(m_act_transMat$origin=="C"),1,m_act_transMat$value)

write.csv(file="act_transitions.csv", format ( m_act_transMat, digits=fmt_digits, nsmall=fmt_digits, scientific=TRUE), quote=FALSE, row.names=FALSE)
