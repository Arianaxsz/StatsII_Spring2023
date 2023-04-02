### Replication code for Taylor C. Boas, F. Daniel Hidalgo, and Guillermo Toral. "Competence versus Priorities: Negative Electoral Responses to Education Quality in Brazil"
### This file replicates the analyses, figures, and tables of the regression discontinuity design included in the main text and in the Appendices, It uses the dataset rdd_dataset.csv, which can be built with the code included in rdd_dataset.R
### R version, platform, and package versions reported at the end of the file
### April 2, 2023
### Ariana Antunes 

# PREPARE THE ENVIRONMENT -------------------------------------------------
# Set Working Directory to wherever this file is located.

# The directory where this file is located must also have a "figures" and a "tables" subdirectory
# Clean the environment
rm(list = ls())
# Load packages (make sure they are previously installed)
library(tidyverse); library(rdd); library(rdrobust); library(texreg); library(xtable); library(lfe); library(timelineS)
# Open and execute rdd_functions.R so that the user-defined rd_plot function is available
# Load dataset, as assembled by rdd_dataset.R
m <- read_csv("rdd/data/rdd_dataset.csv") 
# Create a copy that excludes observations where the mayor is not eligible to run, where there were supplementary elections, or where there are two IDEB signals (see explanations in Appendix A6)
mm <- subset(m, m$incumbent_mayor_cannot_run==0 & m$supplementary_election==0 & m$ef1_only==1)


linear.m <- lm(incumbent_mayor_reelected ~ ideb_gap_centered, data = m)
summary(linear.m)

# TABLE 1: EFFECT OF MEETING TARGET ON RE-ELECTION OF THE MAYOR ------------
# Run models: Local linear ("llr") and Calonico et al. ("cct") models, without and with controls
r1_cct <- rdrobust(mm$incumbent_mayor_reelected, mm$ideb_gap_centered, covs=mm$electoral_cycle_2012 + mm$electoral_cycle_2016,all=T)
r1_llr <- felm(incumbent_mayor_reelected ~ treated*ideb_gap_centered + electoral_cycle_2012 + electoral_cycle_2016, data=mm, subset=abs(mm$ideb_gap_centered)<r1_cct$bws[[1,1]])
r2_cct <- rdrobust(mm$incumbent_mayor_reelected, mm$ideb_gap_centered, covs=mm$electoral_cycle_2012 + mm$electoral_cycle_2016 + mm$incumbent_mayor_voteshare_previous + mm$incumbent_party_pt + mm$incumbent_party_psdb + mm$incumbent_party_pmdb + mm$population_log + mm$share_poor + mm$tenured_employees_share + mm$incumbent_party_ran, all=T)
r2_llr <- felm(incumbent_mayor_reelected ~ treated*ideb_gap_centered + electoral_cycle_2012 + electoral_cycle_2016 + incumbent_mayor_voteshare_previous + incumbent_party_pt + incumbent_party_psdb + incumbent_party_pmdb + population_log + share_poor + tenured_employees_share + incumbent_party_ran, data=mm, subset=abs(mm$ideb_gap_centered)<r2_cct$bws[[1,1]])

# Build table by hand (rdrobust models not compatible with texreg, broom, stargazer)
c1 <- c(paste(round(summary(r1_llr,robust=T)$coef["treated",1],3),
              ifelse(summary(r1_llr,robust=T)$coef["treated",4]<0.01,"***",
                     ifelse(summary(r1_llr,robust=T)$coef["treated",4]<0.05,"**",
                            ifelse(summary(r1_llr,robust=T)$coef["treated",4]<0.1,"*",""))),sep=""),
        paste("(",round(summary(r1_llr,robust=T)$coef["treated",2],3),")",sep=""),
        "Yes", "No", 
        round(r1_cct$bws[[1,1]],3),
        r1_llr$N)
c2 <- c(paste(round(summary(r2_llr,robust=T)$coef["treated",1],3),
              ifelse(summary(r2_llr,robust=T)$coef["treated",4]<0.01,"***",
                     ifelse(summary(r2_llr,robust=T)$coef["treated",4]<0.05,"**",
                            ifelse(summary(r2_llr,robust=T)$coef["treated",4]<0.1,"*",""))),sep=""),
        paste("(",round(summary(r2_llr,robust=T)$coef["treated",2],3),")",sep=""),
        "Yes", "Yes", 
        round(r2_cct$bws[[1,1]],3),
        r2_llr$N)
c3 <- c(paste(round(r1_cct$coef[3],3),
              ifelse(r1_cct$pv[3]<0.01,"***",
                     ifelse(r1_cct$pv[3]<0.05,"**",
                            ifelse(r1_cct$pv[3]<0.1,"*",""))),sep=""),
        paste("(",round(r1_cct$se[3],3),")",sep=""),
        "Yes", "No", 
        round(r1_cct$bws[[1,1]],3),
        sum(r1_cct$N_h))
c4 <- c(paste(round(r2_cct$coef[3],3),
              ifelse(r2_cct$pv[3]<0.01,"***",
                     ifelse(r2_cct$pv[3]<0.05,"**",
                            ifelse(r2_cct$pv[3]<0.1,"*",""))),sep=""),
        paste("(",round(r2_cct$se[3],3),")",sep=""),
        "Yes", "Yes", 
        round(r2_cct$bws[[1,1]],3),
        sum(r2_cct$N_h))

t1 <- as.table(cbind(c1,c2,c3,c4))
rownames(t1) <- c("IDEB target met", "", "Election cycle fixed effects", "Controls", "Bandwidth", "Observations")
colnames(t1) <- c("Linear", "Linear", "Robust", "Robust")

print(t1)


print(file="rdd_results_reelection.tex",
      xtable(t1, align=c("lcccc")),
      caption.placement="top",
      rowname=names,
      comment=F,
      floating=F,
      booktabs=T)

# FIGURE 1: EFFECT OF MEETING TARGET ON RE-ELECTION OF THE MAYOR --------
pdf("rdplot_reelection.pdf",width=9,height=5)
rd_plot(x = mm$ideb_gap, y = mm$incumbent_mayor_reelected, cutpoint = -0.05,
        xrange = c(-.55,.45), yrange=c(0.15,.7),bins=50,
        maintitle="", xtitle = "IDEB score - IDEB target",
        ytitle = "Incumbent mayor re-elected",
        legendabove="Municipalities that \n met their IDEB target",
        legendbelow="Municipalities that \n missed their IDEB target")
grid()
dev.off()

# APPENDIX A2: CITIZEN ATTENTION TO IDEB ----------------------------------
# Data downloaded from https://trends.google.com/trends/
trends <- read_csv("rdd/data/google_trends_complete.csv")
trends$month <- as.Date(paste0(trends$Month,"-15"))

pdf("google_trends.pdf", width=9,height=3)
ggplot(trends, aes(mes)) + 
  geom_line(aes(x = month, y = IDEB, color="blue"), size=1.5, linetype="solid") + 
  geom_line(aes(x = month, y = corrupcao, colour = "red"), size=1, linetype="solid") +
  geom_line(aes(x = month, y = inflacao, colour = "green"), size=1, linetype="solid") +
  geom_line(aes(x = month, y = bolsa_familia, colour = "gray"), size=1, linetype="solid") +
  theme_bw() +
  labs(x = "Date", y = "Relative number of Google searches") +
  scale_color_manual(values = c("blue", "red", "green", "gray"), name = "Search terms", 
                     labels = c("IDEB", "corrupção", "inflação", "Bolsa Família"))
dev.off()

# APPENDIX A3: INFORMATION RELEASE AND ELECTION SCHEDULE ------------------

d <- cbind(c("IDEB 2007 published", "IDEB 2009 published", "IDEB 2011 published", "IDEB 2013 published", "IDEB 2015 published", "IDEB 2017 published",
             "Municipal elections 2008", "State and federal elections 2010", "Municipal elections 2012", "State and federal elections 2014", "Municipal elections 2016", "State and federal elections 2018"),
           as.Date(c("2008-06-21", "2010-07-01", "2012-08-14", "2014-09-05", "2016-09-08", "2018-09-03",
                     "2008-10-05", "2010-10-03", "2012-10-07", "2014-10-05", "2016-10-02", "2018-10-07")))

d <- as.data.frame(cbind(c("IDEB 2007 \n published", "IDEB 2009 published", 
                           "IDEB 2011 \n published", "IDEB 2013 published", 
                           "IDEB 2015 \n published", "IDEB 2017 published", 
                           "2008 municipal \n elections", "2010 state and \n federal elections", 
                           "2010 municipal \n elections", "2014 state and \n federal elections", 
                           "2016 municipal \n elections", "2018 state and \n federal elections"),
                         c("2008-06-21", "2010-07-01", "2012-08-14", "2014-09-05", "2016-09-08","2018-09-03",
                           "2008-10-05", "2010-10-03", "2012-10-07", "2014-10-05", "2016-10-02", "2018-10-07")))
d$V2 <- as.Date(d$V2)
d <- d[order(d$V2),]

pdf("ideb_timeline.pdf",width=10,height=5)
par(mar=c(0.2,0.2,0.2,0.2))
timelineS(d, scale = "year",
          line.width = 7,line.color="blue",
          #  scale.font=c(2,2,1,1,2,2,1,1,2,2),
          # label.position=c("down", "down", "down", "down", "down", "up",  "up",  "up",  "up",  "up"),
          # label.length=c(.5,.8,.8,.5,,.5,1,1,1,1,1),
          # label.direction=c("downup"),
          label.cex = c(1,1,.8,.8,1,1,.8,.8,1,1,.8,.8),
          label.length=c(.3,.3,.5,.5,.3,.3,.5,.5,.3,.3,.5,.5),
          labels=d$V1, buffer.days=150,
          label.color=c("blue", "blue", "grey", "grey","blue", "blue", "grey", "grey","blue", "blue","grey", "grey"),
          point.color=c("blue", "blue", "grey", "grey","blue", "blue", "grey", "grey","blue", "blue","grey", "grey")
)
dev.off()

# APPENDIX A4: CONTINUITY OF THE FORCING VARIABLE -------------------------
# Histograms of the forcing variable

m2008 <- subset(m, m$electoral_cycle==2008)
m2012 <- subset(m, m$electoral_cycle==2012)
m2016 <- subset(m, m$electoral_cycle==2016)

pdf("histogram_forcingvariable.pdf", width=9, height=5)
par(mfrow = c(2, 2), mar=c(5.1,5.5,4.1,2.1))
hist(m$ideb_gap, breaks=100,xlab="IDEB score - IDEB target", main="2007, 2011 and 2015")
abline(v=-0.05,col="red")
legend("topright", c("RD threshold"),lty=1, col="red",bty="n")
hist(m2008$ideb_gap, breaks=100,xlab="IDEB score - IDEB target", main="2007")
abline(v=-0.05,col="red")
legend("topright", c("RD threshold"),lty=1, col="red",bty="n")
hist(m2012$ideb_gap, breaks=100,xlab="IDEB score - IDEB target", main="2011")
abline(v=-0.05,col="red")
legend("topright", c("RD threshold"),lty=1, col="red",bty="n")
hist(m2016$ideb_gap, breaks=100,xlab="IDEB score - IDEB target", main="2015")
abline(v=-0.05,col="red")
legend("topright", c("RD threshold"),lty=1, col="red",bty="n")
dev.off()

# McCrary density tests

pdf("density_forcingvariable.pdf", width=9, height=5)
par(mfrow = c(2, 2), mar=c(5.1,5.5,4.1,2.1))
DCdensity(m$ideb_gap,c=-0.05)
title(main="2007, 2011 and 2015", xlab = "IDEB target - IDEB score", ylab="Density",
      cex.lab=1,cex.main=1.2)
abline(v=-0.05,col="red")
legend("topleft",c(paste0("p-value: ",round(DCdensity(m$ideb_gap,c=-0.05,plot=F),3))),bty="n",cex=0.9)
DCdensity(m2008$ideb_gap,c=-0.05)
title(main="2007", xlab = "IDEB target - IDEB score", ylab="Density",
      cex.lab=1,cex.main=1.2)
abline(v=-0.05,col="red")
legend("topleft",c(paste0("p-value: ",round(DCdensity(m2008$ideb_gap,c=-0.05,plot=F),3))),bty="n",cex=0.9)
DCdensity(m2012$ideb_gap,c=-0.05)
title(main="2011", xlab = "IDEB target - IDEB score", ylab="Density",
      cex.lab=1,cex.main=1.2)
abline(v=-0.05,col="red")
legend("topleft",c(paste0("p-value: ",round(DCdensity(m2012$ideb_gap,c=-0.05,plot=F),3))),bty="n",cex=0.9)
DCdensity(m2016$ideb_gap,c=-0.05)
title(main="2015", xlab = "IDEB target - IDEB score", ylab="Density",
      cex.lab=1,cex.main=1.2)
abline(v=-0.05,col="red")
legend("topleft",c(paste0("p-value: ",round(DCdensity(m2016$ideb_gap,c=-0.05,plot=F),3))),bty="n",cex=0.9)
dev.off()

# APPENDIX A5: CONTINUITY OF PRE-TREATMENT COVARIATES ---------------------
covariate <- c("incumbent_mayor_voteshare_previous", "incumbent_mayor_ran", "electoral_concentration", "incumbent_party_pt", "incumbent_party_psdb", "incumbent_party_pmdb", "incumbent_party_ran", "population_log", "share_poor", "tenured_employees_share", "local_media_2012")
bandwidth <- c()
rdestimate <- c()
rdpvalue <- c()
rdse <- c()

for(i in 1:length(covariate)){
  calonico <- rdrobust(mm[,covariate[i]][[1]], mm$ideb_gap_centered, covs=mm$electoral_cycle_2012 + mm$electoral_cycle_2016)
  bandwidth <- c(bandwidth, calonico$bws[[1,1]])
  rd <- felm(mm[,covariate[i]][[1]] ~ ideb_gap_centered*treated + electoral_cycle_2012 + electoral_cycle_2016, data=mm, subset=abs(mm$ideb_gap_centered)<bandwidth[i])
  rdestimate <- c(rdestimate, summary(rd,robust=T)$coef["treated",1])
  rdpvalue <- c(rdpvalue, summary(rd,robust=T)$coef["treated",4][[1]])
  rdse <- c(rdse, summary(rd,robust=T)$coef["treated",2][[1]])
}

balance <- as_tibble(cbind(covariate, rdestimate, rdpvalue, rdse, bandwidth))
balance$covariate <- c("Mayor vote share in previous election", "Incumbent mayor ran", "Electoral concentration", "Incumbent belongs to PT", "Incumbent belongs to PSDB", "Incumbent belongs to PMDB", "Incumbent party ran", "Population (logged)", "Share of population in poverty", "Share of municipal employees tenured", "Local media stations")
balance <- balance %>%
  mutate(estimate = as.numeric(as.character(rdestimate)), p.value = as.numeric(as.character(rdpvalue)), std.error = as.numeric(as.character(rdse)), bandwidth = as.numeric(as.character(bandwidth)))
rownames(balance) <- balance$covariate
balance <- balance %>%
  dplyr::select(covariate, estimate, std.error, p.value)
colnames(balance) <- c("Covariate", "RD estimate", "Std. error", "p value")
print(xtable(balance, digits=3,align=c("llccc")),
      include.rownames=FALSE,
      floating=F,
      comment=F,
      file="rdd_continuity_covariates.tex")

# APPENDIX A6: DATA CONSTRAINTS -------------------------------------------

# Number of observations for each constraint, for Table A3
nrow(m) # 16710 observations in the dataset, but 5 municipalities were created in 2013, so universe is 5565 + 5565 + 5570 = 16700
nrow(subset(m, !is.na(m$ideb))) # omitting those that lack an IDEB score
nrow(subset(m, !is.na(m$ideb) & !is.na(m$ideb_target))) # omitting those that also lack an IDEB target
nrow(subset(m, !is.na(m$ideb) & !is.na(m$ideb_target) & m$incumbent_mayor_cannot_run==0)) # omitting those where the mayor is not eligible to run
nrow(subset(m, !is.na(m$ideb) & !is.na(m$ideb_target) & m$incumbent_mayor_cannot_run==0 & !is.na(m$cpf_candidate))) # omitting those where the data has no unique ID for the mayor
nrow(subset(m, !is.na(m$ideb) & !is.na(m$ideb_target) & m$incumbent_mayor_cannot_run==0 & !is.na(m$cpf_candidate) & m$supplementary_election==0)) # omitting those where there was a supplementary election
nrow(subset(m, !is.na(m$ideb) & !is.na(m$ideb_target) & m$incumbent_mayor_cannot_run==0 & !is.na(m$cpf_candidate) & m$supplementary_election==0 & m$ef1_only==1)) # omitting those where there is an IDEB signal for middle schools
nrow(subset(m, !is.na(m$ideb) & !is.na(m$ideb_target) & m$incumbent_mayor_cannot_run==0 & !is.na(m$cpf_candidate) & m$supplementary_election==0 & m$ef1_only==1 & m$incumbent_mayor_ran==1)) # omitting those where the mayor did not run 

# Table A4 characterizing the sample
m$rd_sample <- ifelse(m$incumbent_mayor_cannot_run==0 & m$supplementary_election==0 & m$ef1_only==1 & !is.na(m$ideb_gap_centered),1,0)
m$rd_sample_bw <- ifelse(m$rd_sample==1 & abs(m$ideb_gap_centered)<r1_cct$bws[[1,1]],1,0)

m1 <- lm(rd_sample ~ population_log + share_poor + electoral_concentration + tenured_employees_share  + electoral_cycle_2012 + electoral_cycle_2016, data=m)
m2 <- lm(rd_sample_bw ~ population_log + share_poor + electoral_concentration + tenured_employees_share  + electoral_cycle_2012 + electoral_cycle_2016, data=m)
m3 <- lm(rd_sample_bw ~ population_log + share_poor + electoral_concentration + tenured_employees_share  + electoral_cycle_2012 + electoral_cycle_2016, data=m, subset=m$rd_sample==1)

texreg(list(m1,m3,m2),
       file="rdd_sample.tex", table=T, float.pos="htp", scalebox=.9,
       digits=3,
       override.se = list(coeftest(m1, type="HC2")[,2], coeftest(m2, type="HC2")[,2], coeftest(m3, type="HC2")[,2]),
       override.pvalues = list(coeftest(m1, type="HC2")[,4], coeftest(m2, type="HC2")[,4], coeftest(m3, type="HC2")[,4]),
       booktabs = T, use.packages=F,
       custom.model.names = c("In RD sample", "In bandwidth", "In RD sample and bw"),
       custom.coef.map = list("population_log" = "Population (logged)", "share_poor" = "Percent residents poor", "electoral_concentration" = "Electoral concentration", "tenured_employees_share" = "Share workers tenured", "(Intercept)" = "Constant"),
       custom.gof.rows = list("Election fixed effects" = c("Yes", "Yes", "Yes")),
       custom.gof.names = c("R-squared", "Observations"),
       reorder.gof = c(1, 3, 2),
       include.adjrs = F, 
       stars = c(0.01, 0.05, 0.1),
       caption="Characterization of the RDD effective sample. Dependent variable is a dummy for observations that enter the RD sample (versus all observations), the 0.39 bandwidth (versus those in the RD sample), and the RD sample and the 0.39 bandwidth (versus all observations). HC2 heteroskedasticity consistentstandard errors in parentheses.",
       label="tab:rdd_sample",
       custom.coef.names=c("Constant", NA))

# APPENDIX A7: EFFECT OF MEETING THE TARGET ON VOTE SHARE ----------------

# Create a copy of the dataset where the vote share of incumbents that did not run is set to NA, instead of 0
mm_vs <- mm
mm_vs$incumbent_mayor_voteshare <- ifelse(mm_vs$incumbent_mayor_voteshare==0, NA, mm_vs$incumbent_mayor_voteshare)

# Table A5: Effect of meeting the target on vote share
## Fit models: local linear and "robust", with and without controls
v1_cct <- rdrobust(mm_vs$incumbent_mayor_voteshare, mm_vs$ideb_gap_centered, covs=mm_vs$electoral_cycle_2012 + mm_vs$electoral_cycle_2016,all=T)
v1_llr <- felm(incumbent_mayor_voteshare ~ treated*ideb_gap_centered + electoral_cycle_2012 + electoral_cycle_2016, data=mm_vs, subset=abs(mm_vs$ideb_gap_centered)<v1_cct$bws[[1,1]])
v2_cct <- rdrobust(mm_vs$incumbent_mayor_voteshare, mm_vs$ideb_gap_centered, covs=mm_vs$electoral_cycle_2012 + mm_vs$electoral_cycle_2016 + mm_vs$incumbent_mayor_voteshare_previous + mm_vs$incumbent_party_pt + mm_vs$incumbent_party_psdb + mm_vs$incumbent_party_pmdb + mm_vs$population_log + mm_vs$share_poor + mm_vs$tenured_employees_share + mm_vs$incumbent_party_ran, all=T)
v2_llr <- felm(incumbent_mayor_voteshare ~ treated*ideb_gap_centered + electoral_cycle_2012 + electoral_cycle_2016 + incumbent_mayor_voteshare_previous + incumbent_party_pt + incumbent_party_psdb + incumbent_party_pmdb + population_log + share_poor + tenured_employees_share + incumbent_party_ran, data=mm_vs, subset=abs(mm_vs$ideb_gap_centered)<v2_cct$bws[[1,1]])
# Build table by hand (rdrobust models not compatible with texreg, broom, stargazer)
c1 <- c(paste(round(summary(v1_llr,robust=T)$coef["treated",1],3),
              ifelse(summary(v1_llr,robust=T)$coef["treated",4]<0.01,"***",
                     ifelse(summary(v1_llr,robust=T)$coef["treated",4]<0.05,"**",
                            ifelse(summary(v1_llr,robust=T)$coef["treated",4]<0.1,"*",""))),sep=""),
        paste("(",round(summary(v1_llr,robust=T)$coef["treated",2],3),")",sep=""),
        "Yes", "No", 
        round(v1_cct$bws[[1,1]],3),
        v1_llr$N)
c2 <- c(paste(round(summary(v2_llr,robust=T)$coef["treated",1],3),
              ifelse(summary(v2_llr,robust=T)$coef["treated",4]<0.01,"***",
                     ifelse(summary(v2_llr,robust=T)$coef["treated",4]<0.05,"**",
                            ifelse(summary(v2_llr,robust=T)$coef["treated",4]<0.1,"*",""))),sep=""),
        paste("(",round(summary(v2_llr,robust=T)$coef["treated",2],3),")",sep=""),
        "Yes", "Yes", 
        round(v2_cct$bws[[1,1]],3),
        v2_llr$N)
c3 <- c(paste(round(v1_cct$coef[3],3),
              ifelse(v1_cct$pv[3]<0.01,"***",
                     ifelse(v1_cct$pv[3]<0.05,"**",
                            ifelse(v1_cct$pv[3]<0.1,"*",""))),sep=""),
        paste("(",round(v1_cct$se[3],3),")",sep=""),
        "Yes", "No", 
        round(v1_cct$bws[[1,1]],3),
        sum(v1_cct$N_h))
c4 <- c(paste(round(v2_cct$coef[3],3),
              ifelse(v2_cct$pv[3]<0.01,"***",
                     ifelse(v2_cct$pv[3]<0.05,"**",
                            ifelse(v2_cct$pv[3]<0.1,"*",""))),sep=""),
        paste("(",round(v2_cct$se[3],3),")",sep=""),
        "Yes", "Yes", 
        round(v2_cct$bws[[1,1]],3),
        sum(v2_cct$N_h))

t2 <- as.table(cbind(c1,c2,c3,c4))
rownames(t2) <- c("IDEB target met", "", "Election cycle fixed effects", "Controls", "Bandwidth", "Observations")
colnames(t2) <- c("Linear", "Linear", "Robust", "Robust")

print (t2)
print(file="rdd_results_voteshare.tex",
      xtable(t2,
             align=c("lcccc")),
      caption.placement="top",
      rowname=names,
      comment=F,
      floating=F,
      booktabs=T)

# Figure A5: Effect of meeting the target on vote share
pdf("rdplot_voteshare.pdf",width=9,height=5)
rd_plot(x = mm_vs$ideb_gap, y = mm_vs$incumbent_mayor_voteshare, cutpoint = -0.05,
        xrange = c(-.55,.45), yrange=c(.3,.7),bins=50,
        maintitle="", xtitle = "IDEB score - IDEB target",
        ytitle = "Incumbent mayor vote-share",
        legendabove="Municipalities that \n met their IDEB target",
        legendbelow="Municipalities that \n missed their IDEB target")
grid()
dev.off()

# Table A6: Effect of meeting the target on decisions to run
# Fit models
c1_cct <- rdrobust(mm$incumbent_mayor_ran, mm$ideb_gap_centered, covs=mm$electoral_cycle_2012 + mm$electoral_cycle_2016,all=T)
c1_llr <- felm(incumbent_mayor_ran ~ treated*ideb_gap_centered + electoral_cycle_2012 + electoral_cycle_2016, data=mm, subset=abs(mm$ideb_gap_centered)<c1_cct$bws[[1,1]])
c2_cct <- rdrobust(mm$incumbent_mayor_ran, mm$ideb_gap_centered, covs=mm$electoral_cycle_2012 + mm$electoral_cycle_2016 + mm$incumbent_mayor_voteshare_previous + mm$incumbent_party_pt + mm$incumbent_party_psdb + mm$incumbent_party_pmdb + mm$population_log + mm$share_poor + mm$tenured_employees_share + mm$incumbent_party_ran, all=T)
c2_llr <- felm(incumbent_mayor_ran ~ treated*ideb_gap_centered + electoral_cycle_2012 + electoral_cycle_2016 + incumbent_mayor_voteshare_previous + incumbent_party_pt + incumbent_party_psdb + incumbent_party_pmdb + population_log + share_poor + tenured_employees_share + incumbent_party_ran, data=mm, subset=abs(mm$ideb_gap_centered)<c2_cct$bws[[1,1]])

# Build table by hand
# Build table by hand (rdrobust models not compatible with texreg, broom, stargazer)
c1 <- c(paste(round(summary(c1_llr,robust=T)$coef["treated",1],3),
              ifelse(summary(c1_llr,robust=T)$coef["treated",4]<0.01,"***",
                     ifelse(summary(c1_llr,robust=T)$coef["treated",4]<0.05,"**",
                            ifelse(summary(c1_llr,robust=T)$coef["treated",4]<0.1,"*",""))),sep=""),
        round(summary(c1_llr,robust=T)$coef["treated",2],3),
        "Yes", "No", 
        round(c1_cct$bws[[1,1]],3),
        c1_llr$N)
c2 <- c(paste(round(summary(c2_llr,robust=T)$coef["treated",1],3),
              ifelse(summary(c2_llr,robust=T)$coef["treated",4]<0.01,"***",
                     ifelse(summary(c2_llr,robust=T)$coef["treated",4]<0.05,"**",
                            ifelse(summary(c2_llr,robust=T)$coef["treated",4]<0.1,"*",""))),sep=""),
        round(summary(c2_llr,robust=T)$coef["treated",2],3),
        "Yes", "Yes", 
        round(c2_cct$bws[[1,1]],3),
        c2_llr$N)
c3 <- c(paste(round(c1_cct$coef[3],3),
              ifelse(c1_cct$pv[3]<0.01,"***",
                     ifelse(c1_cct$pv[3]<0.05,"**",
                            ifelse(c1_cct$pv[3]<0.1,"*",""))),sep=""),
        round(c1_cct$se[3],3),
        "Yes", "No", 
        round(c1_cct$bws[[1,1]],3),
        sum(c1_cct$N_h))
c4 <- c(paste(round(c2_cct$coef[3],3),
              ifelse(c2_cct$pv[3]<0.01,"***",
                     ifelse(c2_cct$pv[3]<0.05,"**",
                            ifelse(c2_cct$pv[3]<0.1,"*",""))),sep=""),
        round(c2_cct$se[3],3),
        "Yes", "Yes", 
        round(c2_cct$bws[[1,1]],3),
        sum(c2_cct$N_h))

t <- as.table(cbind(c1,c2,c3,c4))
rownames(t) <- c("IDEB target met", "", "Election cycle fixed effects", "Controls", "Bandwidth", "Observations")
colnames(t) <- c("Linear", "Linear", "Robust", "Robust")

# Export table
print(file="rdd_results_decisiontorun.tex",
      xtable(t,
             align=c("lcccc")),
      caption.placement="top",
      rowname=names,
      comment=F,
      floating=F,
      booktabs=T)

# APPENDIX A8: ALTERNATIVE BANDWIDTHS -------------------------------------

# Re-election
bw <- seq(from=r2_cct$bws[[1]]/2, to=r2_cct$bws[[1]]*1.5, by=0.005)
late <- c()
cilow <- c()
ciup <- c()
nobs <- c()
for (i in 1:length(bw)){
  r <- felm(incumbent_mayor_reelected ~ treated*ideb_gap_centered + electoral_cycle_2012 + electoral_cycle_2016 + incumbent_mayor_voteshare_previous + incumbent_party_pt + incumbent_party_psdb + incumbent_party_pmdb + population_log + share_poor + tenured_employees_share + incumbent_party_ran, data=mm, subset=abs(mm$ideb_gap_centered)<bw[i])
  late <- c(late, summary(r, robust=T)$coef["treated",1])
  cilow <- c(cilow, summary(r, robust=T)$coef["treated",1] - qt(.975, summary(r)$df[[1]])*summary(r, robust=T)$coef["treated",2])
  ciup <- c(ciup, summary(r, robust=T)$coef["treated",1] + qt(.975, summary(r)$df[[1]])*summary(r, robust=T)$coef["treated",2])
  nobs <- c(nobs, summary(r, robust=T)$N)
}
results <- data.frame(bw,late,cilow,ciup,nobs)

pdf("robustness_bw_reelection.pdf", width=9,height=5)
plot(results$bw, results$late,xlab="Bandwidth",
     ylab="LATE and 95% confidence intervals",pch=19, main="",ylim=c(-0.25,0.1))
arrows(results$bw, results$cilow, results$bw, results$ciup, length=0.05, angle=90, code=3,col="darkgrey")
abline(h=0,col="red",lty=2)
abline(v=r2_cct$bws[[1]],col="blue",lwd=2)
legend("topright",c("CCT optimal bandwidth"),lwd=2,col="blue",bty="n")
dev.off()

# Vote share
bw <- seq(from=v2_cct$bws[[1]]/2, to=v2_cct$bws[[1]]*1.5, by=0.005)
late <- c()
cilow <- c()
ciup <- c()
nobs <- c()
for (i in 1:length(bw)){
  r <- felm(incumbent_mayor_voteshare ~ treated*ideb_gap_centered + electoral_cycle_2012 + electoral_cycle_2016 + incumbent_mayor_voteshare_previous + incumbent_party_pt + incumbent_party_psdb + incumbent_party_pmdb + population_log + share_poor + tenured_employees_share + incumbent_party_ran, data=mm_vs, subset=abs(mm_vs$ideb_gap_centered)<bw[i])
  late <- c(late, summary(r, robust=T)$coef["treated",1])
  cilow <- c(cilow, summary(r, robust=T)$coef["treated",1] - qt(.975, summary(r)$df[[1]])*summary(r, robust=T)$coef["treated",2])
  ciup <- c(ciup, summary(r, robust=T)$coef["treated",1] + qt(.975, summary(r)$df[[1]])*summary(r, robust=T)$coef["treated",2])
  nobs <- c(nobs, summary(r, robust=T)$N)
}
results <- data.frame(bw,late,cilow,ciup,nobs)

pdf("robustness_bw_voteshare.pdf", width=9,height=5)
plot(results$bw,results$late,xlab="Bandwidth",
     ylab="LATE and 95% confidence intervals",pch=19, main="",ylim=c(-0.12,0.075))
arrows(results$bw, results$cilow, results$bw, results$ciup, length=0.05, angle=90, code=3,col="darkgrey")
abline(h=0,col="red",lty=2)
abline(v=v2_cct$bws[[1]],col="blue",lwd=2)
legend("topright",c("CCT optimal bandwidth"),lwd=2,col="blue",bty="n")
dev.off()

# APPENDIX A9: PLACEBO TESTS ---------------------------------------

placebo_cutoff <- seq(from=-1.1,1.1,0.2)
late <- c()
cilow <- c()
ciup <- c()
nobs <- c()

for (i in placebo_cutoff){
  mm_placebo <- mm
  mm_placebo$treated_placebo <- ifelse(mm_placebo$ideb_gap_centered>0+i,1,0)
  mm_placebo$ideb_gap_centered_placebo <- mm_placebo$ideb_gap_centered-i
  b <- rdrobust(mm_placebo$incumbent_mayor_reelected, mm_placebo$ideb_gap_centered_placebo, covs=mm_placebo$electoral_cycle_2012 + mm_placebo$electoral_cycle_2016 + mm_placebo$incumbent_mayor_voteshare_previous + mm_placebo$incumbent_party_pt + mm_placebo$incumbent_party_psdb + mm_placebo$incumbent_party_pmdb + mm_placebo$population_log + mm_placebo$share_poor + mm_placebo$tenured_employees_share + mm_placebo$incumbent_party_ran,all=T)
  r <- felm(incumbent_mayor_reelected ~ treated_placebo*ideb_gap_centered_placebo + electoral_cycle_2012 + electoral_cycle_2016 + incumbent_mayor_voteshare_previous + incumbent_party_pt + incumbent_party_psdb + incumbent_party_pmdb + population_log + share_poor + tenured_employees_share + incumbent_party_ran, data=mm_placebo, subset=abs(mm_placebo$ideb_gap_centered_placebo)<b$bws[[1,1]])
  late <- c(late, summary(r, robust=T)$coef["treated_placebo",1])
  cilow <- c(cilow, summary(r, robust=T)$coef["treated_placebo",1] - qt(.975, summary(r)$df[[1]])*summary(r)$coef["treated_placebo",2])
  ciup <- c(ciup, summary(r, robust=T)$coef["treated_placebo",1] + qt(.975, summary(r)$df[[1]])*summary(r)$coef["treated_placebo",2])
  nobs <- c(nobs, summary(r, robust=T)$N)
}
results <- data.frame(placebo_cutoff,late,cilow,ciup,nobs)

pdf("placebotests_threshold.pdf", width=9,height=5)
plot(results$placebo_cutoff,results$late,xlab="Placebo discontinuity threshold",
     ylab="LATE and 95% confidence intervals",pch=19, main="",ylim=c(min(cilow),max(ciup)))
arrows(results$placebo_cutoff, results$cilow, results$placebo_cutoff, results$ciup, length=0.05, angle=90, code=3,col="darkgrey")
abline(h=0,col="red",lty=2)
dev.off()

# APPENDIX A10: NO RESTRICTIONS ----------

mm <- m

# Re-election
r1_cct <- rdrobust(m$incumbent_mayor_reelected, m$ideb_gap_centered, covs=m$electoral_cycle_2012 + m$electoral_cycle_2016,all=T)
r1_llr <- felm(incumbent_mayor_reelected ~ treated*ideb_gap_centered + electoral_cycle_2012 + electoral_cycle_2016, data=mm, subset=abs(m$ideb_gap_centered)<r1_cct$bws[[1,1]])
r2_cct <- rdrobust(m$incumbent_mayor_reelected, m$ideb_gap_centered, covs=m$electoral_cycle_2012 + m$electoral_cycle_2016 + m$incumbent_mayor_voteshare_previous + m$incumbent_party_pt + m$incumbent_party_psdb + m$incumbent_party_pmdb + m$population_log + m$share_poor + m$tenured_employees_share + m$incumbent_party_ran,all=T)
r2_llr <- felm(incumbent_mayor_reelected ~ treated*ideb_gap_centered + electoral_cycle_2012 + electoral_cycle_2016 + incumbent_mayor_voteshare_previous + incumbent_party_pt + incumbent_party_psdb + incumbent_party_pmdb + population_log + share_poor + tenured_employees_share + incumbent_party_ran, data=m, subset=abs(m$ideb_gap_centered)<r2_cct$bws[[1,1]])

# Build table by hand (rdrobust models not compatible with texreg, broom, stargazer)
c1 <- c(paste(round(summary(r1_llr,robust=T)$coef["treated",1],3),
              ifelse(summary(r1_llr,robust=T)$coef["treated",4]<0.01,"***",
                     ifelse(summary(r1_llr,robust=T)$coef["treated",4]<0.05,"**",
                            ifelse(summary(r1_llr,robust=T)$coef["treated",4]<0.1,"*",""))),sep=""),
        paste("(",round(summary(r1_llr,robust=T)$coef["treated",2],3),")",sep=""),
        "Yes", "No", 
        round(r1_cct$bws[[1,1]],3),
        r1_llr$N)
c2 <- c(paste(round(summary(r2_llr,robust=T)$coef["treated",1],3),
              ifelse(summary(r2_llr,robust=T)$coef["treated",4]<0.01,"***",
                     ifelse(summary(r2_llr,robust=T)$coef["treated",4]<0.05,"**",
                            ifelse(summary(r2_llr,robust=T)$coef["treated",4]<0.1,"*",""))),sep=""),
        paste("(",round(summary(r2_llr,robust=T)$coef["treated",2],3),")",sep=""),
        "Yes", "Yes", 
        round(r2_cct$bws[[1,1]],3),
        r2_llr$N)
c3 <- c(paste(round(r1_cct$coef[3],3),
              ifelse(r1_cct$pv[3]<0.01,"***",
                     ifelse(r1_cct$pv[3]<0.05,"**",
                            ifelse(r1_cct$pv[3]<0.1,"*",""))),sep=""),
        paste("(",round(r1_cct$se[3],3),")",sep=""),
        "Yes", "No", 
        round(r1_cct$bws[[1,1]],3),
        sum(r1_cct$N_h))
c4 <- c(paste(round(r2_cct$coef[3],3),
              ifelse(r2_cct$pv[3]<0.01,"***",
                     ifelse(r2_cct$pv[3]<0.05,"**",
                            ifelse(r2_cct$pv[3]<0.1,"*",""))),sep=""),
        paste("(",round(r2_cct$se[3],3),")",sep=""),
        "Yes", "Yes", 
        round(r2_cct$bws[[1,1]],3),
        sum(r2_cct$N_h))

t1 <- as.table(cbind(c1,c2,c3,c4))
rownames(t1) <- c("IDEB target met", "", "Election cycle fixed effects", "Controls", "Bandwidth", "Observations")
colnames(t1) <- c("Linear", "Linear", "Robust", "Robust")

print(file="rdd_noconstraints_reelection.tex",float.pos="htp", 
      xtable(t1,
             align=c("lcccc")),
      caption.placement="top",
      rowname=names,
      comment=F,
      floating=F,
      booktabs=T)

# Vote share
m_vs <- m
m_vs$incumbent_mayor_voteshare <- ifelse(m_vs$incumbent_mayor_voteshare==0, NA, m_vs$incumbent_mayor_voteshare)

v1_cct <- rdrobust(m_vs$incumbent_mayor_voteshare, m_vs$ideb_gap_centered, covs=m_vs$electoral_cycle_2012 + m_vs$electoral_cycle_2016, all=T)
v1_llr <- felm(incumbent_mayor_voteshare ~ treated*ideb_gap_centered + electoral_cycle_2012 + electoral_cycle_2016, data=m_vs, subset=abs(m_vs$ideb_gap_centered)<v1_cct$bws[[1,1]])
v2_cct <- rdrobust(m_vs$incumbent_mayor_voteshare, m_vs$ideb_gap_centered, covs=m_vs$electoral_cycle_2012 + m_vs$electoral_cycle_2016 + m_vs$incumbent_mayor_voteshare_previous + m_vs$incumbent_party_pt + m_vs$incumbent_party_psdb + m_vs$incumbent_party_pmdb + m_vs$population_log + m_vs$share_poor + m_vs$tenured_employees_share + m_vs$incumbent_party_ran, all=T)
v2_llr <- felm(incumbent_mayor_voteshare ~ treated*ideb_gap_centered + electoral_cycle_2012 + electoral_cycle_2016 + incumbent_mayor_voteshare_previous + incumbent_party_pt + incumbent_party_psdb + incumbent_party_pmdb + population_log + share_poor + tenured_employees_share + incumbent_party_ran, data=m_vs, subset=abs(m_vs$ideb_gap_centered)<v2_cct$bws[[1,1]])
# Build table by hand (rdrobust models not compatible with texreg, broom, stargazer)
c1 <- c(paste(round(summary(v1_llr,robust=T)$coef["treated",1],3),
              ifelse(summary(v1_llr,robust=T)$coef["treated",4]<0.01,"***",
                     ifelse(summary(v1_llr,robust=T)$coef["treated",4]<0.05,"**",
                            ifelse(summary(v1_llr,robust=T)$coef["treated",4]<0.1,"*",""))),sep=""),
        paste("(",round(summary(v1_llr,robust=T)$coef["treated",2],3),")",sep=""),
        "Yes", "No", 
        round(v1_cct$bws[[1,1]],3),
        v1_llr$N)
c2 <- c(paste(round(summary(v2_llr,robust=T)$coef["treated",1],3),
              ifelse(summary(v2_llr,robust=T)$coef["treated",4]<0.01,"***",
                     ifelse(summary(v2_llr,robust=T)$coef["treated",4]<0.05,"**",
                            ifelse(summary(v2_llr,robust=T)$coef["treated",4]<0.1,"*",""))),sep=""),
        paste("(",round(summary(v2_llr,robust=T)$coef["treated",2],3),")",sep=""),
        "Yes", "Yes", 
        round(v2_cct$bws[[1,1]],3),
        v2_llr$N)
c3 <- c(paste(round(v1_cct$coef[3],3),
              ifelse(v1_cct$pv[3]<0.01,"***",
                     ifelse(v1_cct$pv[3]<0.05,"**",
                            ifelse(v1_cct$pv[3]<0.1,"*",""))),sep=""),
        paste("(",round(v1_cct$se[3],3),")",sep=""),
        "Yes", "No", 
        round(v1_cct$bws[[1,1]],3),
        sum(v1_cct$N_h))
c4 <- c(paste(round(v2_cct$coef[3],3),
              ifelse(v2_cct$pv[3]<0.01,"***",
                     ifelse(v2_cct$pv[3]<0.05,"**",
                            ifelse(v2_cct$pv[3]<0.1,"*",""))),sep=""),
        paste("(",round(v2_cct$se[3],3),")",sep=""),
        "Yes", "Yes", 
        round(v2_cct$bws[[1,1]],3),
        sum(v2_cct$N_h))

t2 <- as.table(cbind(c1,c2,c3,c4))
rownames(t2) <- c("IDEB target met", "", "Election cycle fixed effects", "Controls", "Bandwidth", "Observations")
colnames(t2) <- c("Linear", "Linear", "Robust", "Robust")

print(file="rdd_noconstraints_voteshare.tex",float.pos="htp", 
      xtable(t2,
             align=c("lcccc")),
      caption.placement="top",
      rowname=names,
      comment=F,
      floating=F,
      booktabs=T)

# APPENDIX A11: HETEROGENEITY ANALYSES ---------------------------------------------

# Re-election
mm <- subset(m, m$incumbent_mayor_cannot_run==0 & m$supplementary_election==0 & m$ef1_only==1)
r1_cct <- rdrobust(mm$incumbent_mayor_reelected, mm$ideb_gap_centered, covs=mm$electoral_cycle_2012 + mm$electoral_cycle_2016 + mm$treated_share_pop_enroled_mun_high + mm$treated_forcing_share_pop_enroled_mun_high + mm$forcing_share_pop_enroled_mun_high + mm$forcing_share_pop_enroled_mun_high,all=T)
r2_cct <- rdrobust(mm$incumbent_mayor_reelected, mm$ideb_gap_centered, covs=mm$electoral_cycle_2012 + mm$electoral_cycle_2016 + mm$treated_share_pop_enroled_mun_high + mm$treated_forcing_share_pop_enroled_mun_high + mm$forcing_share_pop_enroled_mun_high + mm$forcing_share_pop_enroled_mun_high + mm$incumbent_mayor_voteshare_previous + mm$incumbent_party_pt + mm$incumbent_party_psdb + mm$incumbent_party_pmdb + mm$population_log + mm$share_poor + mm$tenured_employees_share + mm$incumbent_party_ran,all=T)
r1_llr <- felm(incumbent_mayor_reelected ~ treated*ideb_gap_centered*share_pop_enroled_mun_high + electoral_cycle_2012 + electoral_cycle_2012, data=mm, subset=abs(mm$ideb_gap_centered)<r1_cct$bws[[1,1]])
r2_llr <- felm(incumbent_mayor_reelected ~ treated*ideb_gap_centered*share_pop_enroled_mun_high + electoral_cycle_2012 + electoral_cycle_2012 + incumbent_mayor_voteshare_previous + incumbent_party_pt + incumbent_party_psdb + incumbent_party_pmdb + population_log + share_poor + tenured_employees_share + incumbent_party_ran, data=mm, subset=abs(mm$ideb_gap_centered)<r2_cct$bws[[1,1]])

# Build table by hand (rdrobust models not compatible with texreg, broom, stargazer)
c1 <- c(paste(round(summary(r1_llr,robust=T)$coef["treated",1],3),
              ifelse(summary(r1_llr,robust=T)$coef["treated",4]<0.01,"***",
                     ifelse(summary(r1_llr,robust=T)$coef["treated",4]<0.05,"**",
                            ifelse(summary(r1_llr,robust=T)$coef["treated",4]<0.1,"*",""))),sep=""),
        paste("(",round(summary(r1_llr,robust=T)$coef["treated",2],3),")",sep=""),
        paste(round(summary(r1_llr,robust=T)$coef["treated:share_pop_enroled_mun_high",1],3),
              ifelse(summary(r1_llr,robust=T)$coef["treated:share_pop_enroled_mun_high",4]<0.01,"***",
                     ifelse(summary(r1_llr,robust=T)$coef["treated:share_pop_enroled_mun_high",4]<0.05,"**",
                            ifelse(summary(r1_llr,robust=T)$coef["treated:share_pop_enroled_mun_high",4]<0.1,"*",""))),sep=""),
        paste("(",round(summary(r1_llr,robust=T)$coef["treated:share_pop_enroled_mun_high",2],3),")",sep=""),
        "Yes", "No", 
        round(r1_cct$bws[[1,1]],3),
        r1_llr$N)
c2 <- c(paste(round(summary(r2_llr,robust=T)$coef["treated",1],3),
              ifelse(summary(r2_llr,robust=T)$coef["treated",4]<0.01,"***",
                     ifelse(summary(r2_llr,robust=T)$coef["treated",4]<0.05,"**",
                            ifelse(summary(r2_llr,robust=T)$coef["treated",4]<0.1,"*",""))),sep=""),
        paste("(",round(summary(r2_llr,robust=T)$coef["treated",2],3),")",sep=""),
        paste(round(summary(r2_llr,robust=T)$coef["treated:share_pop_enroled_mun_high",1],3),
              ifelse(summary(r2_llr,robust=T)$coef["treated:share_pop_enroled_mun_high",4]<0.01,"***",
                     ifelse(summary(r2_llr,robust=T)$coef["treated:share_pop_enroled_mun_high",4]<0.05,"**",
                            ifelse(summary(r2_llr,robust=T)$coef["treated:share_pop_enroled_mun_high",4]<0.1,"*",""))),sep=""),
        paste("(",round(summary(r2_llr,robust=T)$coef["treated:share_pop_enroled_mun_high",2],3),")",sep=""),
        "Yes", "Yes", 
        round(r2_cct$bws[[1,1]],3),
        r2_llr$N)

rows <- c("IDEB target met", "", "IDEB target met x High enrolments", "", "Election cycle fixed effects", "Controls", "Bandwidth", "Observations")
t1 <- as.table(cbind(rows,c1,c2))
rownames(t1) <- c("IDEB target met", "", "IDEB target met x High enrolments", "", "Election cycle fixed effects", "Controls", "Bandwidth", "Observations")
colnames(t1) <- c(" ","Linear", "Linear")

print(file="rdd_results_heterogeneity_reelection.tex",
      xtable(t1,
             align=c("llcc")),
      caption.placement="top",
      include.rownames=F,
      comment=F,
      floating=F,
      booktabs=T)

# Vote share
mm_vs <- mm
mm_vs$incumbent_mayor_voteshare <- ifelse(mm_vs$incumbent_mayor_voteshare==0, NA, mm_vs$incumbent_mayor_voteshare)

v1_cct <- rdrobust(mm_vs$incumbent_mayor_voteshare, mm_vs$ideb_gap_centered, covs=mm_vs$electoral_cycle_2012 + mm_vs$electoral_cycle_2016 + mm_vs$treated_share_pop_enroled_mun_high + mm_vs$treated_forcing_share_pop_enroled_mun_high + mm_vs$forcing_share_pop_enroled_mun_high + mm_vs$forcing_share_pop_enroled_mun_high,all=T)
v2_cct <- rdrobust(mm_vs$incumbent_mayor_voteshare, mm_vs$ideb_gap_centered, covs=mm_vs$electoral_cycle_2012 + mm_vs$electoral_cycle_2016 + mm_vs$treated_share_pop_enroled_mun_high + mm_vs$treated_forcing_share_pop_enroled_mun_high + mm_vs$forcing_share_pop_enroled_mun_high + mm_vs$forcing_share_pop_enroled_mun_high + mm_vs$incumbent_mayor_voteshare_previous + mm_vs$incumbent_party_pt + mm_vs$incumbent_party_psdb + mm_vs$incumbent_party_pmdb + mm_vs$population_log + mm_vs$share_poor + mm_vs$tenured_employees_share + mm_vs$incumbent_party_ran,all=T)
v1_llr <- felm(incumbent_mayor_voteshare ~ treated*ideb_gap_centered*share_pop_enroled_mun_high + electoral_cycle_2012 + electoral_cycle_2012, data=mm_vs, subset=abs(mm_vs$ideb_gap_centered)<r1_cct$bws[[1,1]])
v2_llr <- felm(incumbent_mayor_voteshare ~ treated*ideb_gap_centered*share_pop_enroled_mun_high + electoral_cycle_2012 + electoral_cycle_2012 + incumbent_mayor_voteshare_previous + incumbent_party_pt + incumbent_party_psdb + incumbent_party_pmdb + population_log + share_poor + tenured_employees_share + incumbent_party_ran, data=mm_vs, subset=abs(mm_vs$ideb_gap_centered)<r2_cct$bws[[1,1]])

# Build table by hand (rdrobust models not compatible with texreg, broom, stargazer)
c1 <- c(paste(round(summary(v1_llr,robust=T)$coef["treated",1],3),
              ifelse(summary(v1_llr,robust=T)$coef["treated",4]<0.01,"***",
                     ifelse(summary(v1_llr,robust=T)$coef["treated",4]<0.05,"**",
                            ifelse(summary(v1_llr,robust=T)$coef["treated",4]<0.1,"*",""))),sep=""),
        paste("(",round(summary(v1_llr,robust=T)$coef["treated",2],3),")",sep=""),
        paste(round(summary(v1_llr,robust=T)$coef["treated:share_pop_enroled_mun_high",1],3),
              ifelse(summary(v1_llr,robust=T)$coef["treated:share_pop_enroled_mun_high",4]<0.01,"***",
                     ifelse(summary(v1_llr,robust=T)$coef["treated:share_pop_enroled_mun_high",4]<0.05,"**",
                            ifelse(summary(v1_llr,robust=T)$coef["treated:share_pop_enroled_mun_high",4]<0.1,"*",""))),sep=""),
        paste("(",round(summary(v1_llr,robust=T)$coef["treated:share_pop_enroled_mun_high",2],3),")",sep=""),
        "Yes", "No", 
        round(v1_cct$bws[[1,1]],3),
        v1_llr$N)
c2 <- c(paste(round(summary(v2_llr,robust=T)$coef["treated",1],3),
              ifelse(summary(v2_llr,robust=T)$coef["treated",4]<0.01,"***",
                     ifelse(summary(v2_llr,robust=T)$coef["treated",4]<0.05,"**",
                            ifelse(summary(v2_llr,robust=T)$coef["treated",4]<0.1,"*",""))),sep=""),
        paste("(",round(summary(v2_llr,robust=T)$coef["treated",2],3),")",sep=""),
        paste(round(summary(v2_llr,robust=T)$coef["treated:share_pop_enroled_mun_high",1],3),
              ifelse(summary(v2_llr,robust=T)$coef["treated:share_pop_enroled_mun_high",4]<0.01,"***",
                     ifelse(summary(v2_llr,robust=T)$coef["treated:share_pop_enroled_mun_high",4]<0.05,"**",
                            ifelse(summary(v2_llr,robust=T)$coef["treated:share_pop_enroled_mun_high",4]<0.1,"*",""))),sep=""),
        paste("(",round(summary(v2_llr,robust=T)$coef["treated:share_pop_enroled_mun_high",2],3),")",sep=""),
        "Yes", "Yes", 
        round(v2_cct$bws[[1,1]],3),
        v2_llr$N)

rows <- c("IDEB target met", "", "IDEB target met x High enrolments", "", "Election cycle fixed effects", "Controls", "Bandwidth", "Observations")
t2 <- as.table(cbind(rows,c1,c2))
rownames(t2) <- c("IDEB target met", "", "IDEB target met x High enrolments", "", "Election cycle fixed effects", "Controls", "Bandwidth", "Observations")
colnames(t2) <- c(" ","Linear", "Linear")

print(file="rdd_results_heterogeneity_voteshare.tex",
      xtable(t2,
             align=c("llcc")),
      caption.placement="top",
      include.rownames=F,
      comment=F,
      floating=F,
      booktabs=T)

# NOTES -- R version, platform, and loaded packages -------------------------
# sessionInfo(package = NULL)
# R version 3.6.3 (2020-02-29)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS Catalina 10.15.3
# 
# Matrix products: default
# BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
# LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib
# 
# Random number generation:
#   RNG:     Mersenne-Twister 
# Normal:  Inversion 
# Sample:  Rounding 
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] Hmisc_4.4-0       lattice_0.20-40   timelineS_0.1.1   lfe_2.8-5         Matrix_1.2-18    
# [6] xtable_1.8-4      texreg_1.37.1     rdrobust_0.99.7   rdd_0.57          Formula_1.2-3    
# [11] AER_1.2-9         survival_3.1-11   car_3.0-7         carData_3.0-3     lmtest_0.9-37    
# [16] zoo_1.8-7         sandwich_2.5-1    shiny_1.4.0.2     readxl_1.3.1      codebook_0.9.2   
# [21] electionsBR_0.3.1 forcats_0.5.0     stringr_1.4.0     dplyr_1.0.0       purrr_0.3.3      
# [26] readr_1.3.1       tidyr_1.0.2       tibble_3.0.0      ggplot2_3.3.0     tidyverse_1.3.0  
# 
# loaded via a namespace (and not attached):
#   [1] colorspace_1.4-1    ellipsis_0.3.0      rio_0.5.16          htmlTable_1.13.3   
# [5] base64enc_0.1-3     fs_1.4.1            rstudioapi_0.11     listenv_0.8.0      
# [9] farver_2.0.3        DT_0.13             fansi_0.4.1         lubridate_1.7.4    
# [13] xml2_1.3.0          codetools_0.2-16    splines_3.6.3       knitr_1.28         
# [17] jsonlite_1.6.1      broom_0.5.5         cluster_2.1.0       dbplyr_1.4.2       
# [21] png_0.1-7           compiler_3.6.3      httr_1.4.1          backports_1.1.5    
# [25] assertthat_0.2.1    fastmap_1.0.1       cli_2.0.2           later_1.0.0        
# [29] acepack_1.4.1       htmltools_0.4.0     tools_3.6.3         gtable_0.3.0       
# [33] glue_1.3.2          Rcpp_1.0.4          cellranger_1.1.0    vctrs_0.3.1        
# [37] nlme_3.1-145        crosstalk_1.1.0.1   xfun_0.12           globals_0.12.5     
# [41] openxlsx_4.1.4      rvest_0.3.5         mime_0.9            miniUI_0.1.1.1     
# [45] lifecycle_0.2.0     future_1.17.0       scales_1.1.0        hms_0.5.3          
# [49] promises_1.1.0      parallel_3.6.3      RColorBrewer_1.1-2  yaml_2.2.1         
# [53] curl_4.3            gridExtra_2.3       rpart_4.1-15        labelled_2.5.0     
# [57] latticeExtra_0.6-29 stringi_1.4.6       highr_0.8           checkmate_2.0.0    
# [61] rmdpartials_0.5.8   zip_2.0.4           repr_1.1.0          rlang_0.4.6        
# [65] pkgconfig_2.0.3     evaluate_0.14       htmlwidgets_1.5.1   labeling_0.3       
# [69] tidyselect_1.1.0    magrittr_1.5        R6_2.4.1            generics_0.0.2     
# [73] DBI_1.1.0           pillar_1.4.3        haven_2.3.1         foreign_0.8-76     
# [77] withr_2.1.2         nnet_7.3-13         abind_1.4-5         modelr_0.1.6       
# [81] crayon_1.3.4        utf8_1.1.4          rmarkdown_2.1       jpeg_0.1-8.1       
# [85] grid_3.6.3          data.table_1.12.8   reprex_0.3.0        digest_0.6.25      
# [89] httpuv_1.5.2        munsell_0.5.0       skimr_2.1.1      