library(mpitbR)

data <- subset(syn_cdta)
data <- na.omit(data)

svydata <- survey::svydesign(id=~psu, weights = ~weight, strata = ~stratum, data = data)

indicators <- list(d1 = c("d_nutr","d_cm"),
                   d2 = c("d_satt","d_educ"),
                   d3 = c("d_elct","d_sani","d_wtr","d_hsg","d_ckfl","d_asst"))

# Specify mpitb project
set <- mpitb.set(svydata, indicators = indicators, name = "myname", desc = "pref. desc")

# Estimate the cross-sectional MPI and compare non-annualized changes over time
est <- mpitb.est(set, klist = c(33), measures = "M0", indmeasures = NULL,
                 tvar = "t", cotmeasures = "M0",
                 weights = "equal", over = c("area"))

coef(subset(est$lframe, measure == "M0" & t == 1))
confint(subset(est$lframe, measure == "M0" & t == 1))
summary(subset(est$lframe, measure == "M0" & t == 1))

coef(subset(est$cotframe, measure == "M0"))
confint(subset(est$cotframe, measure == "M0"))
summary(subset(est$cotframe, measure == "M0" & ctype == "abs" & ann == 0 & k == 33))
