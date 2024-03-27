library(mpitbR)

data <- subset(syn_cdta, t==1)
data <- na.omit(data)

svydata <- survey::svydesign(id=~psu, weights = ~weight, strata = ~stratum, data = data)

indicators <- list(d1 = c("d_nutr","d_cm"),
                   d2 = c("d_satt","d_educ"),
                   d3 = c("d_elct","d_sani","d_wtr","d_hsg","d_ckfl","d_asst"))

set <- mpitb.set(svydata, indicators = indicators, name = "myname", desc = "pref. desc")
