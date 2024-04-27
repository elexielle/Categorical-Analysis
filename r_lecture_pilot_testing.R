#categ ana 07-17-23 lesson

library(readxl)
pilottesting <- read_excel("school/4y2s/categorical analysis/lectures/last lesson/pilottesting.xlsx")
View(pilottesting)

install.packages("seminr")
library(seminr)

#
measurement_model <- constructs(
  composite("RC", multi_items("RC_", 1:3)), #use single_item if only one question/item
  composite("TC", multi_items("TC_", 1:3)),
  composite("EA", multi_items("EA_", 1:12)),
  composite("PJP", multi_items("PJP_", 1:5))
)

#create path; structural relationships between all constructs
structural_model <- relationships(
  paths(from=c("TC", "RC"), to=c("EA")),
  paths(from=c("TC", "RC"), to=c("PJP")),
  paths(from=c("EA"), to=c("PJP"))
)

#estimating the model
pls_model <- estimate_pls(data=as.data.frame(pilottesting),
                      measurement_model=measurement_model,
                      structural_model=structural_model,
                      inner_weights=path_weighting,
                      missing=mean_replacement,
                      missing_value="-99")

#summarize the model results
summary_pls_model <- summary(pls_model)

#indicator reliability
summary_pls_model$loadings #indicator loadings
write.csv(x=summary_pls_model$loadings, file="indicator_loadings.csv")

#internal consistency reliability and convergent validity
summary_pls_model$reliability

#FLC (discriminant validity)
summary_pls_model$validity$fl_criteria

#heteromotrait-monotrait ratio (disc validity)
summary_pls_model$validity$htmt

#----
#using the recode
library(readxl)
pilottestingRECODE <- read_excel("school/4y2s/categorical analysis/lectures/last lesson/pilottestingRECODE.xlsx")
View(pilottestingRECODE)
attach(pilottestingRECODE)

#remove below 0.40
#q1, q5, q6, q12

measurement_model <- constructs(
  composite("RC", multi_items("RC_", 1:3)), #use single_item if only one question/item
  composite("TC", multi_items("TC_", 1:3)),
  composite("EA", multi_items("EA_", 9:10)),#c(2:4,7:11))),
  composite("PJP", multi_items("PJP_", 1:5))
)

#create path; structural relationships between all constructs
structural_model <- relationships(
  paths(from=c("TC", "RC"), to=c("EA")),
  paths(from=c("TC", "RC"), to=c("PJP")),
  paths(from=c("EA"), to=c("PJP"))
)

#estinating the model
pls_model <- estimate_pls(data=as.data.frame(pilottestingRECODE),
                          measurement_model=measurement_model,
                          structural_model=structural_model,
                          inner_weights=path_weighting,
                          missing=mean_replacement,
                          missing_value="-99")

#summarize the model results
summary_pls_model <- summary(pls_model)

#indicator reliability
summary_pls_model$loadings #indicator loadings
#write.csv(x=summary_pls_model$loadings, file="indicator_loadings.csv")

#internal consistency reliability and convergent validity
summary_pls_model$reliability

#FLC (discriminant validity)
summary_pls_model$validity$fl_criteria

#heteromotrait-monotrait ratio (disc validity)
summary_pls_model$validity$htmt





