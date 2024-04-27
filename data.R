#final exam categ ana

library(readxl)
data <- read_excel("school/4y2s/categorical analysis/final exam/DATA_ID.xlsx")
View(data)
attach(data)

#
#install.packages("seminr")
library(seminr)
measurement_model <- constructs(
  composite("ES", multi_items("ES_", 1:8)), #use single_item if only one question/item
  composite("SS", multi_items("SS_", 1:8)),
  composite("GS", multi_items("GS_", 1:8)),
  composite("IK", multi_items("IK_", 1:8)),
  composite("RP", multi_items("RP_", 1:8)),
  composite("FL", multi_items("FL_", 1:8)),
  composite("IB", multi_items("IB_", 1:8))
)

#create path; structural relationships between all constructs
structural_model <- relationships(
  paths(from=c("ES", "SS", "GS"), to=c("IB")),
  paths(from=c("IK", "RP", "FL"), to=c("IB"))
)

structuralmodel <- relationships(
  paths(from = c("ES", "SS", "GS"), to = c("IB")),
  paths(from = c("IK", "RP", "FL"), to = c("IB")))

#----
str(data)
print(measurement_model)
print(structural_model)
sum(is.na(data))

#estimating the model
pls_model <- estimate_pls(data=as.data.frame(data),
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











