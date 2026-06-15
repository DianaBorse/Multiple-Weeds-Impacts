# full reconstruction
dredgeraw <- read_excel("Fieldwork/dredgeresultsGLM10June.xlsx")
predictors <- c("Height", "DBH", "Slope", "Canopy", "Vascular", "WN", 
                "Disturbance", "Housing", "PopDensityHist", "PopDensityCurr",
                "RichnessResidentNatives", "RichnessResidentWeeds")

dredgeraw <- dredgeraw %>%
  rowwise() %>%
  mutate(formula = paste(
    "response ~",
    paste(predictors[unlist(c_across(all_of(predictors))) == 1], collapse = " + ")
  ))

dredgeraw$formula <- trimws(dredgeraw$formula)


library(purrr)

models <- map(dredgeraw$formula, ~ {
  try(lm(as.formula(.x), data = df_Env_Species.pca), silent = TRUE)
})

attr(dredgeraw, "models") <- models
class(dredgeraw) <- c("model.selection", class(dredgeraw))

bad_idx <- which(sapply(models, function(m) inherits(m, "try-error") || is.null(m)))
bad_idx


good_idx <- setdiff(seq_along(models), bad_idx)

dredge_clean <- dredge_raw[good_idx, ]
attr(dredge_clean, "models") <- models[good_idx]
class(dredge_clean) <- c("model.selection", class(dredge_clean))