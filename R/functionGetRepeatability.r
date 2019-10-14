#### function for repeatability calculations ####

# Code author Pratik Gupte
# PhD student
# MARM group, GELIFES-RUG, NL
# Contact p.r.gupte@rug.nl

# write a function to extract the population repeatability
# from a linear mixed model of the class lmerMod
getRepeatability = function(x){

  # specify classes of acceptable models

  goodMods = c("lmerMod", "glmerMod")
  # check if the package is of the class lmerMod
  # if not, print an error message
  assertthat::assert_that(class(x) %in% goodMods,
              msg = "The model needs to be an LMM of the class lmerMod.")

  # get the random effects variance matrix
  y = as.data.frame(summary(x)$varcor)

  # repeatabilty is the variance of ID as a proportion
  # of all random effects variances
  repeatability = y[y$grp == "id", "vcov"]/sum(y[, "vcov"])

  return(repeatability)

}
