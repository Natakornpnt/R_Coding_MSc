
Cullis_H2 <- function(model, geno_label = "GENO") 
{
  
  ## Extract genotypic variance
  var_g <- lme4::VarCorr(model, comp = "Variance")[[geno_label]][1]
  
  ## Extract genotypic conditional variances
  convars <- lme4::ranef(model, condVar = TRUE)
  g_convar <- attr(convars[[geno_label]], "postVar")
  
  ## Calculate VBLUP and avsed
  vblup <- 2 * mean(g_convar)
  avsed <- sqrt(vblup)
  
  ## Calculate generalized heritability
  H2 <- 1 - (vblup / (2 * var_g))
  
  ## Create and return output list
  out_list <- list("avsed" = avsed, "H2" = H2)
  return(out_list)
}
