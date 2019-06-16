
#' @export
fit1_med <- function(driver,
                     target,
                     mediator,
                     addcovar = NULL
){
  ll_d <- as.vector(logLik(lm(target ~ driver + addcovar)))
  ll_dm <- as.vector(logLik(lm(target ~ driver + mediator + addcovar)))
  ll_1 <- as.vector(logLik(lm(target ~ 1 + addcovar)))
  ll_1m <- as.vector(logLik(lm(target ~ 1 + mediator + addcovar)))
  out <- tibble::tibble(ll_d, ll_dm, ll_1, ll_1m)
  return(out)
}


#' @export
fit1_med_qtl2 <- function(driver,
                     target,
                     mediator,
                     addcovar = NULL,
                     kinship = NULL
){
  lod_no_med <- qtl2::fit1(genoprobs = driver, 
                           pheno = target, 
                           kinship = kinship, 
                           addcovar = addcovar
                           )$lod
  lod_med <- qtl2::fit1(genoprobs = driver, 
                        pheno = target, 
                        kinship = kinship, 
                        addcovar = cbind(addcovar, mediator)
                        )$lod
  
  out <- tibble::tibble(lod_no_med, lod_med)
  return(out)
}


  