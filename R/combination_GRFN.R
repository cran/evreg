#' Combination of Gaussian random fuzzy numbers
#'
#' \code{combination_GRFN} combines two Gaussian random fuzzy numbers using the generalized
#' product-intersection rule with soft or hard normalization.
#'
#' @param GRFN1 A Gaussian random fuzzy number, encoded as a list with components mu, sig
#' and h.
#' @param GRFN2 A Gaussian random fuzzy number, encoded as a list with components mu, sig
#' and h.
#' @param soft If TRUE (default), the combination rule with soft normalization is used.
#' Otherwise, hard normalization is employed.
#'
#' @return The combined Gaussian random fuzzy number, encoded as a list with components mu, sig
#' and h.
#' @export
#'
#' @references
#'
#' Thierry Denoeux. Reasoning with fuzzy and uncertain evidence using epistemic random
#' fuzzy sets: general framework and practical models. Fuzzy Sets and Systems, Vol. 453,
#' Pages 1-36, 2023.
#'
#'@seealso \code{\link{Bel}}, \code{\link{Pl}}, \code{\link{pl_contour}}
#'
#' @examples
#' GRFN1<-list(mu=1,sig=1,h=2)
#' GRFN2<-list(mu=2,sig=2,h=3)
#' GRFN12s<-combination_GRFN(GRFN1,GRFN2) # soft normalization
#' GRFN12h<-combination_GRFN(GRFN1,GRFN2,soft=FALSE) # hard normalization
#' print(GRFN12s)
#' print(GRFN12h)
combination_GRFN<- function(GRFN1,GRFN2,soft=TRUE){
  if(soft){
    A<-1/GRFN1$h+1/GRFN2$h
    rho<-1/sqrt((A/GRFN1$sig^2+1)*(A/GRFN2$sig^2+1))
    S1<-(1/GRFN1$sig^2+1/A)^(-1/2)/sqrt(1-rho^2)
    S2<-(1/GRFN2$sig^2+1/A)^(-1/2)/sqrt(1-rho^2)
    M1<-GRFN1$mu*S1^2/GRFN1$sig^2 + rho*S1*S2/GRFN2$sig^2*GRFN2$mu
    M2<-GRFN2$mu*S2^2/GRFN2$sig^2 + rho*S1*S2/GRFN1$sig^2*GRFN1$mu
    SIG<-matrix(c(S1^2,rho*S1*S2,rho*S1*S2,S2^2),2,2)

  } else {
    M1<-GRFN1$mu
    M2<-GRFN2$mu
    SIG<-diag(c(GRFN1$sig^2,GRFN2$sig^2))
  }
  u<-c(GRFN1$h,GRFN2$h)/(GRFN1$h+GRFN2$h)
  M<-as.double(u%*%c(M1,M2))
  S<-as.double(sqrt(u%*%SIG%*%u))
  return(list(mu=M,sig=S,h=GRFN1$h+GRFN2$h))
}


