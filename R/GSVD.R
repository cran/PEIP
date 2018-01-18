GSVD <-
function(A,B)
{
    ##  the author thanks Berend Hasselman  and Lapack authors
    ## for help in preparing some of these wrappers
    ## some of the code here has been adapted from package
    ## geigen, by Hasselman

    if(!is.matrix(A)) stop("Argument A should be a matrix")
    if(!is.matrix(B)) stop("Argument B should be a matrix")
  
    
    # A=U*E1*Q'
    # B=V*E2*Q'
  

    dimA <- dim(A)
    dimB <- dim(B)
    if(dimA[1]==0) stop("Matrix A has zero rows/columns")
    if(dimB[1]==0) stop("Matrix B has zero rows/columns")
    
    if(!all(is.finite(A))) stop("Matrix A may not contain infinite/NaN/NA")
    if(!all(is.finite(B))) stop("Matrix B may not contain infinite/NaN/NA")
 z = geigen::gsvd(A, B)

### names(z)
        R <- geigen::gsvd.R(z)
        oR <- geigen::gsvd.oR(z)
        D1 <- geigen::gsvd.D1(z);
        D2 <- geigen::gsvd.D2(z)
### R;oR
### D1;D2


##### matlab X is (sort of) 
        X    =    t(oR %*% t(z$Q) )

###  z$U %*% D1 %*% t(X) 

        C = D1
        S = D2

        return(list(U = z$U, V = z$V, X = X, C = D1, S = D2))

     
}
