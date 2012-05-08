wGSVD <-
function(A,B)
{

    # A=U*E1*Q'
    # B=V*E2*Q'
    #for linux
    #lpath=R.home(component='lib')
    #for windows
    lpath=R.home(component='bin')
    lfile=list.files(lpath,pattern='lapack',full.names=TRUE)
     dyn.load(lfile)

        #is.loaded("dggsvd") # returns TRUE  

    z <- .Fortran("dggsvd",

        as.character('U'),
        as.character('V'),
        as.character('Q'),
        as.integer(nrow(A)),
        as.integer(ncol(A)),
        as.integer(nrow(B)),
        integer(1),
        integer(1),
        as.double(A),
        as.integer(nrow(A)),
        as.double(B),
        as.integer(nrow(B)),
        double(ncol(A)),
        double(ncol(A)),
        double(nrow(A)*nrow(A)),
        as.integer(nrow(A)),
        double(nrow(B)*nrow(B)),
        as.integer(nrow(B)),
        double(ncol(A)*ncol(A)),
        as.integer(ncol(A)),
        double(max(c(3*ncol(A),nrow(A),nrow(B)))+ncol(A)),
        integer(ncol(A)),
        integer(1),dup=FALSE)

    K=z[7][[1]]
    L=z[8][[1]]
    U=z[15][[1]]
    V=z[17][[1]]


    Q=z[19][[1]]
    ALPHA=z[13][[1]]

        BETA=z[14][[1]]

    R=matrix(z[9][[1]],ncol(A),nrow=nrow(A),byrow=FALSE)
    U=matrix(U,ncol=nrow(A),nrow=nrow(A),byrow=FALSE)
    V=matrix(V,ncol=nrow(B),nrow=nrow(B),byrow=FALSE)
      Q=matrix(Q,ncol=ncol(A),nrow=ncol(A),byrow=FALSE)

    D1=mat.or.vec(nrow(A),K+L)
    D2=mat.or.vec(nrow(B),K+L)

    oR=mat.or.vec((K+L),ncol(A))
    if(K > 0)
    {

        if(K==1)
    { D1[1:K,1:K] =rep(1,K)
    }
    else
    {
      diag(D1[1:K,1:K])=rep(1,K)
    }

         diag(D1[(K+1):(K+L),(K+1):(K+L)])=ALPHA[(K+1):(K+L)]
        diag(D2[1:L,(K+1):(K+L)])=BETA[(K+1):(K+L)]
               

    }

    if(K ==0)
    {

        diag(D1[(K+1):(K+L),(K+1):(K+L)])=ALPHA[(K+1):(K+L)]
        diag(D2[1:L,(K+1):(K+L)])=BETA[(K+1):(K+L)]


    }    

    Ci=ALPHA[(K+1):(K+L)]
    S=BETA[(K+1):(K+L)]
    oR[(1):(K+L),(ncol(A)-K-L+1):(ncol(A))]=R[(1):(K+L),(ncol(A)-K-L+1):(ncol(A))]


     X = t(oR  %*% t(Q))

  return(list(U=U,V=V, X=X, C=D1,S=D2) )
    ##  return(list(U=U,V=V,Q=Q,D1=D1,D2=D2,oR=oR,C=Ci,S=S,K=K,L=L,Z=z))    
}
