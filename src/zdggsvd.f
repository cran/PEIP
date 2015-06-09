c     interface to Lapack's dggsvd
c     subroutine dggsvd 	( 	character  	JOBU,
c     		character  	JOBV,
c     		character  	JOBQ,
c     		integer  	M,
c     		integer  	N,
c     		integer  	P,
c     		integer  	K,
c     		integer  	L,
c     		double precision, dimension( lda, * )  	A,
c     		integer  	LDA,
c     		double precision, dimension( ldb, * )  	B,
c     		integer  	LDB,
c     		double precision, dimension( * )  	ALPHA,
c     		double precision, dimension( * )  	BETA,
c     		double precision, dimension( ldu, * )  	U,
c     		integer  	LDU,
c     		double precision, dimension( ldv, * )  	V,
c     		integer  	LDV,
c     		double precision, dimension( ldq, * )  	Q,
c     		integer  	LDQ,
c     		double precision, dimension( * )  	WORK,
c     		integer, dimension( * )  	IWORK,
c     		integer  	INFO 
c     	) 		
      subroutine zdggsvd(kJOBU, kJOBV, kJOBQ, M, N, P, K, L,
     *     A, LDA, B, LDB, ALPHA, BETA, U, LDU, V, LDV, Q,
     *     LDQ, WORK, IWORK, INFO)
      integer  kJOBU, kJOBV, kJOBQ
      integer  M, N, P, K, L
       double precision  A(LDA, *)
      integer      LDA
       double precision  B(LDB, *)
       integer       LDB
       double precision  ALPHA(*), BETA(*)
       double precision  U(LDU,*)
       integer LDU
       double precision V(LDV,*)
       integer LDV
       double precision Q(LDQ,*)
       integer LDQ
       double precision WORK(*)
       integer  IWORK(*), INFO

        character*2        cJOBU, cJOBV, cJOBQ
        character*1        JOBU, JOBV, JOBQ
c     if you change the cxxx values don't forget to adjust the R functions
        
        parameter(cJOBU='NU', cJOBV='NV', cJOBQ='NQ')
        
        JOBU = cJOBU(kJOBU:kJOBU)
        JOBV = cJOBV(kJOBV:kJOBV)
        JOBQ = cJOBQ(kJOBQ:kJOBQ)

        
        call dggsvd(JOBU, JOBV, JOBQ, M, N, P, K, L,
     *     A, LDA, B, LDB, ALPHA, BETA, U, LDU, V, LDV, Q,
     *     LDQ, WORK, IWORK, INFO)

        return
        end
