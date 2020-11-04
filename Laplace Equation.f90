
PROGRAM LAPLACE_EQUATION
    IMPLICIT NONE
    INTEGER,PARAMETER :: M=101, N=101
    REAL(KIND=8)      :: U(N,M),U_N(N,M),X(N),Y(M)
    REAL(KIND=8)      :: DX,DY,ERROR
    INTEGER           ::I,J,K

    OPEN(UNIT=1,FILE='ISOTERM.PLT',STATUS='REPLACE')
    WRITE(1,*)"ZONE"," I= ",M,''," J=",N,' F=POINT'


    DX=1./(N-1)
    DY=1./(M-1)
 
    DO I=1,N
        X(I)=(I-1)*DX
    END DO

    DO J=1,M
        Y(J)=(J-1)*DY
    END DO

    DO J=1,M
        DO I=1,N
            U(I,J)=0.0
        END DO
    END DO
    
    DO J=1,M
        DO I=1,N
            U_N(I,J)=U(I,J)
        END DO
    END DO
   
    DO K=1,100000

        ERROR=0.
   
        DO J=1,M             
            U(N,J)=0.0
            U(1,J)=0.0
        END DO
   
        DO I=1,N
            U(I,1)=1.0 
            U(I,M)=0.0 
        END DO
   
        DO J=2,M-1
            DO I=2,N-1
                U(I,J)=(1./((.5/(DX**2.))+(.5/(DY**2.))))*(((U(I+1,J)+U(I-1,J))/(4.*(DX**2.)))+((U(I,J+1)+U(I,J-1))/(4.*(DY**2.))))
            END DO
        END DO
  
        DO J=1,M
            DO I=1,N
                ERROR=ABS(U_N(I,J)-U(I,J))+ERROR
            END DO
        END DO
        
        IF(MOD(K,500)==0)THEN
            PRINT*,'        ITRATION=',K, '      ERROR=',ERROR
        END IF
        
        DO J=1,M
            DO I=1,N
                U_N(I,J)=U(I,J)
            END DO
        END DO
        
        IF (ERROR .LT. 0.000001) EXIT
        
    END DO

    DO J=1,M
        DO I=1,N
            WRITE (1,'(18X,F10.4,3X,F10.4,2X,F10.3)')X(I),Y(J),U(I,J) 
        END DO
    END DO
    
    PAUSE

END PROGRAM LAPLACE_EQUATION

