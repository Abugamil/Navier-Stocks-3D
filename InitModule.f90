!-------------------------------------!
! Setting boundary condition for UV   !
!-------------------------------------!
subroutine InitFields
   use variables
   use constants
   
   do j=1,MaxN                       ! Boundary On ZOY -> Left and Right
      do k=1,MaxO
          aUx1(j,k)=0.0
          bUx1(j,k)=0.0
          aUx2(j,k)=0.0
          bUx2(j,k)=0.0
      
          aVx1(j,k)=0.0
          bVx1(j,k)=0.0
          aVx2(j,k)=1.0
          bVx2(j,k)=0.0
          
          aWx1(j,k)=0.0
          bWx1(j,k)=0.0
          aWx2(j,k)=1.0
          bWx2(j,k)=0.0
      enddo      
   enddo
   do j=5,MaxN-4                       ! Boundary On ZOY -> Left and Right
      do k=5,MaxO-4
          bUx1(j,k)=1.0
      enddo
   enddo
      
   do i=1,MaxM                      ! Boundary On ZOX -> Top and Bottom
      do k=1,MaxO
          aUy1(i,k)=0.0
          bUy1(i,k)=0.0
          aUy2(i,k)=0.0
          bUy2(i,k)=0.0
            
          aVy1(i,k)=0.0
          bVy1(i,k)=0.0
          aVy2(i,k)=0.0
          bVy2(i,k)=0.0
          
          aWy1(i,k)=0.0
          bWy1(i,k)=0.0
          aWy2(i,k)=0.0
          bWy2(i,k)=0.0
      enddo
   enddo
   
   do i=1,MaxM                      ! Boundary On XOY -> Front and Back
      do j=1,MaxN
          aUz1(i,j)=0.0
          bUz1(i,j)=0.0
          aUz2(i,j)=0.0
          bUz2(i,j)=0.0
            
          aVz1(i,j)=0.0
          bVz1(i,j)=0.0
          aVz2(i,j)=0.0
          bVz2(i,j)=0.0
          
          aWz1(i,j)=0.0
          bWz1(i,j)=0.0
          aWz2(i,j)=0.0
          bWz2(i,j)=0.0
      enddo
   enddo
   
!      bUx1(1)=0.0
!      bUx1(MaxN)=0.0
   
   
   do i=0,MaxM+1
       do j=0,MaxN+1
          do k=0,MaxO+1
           u(i,j,k)=0.0
           v(i,j,k)=0.0
           w(i,j,k)=0.0
           p(i,j,k)=0.0
           Uo13(i,j,k)=0.0
           Uo23(i,j,k)=0.0
           Uon(i,j,k)=0.0
           Vo13(i,j,k)=0.0
           Vo23(i,j,k)=0.0
           Von(i,j,k)=0.0
           Wo13(i,j,k)=0.0
           Wo23(i,j,k)=0.0
           Won(i,j,k)=0.0
          enddo
       enddo
   enddo
 !  forall (j=1:MaxN) u(0,j)=1.0
end subroutine InitFields
subroutine InitPressureA
   use variables
   use constants
 j=0
do i=1,n
   j=j+1 
   ia(i)=j
   CD=-6.d0
   if (i.le.(pn*po)) CD=CD+1.d0                                              ! PO X -> Pm
   if (i.gt.(n-pn*po)) CD=CD+1.d0                                            ! PO X -> 0
   if (Mod(i,po).eq.0.0) CD=CD+1.d0                                          ! PO Z -> Pn
   if (Mod(i-1,po).eq.0.0) CD=CD+1.d0                                        ! PO Z -> 0
   if ((Mod(i,po*pn).gt.((pn-1)*po)).or.(Mod(i,po*pn).eq.0)) CD=CD+1.d0      ! PO Y -> Po
   if ((Mod(i,po*pn).le.po).and.(Mod(i,pn*po).ne.0)) CD=CD+1.d0              ! Po Z -> 0
   ap(j)=CD
   ja(j)=i
   if (Mod(i,po).gt.0) then
       j=j+1
       ap(j)=1.d0
       ja(j)=i+1
   endif
   if ((Mod(i,po*pn).le.((pn-1)*po)).and.(Mod(i,po*pn).ne.0)) then
       j=j+1
       ap(j)=1.d0
       ja(j)=i+po
   endif
   if (i.le.(n-pn*po)) then
       j=j+1
       ap(j)=1.d0
       ja(j)=i+pn*po
   endif
enddo
ia(n+1)=j+1

end subroutine InitPressureA

subroutine initObstacle
use variables
use constants
integer :: shX,shY,shZ
integer :: i,j
do i=1,MaxM
    do j=1,MaxN
        do k=1,MaxO
            Obstacle(i,j,k)=0.0
        enddo
    enddo
enddo

if (ObsType.eq.ObsSquare) then
    shX=INT(MaxM/4.0)
    shY=INT(MaxN/2.0)-INT(ObsSize/2.0)
    shZ=INT(MaxO/2.0)-INT(ObsSize/2.0)
    do i=shX,(shX+ObsSize)
        do j=shY,(shY+ObsSize)
           do k=shZ,(shZ+ObsSize)
            Obstacle(i,j,k)=1.0
           enddo
        enddo
    enddo
endif
end subroutine initObstacle
!-------------------------------------!
! Set Variables                       !
!-------------------------------------!
subroutine InitiliseVariables
   use variables
   use constants
   
   allocate(P(0:MaxM+1,0:MaxN+1,0:MaxO+1),STAT=istat)   
   if (istat.ne.0) write(*,*) "**** Not enough memory to allocate P ****" 
   allocate(Obstacle(MaxM,MaxN,MaxO),STAT=istat)   
   if (istat.ne.0) write(*,*) "**** Not enough memory to allocate Obstacle ****"  
   
   
   allocate(aUx1(MaxN,MaxO),STAT=istat) !PO X
   allocate(bUx1(MaxN,MaxO),STAT=istat)
   allocate(aUx2(MaxN,MaxO),STAT=istat)
   allocate(bUx2(MaxN,MaxO),STAT=istat)
   
   allocate(aVx1(MaxN,MaxO),STAT=istat)
   allocate(bVx1(MaxN,MaxO),STAT=istat)
   allocate(aVx2(MaxN,MaxO),STAT=istat)
   allocate(bVx2(MaxN,MaxO),STAT=istat)
   
   allocate(aWx1(MaxN,MaxO),STAT=istat)
   allocate(bWx1(MaxN,MaxO),STAT=istat)
   allocate(aWx2(MaxN,MaxO),STAT=istat)
   allocate(bWx2(MaxN,MaxO),STAT=istat)
   
   allocate(aUy1(MaxM,MaxO),STAT=istat) !PO Y
   allocate(bUy1(MaxM,MaxO),STAT=istat)
   allocate(aUy2(MaxM,MaxO),STAT=istat)
   allocate(bUy2(MaxM,MaxO),STAT=istat)
   
   allocate(aVy1(MaxM,MaxO),STAT=istat)
   allocate(bVy1(MaxM,MaxO),STAT=istat)
   allocate(aVy2(MaxM,MaxO),STAT=istat)
   allocate(bVy2(MaxM,MaxO),STAT=istat)
   
   allocate(aWy1(MaxM,MaxO),STAT=istat)
   allocate(bWy1(MaxM,MaxO),STAT=istat)
   allocate(aWy2(MaxM,MaxO),STAT=istat)
   allocate(bWy2(MaxM,MaxO),STAT=istat)
   
   allocate(aUz1(MaxM,MaxN),STAT=istat) !PO Z
   allocate(bUz1(MaxM,MaxN),STAT=istat)
   allocate(aUz2(MaxM,MaxN),STAT=istat)
   allocate(bUz2(MaxM,MaxN),STAT=istat)
   
   allocate(aVz1(MaxM,MaxN),STAT=istat)
   allocate(bVz1(MaxM,MaxN),STAT=istat)
   allocate(aVz2(MaxM,MaxN),STAT=istat)
   allocate(bVz2(MaxM,MaxN),STAT=istat)
   
   allocate(aWz1(MaxM,MaxN),STAT=istat)
   allocate(bWz1(MaxM,MaxN),STAT=istat)
   allocate(aWz2(MaxM,MaxN),STAT=istat)
   allocate(bWz2(MaxM,MaxN),STAT=istat)

   
   
   allocate(U(0:MaxM+1,0:MaxN+1,0:MaxO+1),STAT=istat)   
   if (istat.ne.0) write(*,*) "**** Not enough memory to allocate U ****"
   allocate(V(0:MaxM+1,0:MaxN+1,0:MaxO+1),STAT=istat)   
   if (istat.ne.0) write(*,*) "**** Not enough memory to allocate V ****"
   allocate(W(0:MaxM+1,0:MaxN+1,0:MaxO+1),STAT=istat)   
   if (istat.ne.0) write(*,*) "**** Not enough memory to allocate W ****"

   allocate(Uon(0:MaxM+1,0:MaxN+1,0:MaxO+1),STAT=istat)   
   if (istat.ne.0) write(*,*) "**** Not enough memory to allocate Uon ****"
   allocate(Von(0:MaxM+1,0:MaxN+1,0:MaxO+1),STAT=istat)   
   if (istat.ne.0) write(*,*) "**** Not enough memory to allocate Von ****"
   allocate(Won(0:MaxM+1,0:MaxN+1,0:MaxO+1),STAT=istat)   
   if (istat.ne.0) write(*,*) "**** Not enough memory to allocate Won ****"

   allocate(Uo13(0:MaxM+1,0:MaxN+1,0:MaxO+1),STAT=istat)   
   if (istat.ne.0) write(*,*) "**** Not enough memory to allocate Uo13 ****"
   allocate(Vo13(0:MaxM+1,0:MaxN+1,0:MaxO+1),STAT=istat)   
   if (istat.ne.0) write(*,*) "**** Not enough memory to allocate Vo13 ****"
   allocate(Wo13(0:MaxM+1,0:MaxN+1,0:MaxO+1),STAT=istat)   
   if (istat.ne.0) write(*,*) "**** Not enough memory to allocate Wo13 ****"
   
   allocate(Uo23(0:MaxM+1,0:MaxN+1,0:MaxO+1),STAT=istat)   
   if (istat.ne.0) write(*,*) "**** Not enough memory to allocate Uo23 ****"
   allocate(Vo23(0:MaxM+1,0:MaxN+1,0:MaxO+1),STAT=istat)   
   if (istat.ne.0) write(*,*) "**** Not enough memory to allocate Vo23 ****"
   allocate(Wo23(0:MaxM+1,0:MaxN+1,0:MaxO+1),STAT=istat)   
   if (istat.ne.0) write(*,*) "**** Not enough memory to allocate Wo23 ****"
   
   ALLOCATE( ia ( n + 1 ),STAT=istat)
   if (istat.ne.0) write(*,*) "**** Not enough memory to allocate ia ****"
   ALLOCATE( ja ( nnz ),STAT=istat )
   if (istat.ne.0) write(*,*) "**** Not enough memory to allocate ja ****"
   ALLOCATE( ap ( nnz ),STAT=istat )
   if (istat.ne.0) write(*,*) "**** Not enough memory to allocate ap ****"
   ALLOCATE( bp ( n ),STAT=istat )
   if (istat.ne.0) write(*,*) "**** Not enough memory to allocate bp ****"
   ALLOCATE( xp ( n ),STAT=istat )
   if (istat.ne.0) write(*,*) "**** Not enough memory to allocate xp ****"   
   ALLOCATE( iparm ( 64 ),STAT=istat )
   if (istat.ne.0) write(*,*) "**** Not enough memory to allocate iparam ****"

   
end subroutine InitiliseVariables

!-------------------------------------!
! Destroy Variables                   !
!-------------------------------------!
subroutine DestroyVariables
   use variables
   use constants
   
   IF ( ALLOCATED( ia ) )      DEALLOCATE( ia )
   IF ( ALLOCATED( ja ) )      DEALLOCATE( ja )
   IF ( ALLOCATED( ap ) )       DEALLOCATE( ap )
   IF ( ALLOCATED( bp ) )       DEALLOCATE( bp )
   IF ( ALLOCATED( xp ) )       DEALLOCATE( xp )
   IF ( ALLOCATED( iparm ) )   DEALLOCATE( iparm )

   deallocate(P,STAT=istat)   
   if (istat.ne.0) write(*,*) "**** Can't deallocate P ****"
   deallocate(Obstacle,STAT=istat)   
   if (istat.ne.0) write(*,*) "**** Can't deallocate Obstacle ****"
   
   deallocate(aUx1,STAT=istat)
   deallocate(bUx1,STAT=istat)
   deallocate(aUx2,STAT=istat)
   deallocate(bUx2,STAT=istat)
   
   deallocate(aVx1,STAT=istat)
   deallocate(bVx1,STAT=istat)
   deallocate(aVx2,STAT=istat)
   deallocate(bVx2,STAT=istat)
   
   deallocate(aWx1,STAT=istat)
   deallocate(bWx1,STAT=istat)
   deallocate(aWx2,STAT=istat)
   deallocate(bWx2,STAT=istat)
   
   deallocate(aUy1,STAT=istat)
   deallocate(bUy1,STAT=istat)
   deallocate(aUy2,STAT=istat)
   deallocate(bUy2,STAT=istat)
      
   deallocate(aVy1,STAT=istat)
   deallocate(bVy1,STAT=istat)
   deallocate(aVy2,STAT=istat)
   deallocate(bVy2,STAT=istat)
   
   deallocate(aWy1,STAT=istat)
   deallocate(bWy1,STAT=istat)
   deallocate(aWy2,STAT=istat)
   deallocate(bWy2,STAT=istat)
   
   deallocate(aUz1,STAT=istat)
   deallocate(bUz1,STAT=istat)
   deallocate(aUz2,STAT=istat)
   deallocate(bUz2,STAT=istat)
      
   deallocate(aVz1,STAT=istat)
   deallocate(bVz1,STAT=istat)
   deallocate(aVz2,STAT=istat)
   deallocate(bVz2,STAT=istat)
   
   deallocate(aWz1,STAT=istat)
   deallocate(bWz1,STAT=istat)
   deallocate(aWz2,STAT=istat)
   deallocate(bWz2,STAT=istat)
   
   deallocate(U,STAT=istat)   
   if (istat.ne.0) write(*,*) "**** Can't deallocate U ****"
   deallocate(V,STAT=istat)   
   if (istat.ne.0) write(*,*) "**** Can't deallocate V ****"
   deallocate(W,STAT=istat)   
   if (istat.ne.0) write(*,*) "**** Can't deallocate W ****"
   
   deallocate(Uon,STAT=istat)   
   if (istat.ne.0) write(*,*) "**** Can't deallocate Uon1 ****"
   deallocate(Von,STAT=istat)   
   if (istat.ne.0) write(*,*) "**** Can't deallocate Von1 ****"
   deallocate(Won,STAT=istat)   
   if (istat.ne.0) write(*,*) "**** Can't deallocate Von1 ****"

   deallocate(Uo13,STAT=istat)   
   if (istat.ne.0) write(*,*) "**** Can't deallocate Uo13 ****"
   deallocate(Vo13,STAT=istat)   
   if (istat.ne.0) write(*,*) "**** Can't deallocate Vo13 ****"
   deallocate(Wo13,STAT=istat)   
   if (istat.ne.0) write(*,*) "**** Can't deallocate Wo13 ****"
   deallocate(Uo23,STAT=istat)   
   if (istat.ne.0) write(*,*) "**** Can't deallocate Uo23 ****"
   deallocate(Vo23,STAT=istat)   
   if (istat.ne.0) write(*,*) "**** Can't deallocate Vo23 ****"
   deallocate(Wo23,STAT=istat)   
   if (istat.ne.0) write(*,*) "**** Can't deallocate Wo23 ****"
   
end subroutine DestroyVariables

