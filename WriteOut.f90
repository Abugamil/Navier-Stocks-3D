
   subroutine WriteVtkTxtF(TVar,Num)
   use variables
   use constants
   Character(LEN=15) :: FName
   Integer :: TVar
   Integer :: Num
   character :: buffer*80, str1*8, str2*8, str3*8   
   If (TVar.eq.VarU) then
    Write(FName,"(A,I5.5,A)") 'U',Num,'.vtk'
   elseIf (TVar.eq.VarV) then
    Write(FName,"(A,I5.5,A)") 'V',Num,'.vtk'    
   elseIf (TVar.eq.VarW) then
    Write(FName,"(A,I5.5,A)") 'W',Num,'.vtk'    
   elseif (TVar.eq.VarU13) then
    Write(FName,"(A,I5.5,A)") 'U13',Num,'.vtk'
   elseif (TVar.eq.VarV13) then
    Write(FName,"(A,I5.5,A)") 'V13',Num,'.vtk'
   elseif (TVar.eq.VarW13) then
    Write(FName,"(A,I5.5,A)") 'W13',Num,'.vtk'
   elseif (TVar.eq.VarU23) then
    Write(FName,"(A,I5.5,A)") 'U23',Num,'.vtk'
   elseif (TVar.eq.VarV23) then
    Write(FName,"(A,I5.5,A)") 'V23',Num,'.vtk'
   elseif (TVar.eq.VarW23) then
    Write(FName,"(A,I5.5,A)") 'W23',Num,'.vtk'
   endif
    
   open(TVar,file=trim(adjustl(FName)))
   buffer = '# vtk DataFile Version 3.0'                                ; write(TVar,"(A)") trim(buffer)
   buffer = 'vtk output'                                                ; write(TVar,"(A)") trim(buffer)
   buffer = 'ASCII'                                                     ; write(TVar,"(A)") trim(buffer)
   buffer = 'DATASET STRUCTURED_POINTS'                                 ; write(TVar,"(A)") trim(buffer)
   write(str1(1:8),'(i8)') MaxM 
   write(str2(1:8),'(i8)') MaxN
   write(str3(1:8),'(i8)') MaxO
   buffer = 'DIMENSIONS '//str1//' '//str2//' '//str3                   ; write(TVar,"(A)") trim(buffer)
   write(str1(1:8),'(i8)') 1
   buffer = 'SPACING '//str1//' '//str1//' '//str1                      ; write(TVar,"(A)") trim(buffer)
   write(str1(1:8),'(i8)') 0
   buffer = 'ORIGIN '//str1//' '//str1//' '//str1                       ; write(TVar,"(A)") trim(buffer)
   write(str1(1:8),'(i8)') MaxM*MaxN*MaxO
   buffer = 'POINT_DATA '//str1                                         ; write(TVar,"(A)") trim(buffer)
   buffer = 'SCALARS velocity double'                                   ; write(TVar,"(A)") trim(buffer)
   buffer = 'LOOKUP_TABLE default'                                      ; write(TVar,"(A)") trim(buffer)
   do k=1,MaxO
    do j=1,MaxN
     do i=1,MaxM
       if (TVar.eq.VarU) then
          write(TVar,*) u(i,j,k)
       elseif (TVar.eq.VarV) then
          write(TVar,*) v(i,j,k)
       elseif (TVar.eq.VarW) then
          write(TVar,*) w(i,j,k)          
       elseif (TVar.eq.VarU13) then
          write(TVar,*) Uo13(i,j,k)
       elseif (TVar.eq.VarV13) then
          write(TVar,*) Vo13(i,j,k)
       elseif (TVar.eq.VarW13) then
          write(TVar,*) Wo13(i,j,k)
       elseif (TVar.eq.VarU23) then
          write(TVar,*) Uo23(i,j,k)
       elseif (TVar.eq.VarV23) then
          write(TVar,*) Vo23(i,j,k)
       elseif (TVar.eq.VarW23) then
          write(TVar,*) Wo23(i,j,k)
       endif
     enddo
    enddo
   enddo
   close(TVar)  
end subroutine WriteVtkTxtF
subroutine WriteVtkTxtUVW(Num)
   use variables
   use constants
   Character(LEN=15) :: FName
   Integer :: TVar=10
   Integer :: Num
   character :: buffer*80, str1*8, str2*8, str3*8
   Write(FName,"(A,I5.5,A)") 'Out',Num,'.vtk'
    
   open(TVar,file=trim(adjustl(FName)))
   buffer = '# vtk DataFile Version 3.0'                                ; write(TVar,"(A)") trim(buffer)
   buffer = 'vtk output'                                                ; write(TVar,"(A)") trim(buffer)
   buffer = 'ASCII'                                                     ; write(TVar,"(A)") trim(buffer)
   buffer = 'DATASET STRUCTURED_POINTS'                                 ; write(TVar,"(A)") trim(buffer)
   write(str1(1:8),'(i8)') MaxM 
   write(str2(1:8),'(i8)') MaxN
   write(str3(1:8),'(i8)') MaxO
   buffer = 'DIMENSIONS '//str1//' '//str2//' '//str3                   ; write(TVar,"(A)") trim(buffer)
   write(str1(1:8),'(i8)') 1
   buffer = 'SPACING '//str1//' '//str1//' '//str1                      ; write(TVar,"(A)") trim(buffer)
   write(str1(1:8),'(i8)') 0
   buffer = 'ORIGIN '//str1//' '//str1//' '//str1                       ; write(TVar,"(A)") trim(buffer)
   write(str1(1:8),'(i8)') MaxM*MaxN*MaxO
   buffer = 'POINT_DATA '//str1                                         ; write(TVar,"(A)") trim(buffer)
   buffer = 'SCALARS U double'                                          ; write(TVar,"(A)") trim(buffer)
   buffer = 'LOOKUP_TABLE default'                                      ; write(TVar,"(A)") trim(buffer)
   do k=1,MaxO
       do j=1,MaxN
           do i=1,MaxM
               write(TVar,*) u(i,j,k)
           enddo
       enddo
   enddo
   buffer = 'SCALARS V double'                                          ; write(TVar,"(A)") trim(buffer)
   buffer = 'LOOKUP_TABLE default'                                      ; write(TVar,"(A)") trim(buffer)
   do k=1,MaxO
       do j=1,MaxN
           do i=1,MaxM
               write(TVar,*) v(i,j,k)
           enddo
       enddo
   enddo
   buffer = 'SCALARS W double'                                          ; write(TVar,"(A)") trim(buffer)
   buffer = 'LOOKUP_TABLE default'                                      ; write(TVar,"(A)") trim(buffer)        
   do k=1,MaxO
       do j=1,MaxN
           do i=1,MaxM
               write(TVar,*) w(i,j,k)
           enddo
       enddo
   enddo
   buffer = 'SCALARS E double'                                          ; write(TVar,"(A)") trim(buffer)
   buffer = 'LOOKUP_TABLE default'                                      ; write(TVar,"(A)") trim(buffer)        
   do k=1,MaxO
       do j=1,MaxN
           do i=1,MaxM
               write(TVar,*) 0.5*(u(i,j,k)*u(i,j,k)+v(i,j,k)*v(i,j,k)+w(i,j,k)*w(i,j,k))
           enddo
       enddo
   enddo
   close(TVar)  
end subroutine WriteVtkTxtUVW

subroutine WriteVtkBinUVW(Num)
   use variables
   use constants
   Character(LEN=15) :: FName
   Integer :: ivtk=11
   Integer :: Num
   character :: buffer*80, lf*1, str1*8, str2*8, str3*8
   lf = char(10)
   Write(FName,"(A,I5.5,A)") 'Out',Num,'.vtk'
    
   open(unit=ivtk,file=trim(adjustl(FName)),form='binary',convert='BIG_ENDIAN')
   buffer = '# vtk DataFile Version 3.0'//lf                                ; write(ivtk) trim(buffer)
   buffer = 'vtk output'//lf                                                ; write(ivtk) trim(buffer)
   buffer = 'BINARY'//lf                                                    ; write(ivtk) trim(buffer)
   buffer = 'DATASET STRUCTURED_POINTS'//lf                                 ; write(ivtk) trim(buffer)
   write(str1(1:8),'(i8)') MaxM 
   write(str2(1:8),'(i8)') MaxN
   write(str3(1:8),'(i8)') MaxO
   buffer = 'DIMENSIONS '//str1//' '//str2//' '//str3//lf                   ; write(ivtk) trim(buffer)
   write(str1(1:8),'(i8)') 1
   buffer = 'SPACING '//str1//' '//str1//' '//str1//lf                      ; write(ivtk) trim(buffer)
   write(str1(1:8),'(i8)') 0
   buffer = 'ORIGIN '//str1//' '//str1//' '//str1//lf                       ; write(ivtk) trim(buffer)
   write(str1(1:8),'(i8)') MaxM*MaxN*MaxO
   buffer = 'POINT_DATA '//str1//lf                                         ; write(ivtk) trim(buffer)
   buffer = 'SCALARS U double'//lf                                          ; write(ivtk) trim(buffer)
   buffer = 'LOOKUP_TABLE default'//lf                                      ; write(ivtk) trim(buffer)
   do k=1,MaxO
       do j=1,MaxN
           do i=1,MaxM
               write(ivtk) u(i,j,k)
           enddo
       enddo
   enddo
   buffer = lf//lf//'SCALARS V double'//lf                                   ; write(ivtk) trim(buffer)
   buffer = 'LOOKUP_TABLE default'//lf                                       ; write(ivtk) trim(buffer)
   do k=1,MaxO
       do j=1,MaxN
           do i=1,MaxM
               write(ivtk) v(i,j,k)
           enddo
       enddo
   enddo
   buffer = lf//lf//'SCALARS W double'//lf                                   ; write(ivtk) trim(buffer)
   buffer = 'LOOKUP_TABLE default'//lf                                       ; write(ivtk) trim(buffer)
   do k=1,MaxO
       do j=1,MaxN
           do i=1,MaxM
               write(ivtk) w(i,j,k)
           enddo
       enddo
   enddo
   buffer = lf//lf//'SCALARS E double'//lf                                   ; write(ivtk) trim(buffer)
   buffer = 'LOOKUP_TABLE default'//lf                                       ; write(ivtk) trim(buffer)        
   do k=1,MaxO
       do j=1,MaxN
           do i=1,MaxM
               write(ivtk) 0.5*(u(i,j,k)*u(i,j,k)+v(i,j,k)*v(i,j,k)+w(i,j,k)*w(i,j,k))
           enddo
       enddo
   enddo
   
   buffer = lf//lf//'VECTORS U double'//lf                                 ; write(ivtk) trim(buffer)
   do k=1,MaxO
       do j=1,MaxN
           do i=1,MaxM
               write(ivtk) u(i,j,k),v(i,j,k),w(i,j,k)
           enddo
       enddo
   enddo
   close(ivtk)  
end subroutine WriteVtkBinUVW


subroutine WriteVtkBinVect(Num)
   use variables
   use constants
   Character(LEN=15) :: FName
   Integer :: ivtk=12
   Integer :: Num
   character :: buffer*80, lf*1, str1*8, str2*8, str3*8
   lf = char(10)
   Write(FName,"(A,I5.5,A)") 'Vect',Num,'.vtk'
    
   open(unit=ivtk,file=trim(adjustl(FName)),form='binary',convert='BIG_ENDIAN')
   buffer = '# vtk DataFile Version 3.0'//lf                                ; write(ivtk) trim(buffer)
   buffer = 'vtk output'//lf                                                ; write(ivtk) trim(buffer)
   buffer = 'BINARY'//lf                                                    ; write(ivtk) trim(buffer)
   buffer = 'DATASET STRUCTURED_POINTS'//lf                                 ; write(ivtk) trim(buffer)
   write(str1(1:8),'(i8)') MaxM 
   write(str2(1:8),'(i8)') MaxN
   write(str3(1:8),'(i8)') MaxO
   buffer = 'DIMENSIONS '//str1//' '//str2//' '//str3//lf                   ; write(ivtk) trim(buffer)
   write(str1(1:8),'(i8)') 1
   buffer = 'SPACING '//str1//' '//str1//' '//str1//lf                      ; write(ivtk) trim(buffer)
   write(str1(1:8),'(i8)') 0
   buffer = 'ORIGIN '//str1//' '//str1//' '//str1//lf                       ; write(ivtk) trim(buffer)
   write(str1(1:8),'(i8)') MaxM*MaxN*MaxO
   buffer = 'POINT_DATA '//str1//lf                                         ; write(ivtk) trim(buffer)
   buffer = 'VECTORS U double'//lf                                          ; write(ivtk) trim(buffer)
!   buffer = 'LOOKUP_TABLE default'//lf                                      ; write(ivtk) trim(buffer)
   do k=1,MaxO
       do j=1,MaxN
           do i=1,MaxM
               write(ivtk) u(i,j,k),v(i,j,k),w(i,j,k)
           enddo
       enddo
   enddo
   close(ivtk)  
end subroutine WriteVtkBinVect

subroutine TecplotWrite(Num,Dir,Layer)
      use variables
      use constants
      INCLUDE 'tecio.f90'
      
      Integer :: Num
      Character(LEN=25) :: FName
      integer :: Dir
      integer :: layer
      

      character*1 NULLCHR
      Integer*4   Debug,NPts,NElm
      


      real (kind=4),Dimension(:,:),allocatable  :: Axis
      

      real (kind=8)    SolTime
      integer III,maxI,MaxJ,maxK
      Integer VIsDouble, FileType
      Integer ZoneType,StrandID,ParentZn,IsBlock
      Integer ICellMax,JCellMax,KCellMax,NFConns,FNMode,ShrConn
      POINTER   (NullPtr,Null)
      Integer Null(*)
      
   
      If (Dir.eq.ByXoY) then          
          Write(FName,"(A,I3.3,A,I5.5,A)") 'SXY',Layer,'T',Num,'.plt'  
          MaxI    = MaxM
          MaxJ    = MaxN
      elseif (Dir.eq.ByXoZ) then
          Write(FName,"(A,I3.3,A,I5.5,A)") 'SXZ',Layer,'T',Num,'.plt'
          MaxI    = MaxM
          MaxJ    = MaxO 
      elseif (Dir.eq.ByYoZ) then          
          Write(FName,"(A,I3.3,A,I5.5,A)") 'SYZ',Layer,'T',Num,'.plt'
          MaxI    = MaxN
          MaxJ    = MaxO 
      endif
      
      allocate(Axis(MaxI,MaxJ),STAT=istat)
      if (istat.ne.0) write(*,*) "**** Can't deallocate Axis matrix****"      
      
      NULLCHR = CHAR(0)
      NullPtr = 0
      Debug   = 0
      FileType = 0
      VIsDouble = 0
      MaxK    = 1        
      ZoneType = 0
      SolTime = 360.0
      StrandID = 0
      ParentZn = 0
      IsBlock = 1
      ICellMax = 0
      JCellMax = 0
      KCellMax = 0
      NFConns = 0
      FNMode = 0
      ShrConn = 0
!
!... Open the file and write the tecplot datafile 
!... header information.
!
      I = TecIni112('SIMPLE DATASET'//NULLCHR, &
                    'X Y U V'//NULLCHR, &
                    FNAME//NULLCHR, &
                    '.'//NULLCHR, &
                    FileType, &
                    Debug, &
                    VIsDouble)
      
!     write(*,*) "**** Init Pass Ok! ****"
!
!... Write the zone header information.
!
      I = TecZne112('Simple Zone'//NULLCHR, &
                    ZoneType, &
                    MaxI, &
                    MaxJ, &
                    MaxK, &
                    ICellMax, &
                    JCellMax, &
                    KCellMax, &
                    SolTime, &
                    StrandID, &
                    ParentZn, &
                    IsBlock, &
                    NFConns, &
                    FNMode, &
                    0, &
                    0, &
                    0, &
                    Null, &
                    Null, &
                    Null, &
                    ShrConn)
!     write(*,*) "**** Zone declaration Pass Ok! ****"      
!
!... Write out the field data.
!
      do I = 1,MaxI
        do J = 1,MaxJ
            Axis(I,J) = I        
        enddo      
      enddo


      III = MaxI*MaxJ
      I   = TecDat112(III,Axis,0)
      
!      write(*,*) "**** X pass ****"
      
      do I = 1,MaxI
        do J = 1,MaxJ
            Axis(I,J) = J
        enddo      
      enddo 
      
      III = MaxI*MaxJ
      I   = TecDat112(III,Axis,0)
!      write(*,*) "**** Y pass ****"

     if (Dir.eq.ByXoY)then
         
          do I = 1,MaxI
            do J = 1,MaxJ
                Axis(I,J) = U(i,j,Layer)
            enddo      
          enddo         
      
          III = MaxI*MaxJ
          I   = TecDat112(III,Axis,0)
      
          do I = 1,MaxI
            do J = 1,MaxJ
                Axis(I,J) = V(i,j,Layer)
            enddo      
          enddo   
          
          III = MaxI*MaxJ
          I   = TecDat112(III,Axis,0)         
           
     elseif (Dir.eq.ByXoZ)then
         
          do I = 1,MaxI
            do J = 1,MaxJ
                Axis(I,J) = U(i,Layer,j)
            enddo      
          enddo         
      
          III = MaxI*MaxJ
          I   = TecDat112(III,Axis,0)
      
          do I = 1,MaxI
            do J = 1,MaxJ
                Axis(I,J) = V(i,Layer,j)
            enddo      
          enddo   
          
          III = MaxI*MaxJ
          I   = TecDat112(III,Axis,0)
          
     elseif (Dir.eq.ByYoZ)then
         
          do I = 1,MaxI
            do J = 1,MaxJ
                Axis(I,J) = U(Layer,i,j)
            enddo      
          enddo         
      
          III = MaxI*MaxJ
          I   = TecDat112(III,Axis,0)
      
          do I = 1,MaxI
            do J = 1,MaxJ
                Axis(I,J) = V(Layer,i,j)
            enddo      
          enddo   
          
          III = MaxI*MaxJ
          I   = TecDat112(III,Axis,0)
          
         
      endif
      
        
      deallocate(Axis)
      I = TecEnd112()
      
End subroutine TecplotWrite
