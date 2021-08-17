! In Thomas algorithm 
! A-> i+1
! B-> i
! C-> i-1
    
!-----------------------------------------------!
!     A  df/dx ->  Thomas algorithm             !
!-----------------------------------------------!
function addx(i,j,k)
use variables
use constants
real (kind=TypeFloat) tempout
 if (Cscheme.eq.CentralDifference) then
     tempout=0.5/dx
 elseif (Cscheme.eq.ForwardDifference) then
     tempout=1.0/dx
 else
     tempout=0.0
 endif
 addx=tempout
return
end function addx

!-----------------------------------------------!
!     B  df/dx ->  Thomas algorithm             !
!-----------------------------------------------!
function bddx(i,j,k)
use variables
use constants
real (kind=TypeFloat) tempout
 if (Cscheme.eq.CentralDifference) then
     tempout=0.0
 elseif (Cscheme.eq.ForwardDifference) then
     tempout=-1.0/dx
 elseif (Cscheme.eq.BackwardDifference) then
     tempout=1.0/dx
 else
     tempout=0.0
 endif
 bddx=tempout
return
end function bddx


!-----------------------------------------------!
!     C  df/dx ->  Thomas algorithm             !
!-----------------------------------------------!
function cddx(i,j,k)
use variables
use constants
real (kind=TypeFloat) :: tempout
 if (Cscheme.eq.CentralDifference) then
     tempout=-0.5/dx
 elseif (Cscheme.eq.ForwardDifference) then
     tempout=0.0
 elseif (Cscheme.eq.BackwardDifference) then
     tempout=-1.0/dx
 else
     tempout=0.0
 endif
 cddx=tempout
return
end function cddx

!-----------------------------------------------!
!     A  df/dy ->  Thomas algorithm             !
!-----------------------------------------------!
function addy(i,j,k)
use variables
use constants
real (kind=TypeFloat) :: tempout
 if (Cscheme.eq.CentralDifference) then
     tempout=0.5/dy
 elseif (Cscheme.eq.ForwardDifference) then
     tempout=1.0/dy
 else
     tempout=0.0
 endif
 addy=tempout
return
end function addy

!-----------------------------------------------!
!     B  df/dy ->  Thomas algorithm             !
!-----------------------------------------------!
function bddy(i,j,k)
use variables
use constants
real (kind=TypeFloat) :: tempout
 if (Cscheme.eq.CentralDifference) then
     tempout=0.0
 elseif (Cscheme.eq.ForwardDifference) then
     tempout=-1.0/dy
 elseif (Cscheme.eq.BackwardDifference) then
     tempout=1.0/dy
 else
     tempout=0.0
 endif
 bddy=tempout
return
end function bddy


!-----------------------------------------------!
!     C  df/dy ->  Thomas algorithm             !
!-----------------------------------------------!
function cddy(i,j,k)
use variables
use constants
real (kind=TypeFloat) :: tempout
 if (Cscheme.eq.CentralDifference) then
     tempout=-0.5/dy
 elseif (Cscheme.eq.ForwardDifference) then
     tempout=0.0
 elseif (Cscheme.eq.BackwardDifference) then
     tempout=-1.0/dy
 else
     tempout=0.0
 endif
 cddy=tempout
return
end function cddy

!-----------------------------------------------!
!     A  df/dz ->  Thomas algorithm             !
!-----------------------------------------------!
function addz(i,j,k)
use variables
use constants
real (kind=TypeFloat) :: tempout
 if (Cscheme.eq.CentralDifference) then
     tempout=0.5/dz
 elseif (Cscheme.eq.ForwardDifference) then
     tempout=1.0/dz
 else
     tempout=0.0
 endif
 addz=tempout
return
end function addz

!-----------------------------------------------!
!     B  df/dz ->  Thomas algorithm             !
!-----------------------------------------------!
function bddz(i,j,k)
use variables
use constants
real (kind=TypeFloat) :: tempout
 if (Cscheme.eq.CentralDifference) then
     tempout=0.0
 elseif (Cscheme.eq.ForwardDifference) then
     tempout=-1.0/dz
 elseif (Cscheme.eq.BackwardDifference) then
     tempout=1.0/dz
 else
     tempout=0.0
 endif
 bddz=tempout
return
end function bddz


!-----------------------------------------------!
!     C  df/dz ->  Thomas algorithm             !
!-----------------------------------------------!
function cddz(i,j,k)
use variables
use constants
real (kind=TypeFloat) :: tempout
 if (Cscheme.eq.CentralDifference) then
     tempout=-0.5/dz
 elseif (Cscheme.eq.ForwardDifference) then
     tempout=0.0
 elseif (Cscheme.eq.BackwardDifference) then
     tempout=-1.0/dz
 else
     tempout=0.0
 endif
 cddz=tempout
return

end function cddz

!-----------------------------------------------!
!     A  d2f/dx2 ->  Thomas algorithm           !
!-----------------------------------------------!
function ad2dx2(i,j,k)
use variables
use constants
real (kind=TypeFloat) :: tempout
 tempout=1.0/(dx*dx)
 ad2dx2=tempout
return
end function ad2dx2

!-----------------------------------------------!
!     B  d2f/dx2 ->  Thomas algorithm           !
!-----------------------------------------------!
function bd2dx2(i,j,k)
use variables
use constants
real (kind=TypeFloat) :: tempout
 tempout=-2.0/(dx*dx)
 bd2dx2=tempout
return
end function bd2dx2

!-----------------------------------------------!
!    C  d2f/dx2 ->  Thomas algorithm            !
!-----------------------------------------------!
function cd2dx2(i,j,k)
use variables
use constants
real (kind=TypeFloat) :: tempout
 tempout=1.0/(dx*dx)
 cd2dx2=tempout
return
end function cd2dx2

!-----------------------------------------------!
!     A  d2f/dy2 ->  Thomas algorithm           !
!-----------------------------------------------!
function ad2dy2(i,j,k)
use variables
use constants
real (kind=TypeFloat) :: tempout
 tempout=1.0/(dy*dy)
 ad2dy2=tempout
return
end function ad2dy2

!-----------------------------------------------!
!     B  d2f/dy2 ->  Thomas algorithm           !
!-----------------------------------------------!
function bd2dy2(i,j,k)
use variables
use constants
real (kind=TypeFloat) :: tempout
 tempout=-2.0/(dy*dy)
 bd2dy2=tempout
return
end function bd2dy2

!-----------------------------------------------!
!    C  d2f/dy2 ->  Thomas algorithm            !
!-----------------------------------------------!
function cd2dy2(i,j,k)
use variables
use constants
real (kind=TypeFloat) :: tempout
 tempout=1.0/(dy*dy)
 cd2dy2=tempout
return
end function cd2dy2

!-----------------------------------------------!
!     A  d2f/dz2 ->  Thomas algorithm           !
!-----------------------------------------------!
function ad2dz2(i,j,k)
use variables
use constants
real (kind=TypeFloat) :: tempout
 tempout=1.0/(dz*dz)
 ad2dz2=tempout
return
end function ad2dz2

!-----------------------------------------------!
!     B  d2f/dz2 ->  Thomas algorithm           !
!-----------------------------------------------!
function bd2dz2(i,j,k)
use variables
use constants
real (kind=TypeFloat) :: tempout
 tempout=-2.0/(dz*dz)
 bd2dz2=tempout
return
end function bd2dz2

!-----------------------------------------------!
!    C  d2f/dz2 ->  Thomas algorithm            !
!-----------------------------------------------!
function cd2dz2(i,j,k)
use variables
use constants
real (kind=TypeFloat) :: tempout
 tempout=1.0/(dz*dz)
 cd2dz2=tempout
return
end function cd2dz2


!-----------------------------------------------!
!    A  dtau1/dx ->  Thomas algorithm            !
!-----------------------------------------------!
function adtau1dx(i,j,k)
use variables
use constants
 adtau1dx=-2*vt(11,iplus12,i,j,k)/(dx*dx)
return
end function adtau1dx

!-----------------------------------------------!
!    B  dtau1/dx ->  Thomas algorithm            !
!-----------------------------------------------!
function bdtau1dx(i,j,k)
use variables
use constants
 bdtau1dx=2*vt(11,iplus12,i,j,k)/(dx*dx)+2*vt(11,iminus12,i,j,k)/(dx*dx)
return
end function bdtau1dx

!-----------------------------------------------!
!    C  dtau1/dx ->  Thomas algorithm            !
!-----------------------------------------------!
function cdtau1dx(i,j,k)
use variables
use constants
 cdtau1dx=-2*vt(11,iminus12,i,j,k)/(dx*dx)
return
end function cdtau1dx

!-----------------------------------------------!
!    A  dtau2/dx ->  Thomas algorithm            !
!-----------------------------------------------!
function adtau2dx(i,j,k)
use variables
use constants
 adtau2dx=-vt(21,iplus12,i,j,k)/(dx*dx)
return
end function adtau2dx

!-----------------------------------------------!
!    B  dtau2/dx ->  Thomas algorithm            !
!-----------------------------------------------!
function bdtau2dx(i,j,k)
use variables
use constants
 bdtau2dx=vt(21,iplus12,i,j,k)/(dx*dx)+vt(21,iminus12,i,j,k)/(dx*dx)
return
end function bdtau2dx

!-----------------------------------------------!
!    C  dtau2/dx ->  Thomas algorithm            !
!-----------------------------------------------!
function cdtau2dx(i,j,k)
use variables
use constants
 cdtau2dx=-vt(21,iminus12,i,j,k)/(dx*dx)
return
end function cdtau2dx

!-----------------------------------------------!
!    A  dtau3/dx ->  Thomas algorithm            !
!-----------------------------------------------!
function adtau3dx(i,j,k)
use variables
use constants
 adtau3dx=-vt(31,iplus12,i,j,k)/(dx*dx)
return
end function adtau3dx

!-----------------------------------------------!
!    B  dtau3/dx ->  Thomas algorithm            !
!-----------------------------------------------!
function bdtau3dx(i,j,k)
use variables
use constants
 bdtau3dx=vt(31,iplus12,i,j,k)/(dx*dx)+vt(31,iminus12,i,j,k)/(dx*dx)
return
end function bdtau3dx

!-----------------------------------------------!
!    C  dtau3/dx ->  Thomas algorithm            !
!-----------------------------------------------!
function cdtau3dx(i,j,k)
use variables
use constants
 cdtau3dx=-vt(31,iminus12,i,j,k)/(dx*dx)
return
end function cdtau3dx

!-----------------------------------------------!
!    A  dtau1/dy ->  Thomas algorithm            !
!-----------------------------------------------!
function adtau1dy(i,j,k)
use variables
use constants
 adtau1dy=-vt(12,jplus12,i,j,k)/(dy*dy)
return
end function adtau1dy

!-----------------------------------------------!
!    B  dtau1/dy ->  Thomas algorithm            !
!-----------------------------------------------!
function bdtau1dy(i,j,k)
use variables
use constants
 bdtau1dy=vt(12,jplus12,i,j,k)/(dy*dy)+vt(12,jminus12,i,j,k)/(dy*dy)
return
end function bdtau1dy

!-----------------------------------------------!
!    C  dtau1/dy ->  Thomas algorithm            !
!-----------------------------------------------!
function cdtau1dy(i,j,k)
use variables
use constants
 cdtau1dy=-vt(12,jminus12,i,j,k)/(dy*dy)
return
end function cdtau1dy

!-----------------------------------------------!
!    A  dtau2/dy ->  Thomas algorithm            !
!-----------------------------------------------!
function adtau2dy(i,j,k)
use variables
use constants
 adtau2dy=-2*vt(22,jplus12,i,j,k)/(dy*dy)
return
end function adtau2dy

!-----------------------------------------------!
!    B  dtau2/dy ->  Thomas algorithm            !
!-----------------------------------------------!
function bdtau2dy(i,j,k)
use variables
use constants
 bdtau2dy=2*vt(22,jplus12,i,j,k)/(dy*dy)+2*vt(22,jminus12,i,j,k)/(dy*dy)
return
end function bdtau2dy

!-----------------------------------------------!
!    C  dtau2/dy ->  Thomas algorithm            !
!-----------------------------------------------!
function cdtau2dy(i,j,k)
use variables
use constants
 cdtau2dy=-2*vt(22,jminus12,i,j,k)/(dy*dy)
return
end function cdtau2dy

!-----------------------------------------------!
!    A  dtau3/dy ->  Thomas algorithm            !
!-----------------------------------------------!
function adtau3dy(i,j,k)
use variables
use constants
 adtau3dy=-vt(32,jplus12,i,j,k)/(dy*dy)
return
end function adtau3dy

!-----------------------------------------------!
!    B  dtau3/dy ->  Thomas algorithm            !
!-----------------------------------------------!
function bdtau3dy(i,j,k)
use variables
use constants
 bdtau3dy=vt(32,jplus12,i,j,k)/(dy*dy)+vt(32,jminus12,i,j,k)/(dy*dy)
return
end function bdtau3dy

!-----------------------------------------------!
!    C  dtau3/dy ->  Thomas algorithm            !
!-----------------------------------------------!
function cdtau3dy(i,j,k)
use variables
use constants
 cdtau3dy=-vt(32,jminus12,i,j,k)/(dy*dy)
return
end function cdtau3dy

!-----------------------------------------------!
!    A  dtau1/dz ->  Thomas algorithm            !
!-----------------------------------------------!
function adtau1dz(i,j,k)
use variables
use constants
 adtau1dz=-vt(13,kplus12,i,j,k)/(dz*dz)
return
end function adtau1dz

!-----------------------------------------------!
!    B  dtau1/dz ->  Thomas algorithm            !
!-----------------------------------------------!
function bdtau1dz(i,j,k)
use variables
use constants
 bdtau1dz=vt(13,kplus12,i,j,k)/(dz*dz)+vt(13,kminus12,i,j,k)/(dz*dz)
return
end function bdtau1dz

!-----------------------------------------------!
!    C  dtau1/dz ->  Thomas algorithm            !
!-----------------------------------------------!
function cdtau1dz(i,j,k)
use variables
use constants
 cdtau1dz=-vt(13,kminus12,i,j,k)/(dz*dz)
return
end function cdtau1dz

!-----------------------------------------------!
!    A  dtau2/dz ->  Thomas algorithm            !
!-----------------------------------------------!
function adtau2dz(i,j,k)
use variables
use constants
 adtau2dz=-vt(23,kplus12,i,j,k)/(dz*dz)
return
end function adtau2dz

!-----------------------------------------------!
!    B  dtau2/dz ->  Thomas algorithm            !
!-----------------------------------------------!
function bdtau2dz(i,j,k)
use variables
use constants
 bdtau2dz=vt(23,kplus12,i,j,k)/(dz*dz)+vt(23,kminus12,i,j,k)/(dz*dz)
return
end function bdtau2dz

!-----------------------------------------------!
!    C  dtau2/dz ->  Thomas algorithm            !
!-----------------------------------------------!
function cdtau2dz(i,j,k)
use variables
use constants
 cdtau2dz=-vt(23,kminus12,i,j,k)/(dz*dz)
return
end function cdtau2dz

!-----------------------------------------------!
!    A  dtau3/dz ->  Thomas algorithm            !
!-----------------------------------------------!
function adtau3dz(i,j,k)
use variables
use constants
 adtau3dz=-2*vt(33,kplus12,i,j,k)/(dz*dz)
return
end function adtau3dz

!-----------------------------------------------!
!    B  dtau3/dz ->  Thomas algorithm            !
!-----------------------------------------------!
function bdtau3dz(i,j,k)
use variables
use constants
 bdtau3dz=2*vt(33,kplus12,i,j,k)/(dz*dz)+2*vt(33,kminus12,i,j,k)/(dz*dz)
return
end function bdtau3dz

!-----------------------------------------------!
!    C  dtau3/dz ->  Thomas algorithm            !
!-----------------------------------------------!
function cdtau3dz(i,j,k)
use variables
use constants
 cdtau3dz=-2*vt(33,kminus12,i,j,k)/(dz*dz)
return
end function cdtau3dz

!-----------------------------------------------!
!    du/dx in i,j,k                             !
!-----------------------------------------------!
function dudx(i,j,k,sloi) 
   use variables
   use constants
   integer :: i
   integer :: j
   integer :: k   
   integer :: sloi
   real (kind=TypeFloat) :: tempout=0.0
   if ((i.eq.1).or.(i.eq.MaxM)) then
       temout=0.0
   elseif (sloi.eq.0) then
      if (Cscheme.eq.CentralDifference) then
          tempout=(u(i+1,j,k)-u(i-1,j,k))/(2.0*dx)
      elseif (Cscheme.eq.ForwardDifference) then
          tempout=(u(i+1,j,k)-u(i,j,k))/dx
      elseif (Cscheme.eq.BackwardDifference) then
         tempout=(u(i,j,k)-u(i-1,j,k))/dx
      else
         tempout=0.0
      endif
   elseif (sloi.eq.1) then
      if (Cscheme.eq.CentralDifference) then
          tempout=(Uo13(i+1,j,k)-Uo13(i-1,j,k))/(2.0*dx)
      elseif (Cscheme.eq.ForwardDifference) then
          tempout=(Uo13(i+1,j,k)-Uo13(i,j,k))/dx
      elseif (Cscheme.eq.BackwardDifference) then
          tempout=(Uo13(i,j,k)-Uo13(i-1,j,k))/dx
      else
          tempout=0.0
      endif
   elseif (sloi.eq.2) then
      if (Cscheme.eq.CentralDifference) then
          tempout=(Uo23(i+1,j,k)-Uo23(i-1,j,k))/(2.0*dx)
      elseif (Cscheme.eq.ForwardDifference) then
          tempout=(Uo23(i+1,j,k)-Uo23(i,j,k))/dx
      elseif (Cscheme.eq.BackwardDifference) then
          tempout=(Uo23(i,j,k)-Uo23(i-1,j,k))/dx
      else
          tempout=0.0
      endif
   endif
   dudx=tempout
   return
end function dudx

!-----------------------------------------------!
!    du/dy in i,j,k                             !
!-----------------------------------------------!
function dudy(i,j,k,sloi) 
   use variables
   use constants
   integer :: i
   integer :: j
   integer :: k
   integer :: sloi
   real (kind=TypeFloat) :: tempout=0.0
   if ((j.eq.1).or.(j.eq.MaxN)) then
       temout=0.0
   elseif (sloi.eq.0) then
      if (Cscheme.eq.CentralDifference) then
          tempout=(u(i,j+1,k)-u(i,j-1,k))/(2.0*dy)
      elseif (Cscheme.eq.ForwardDifference) then
          tempout=(u(i,j+1,k)-u(i,j,k))/dy
      elseif (Cscheme.eq.BackwardDifference) then
          tempout=(u(i,j,k)-u(i,j-1,k))/dy
      else
          tempout=0.0
      endif
   elseif (sloi.eq.1) then
      if (Cscheme.eq.CentralDifference) then
          tempout=(Uo13(i,j+1,k)-Uo13(i,j-1,k))/(2.0*dy)
      elseif (Cscheme.eq.ForwardDifference) then
          tempout=(Uo13(i,j+1,k)-Uo13(i,j,k))/dy
      elseif (Cscheme.eq.BackwardDifference) then
         tempout=(Uo13(i,j,k)-Uo13(i,j-1,k))/dy
      else
         tempout=0.0
      endif
   elseif (sloi.eq.2) then
      if (Cscheme.eq.CentralDifference) then
          tempout=(Uo23(i,j+1,k)-Uo23(i,j-1,k))/(2.0*dy)
      elseif (Cscheme.eq.ForwardDifference) then
          tempout=(Uo23(i,j+1,k)-Uo23(i,j,k))/dy
      elseif (Cscheme.eq.BackwardDifference) then
          tempout=(Uo23(i,j,k)-Uo23(i,j-1,k))/dy
      else
         tempout=0.0
      endif
   endif
   dudy=tempout
   return
end function dudy

!-----------------------------------------------!
!    du/dz in i,j,k                             !
!-----------------------------------------------!
function dudz(i,j,k,sloi) 
   use variables
   use constants
   integer :: i
   integer :: j
   integer :: k
   integer :: sloi
   real (kind=TypeFloat) :: tempout=0.0
   if ((k.eq.1).or.(k.eq.MaxO)) then
       temout=0.0
   elseif (sloi.eq.0) then
      if (Cscheme.eq.CentralDifference) then
          tempout=(u(i,j,k+1)-u(i,j,k-1))/(2.0*dz)
      elseif (Cscheme.eq.ForwardDifference) then
          tempout=(u(i,j,k+1)-u(i,j,k))/dz
      elseif (Cscheme.eq.BackwardDifference) then
          tempout=(u(i,j,k)-u(i,j,k-1))/dz
      else
          tempout=0.0
      endif
   elseif (sloi.eq.1) then
      if (Cscheme.eq.CentralDifference) then
          tempout=(Uo13(i,j,k+1)-Uo13(i,j,k-1))/(2.0*dy)
      elseif (Cscheme.eq.ForwardDifference) then
          tempout=(Uo13(i,j,k+1)-Uo13(i,j,k))/dy
      elseif (Cscheme.eq.BackwardDifference) then
         tempout=(Uo13(i,j,k)-Uo13(i,j,k-1))/dy
      else
         tempout=0.0
      endif
   elseif (sloi.eq.2) then
      if (Cscheme.eq.CentralDifference) then
          tempout=(Uo23(i,j,k+1)-Uo23(i,j,k-1))/(2.0*dy)
      elseif (Cscheme.eq.ForwardDifference) then
          tempout=(Uo23(i,j,k+1)-Uo23(i,j,k))/dy
      elseif (Cscheme.eq.BackwardDifference) then
         tempout=(Uo23(i,j,k)-Uo23(i,j,k-1))/dy
      else
         tempout=0.0
      endif
   endif
   dudz=tempout
   return
end function dudz


!-----------------------------------------------!
!    dv/dx in i,j,k                             !
!-----------------------------------------------!
function dvdx(i,j,k,sloi) 
   use variables
   use constants
   integer :: i
   integer :: j
   integer :: k
   integer :: sloi
   real (kind=TypeFloat) :: tempout=0.0
   if ((i.eq.1).or.(i.eq.MaxM)) then
       temout=0.0
   elseif (sloi.eq.0) then
      if (Cscheme.eq.CentralDifference) then
          tempout=(v(i+1,j,k)-v(i-1,j,k))/(2.0*dx)
      elseif (Cscheme.eq.ForwardDifference) then
          tempout=(v(i+1,j,k)-v(i,j,k))/dx
      elseif (Cscheme.eq.BackwardDifference) then
         tempout=(v(i,j,k)-v(i-1,j,k))/dx
      else
         tempout=0.0
      endif
   elseif (sloi.eq.1) then
      if (Cscheme.eq.CentralDifference) then
          tempout=(Vo13(i+1,j,k)-Vo13(i-1,j,k))/(2.0*dx)
      elseif (Cscheme.eq.ForwardDifference) then
          tempout=(Vo13(i+1,j,k)-Vo13(i,j,k))/dx
      elseif (Cscheme.eq.BackwardDifference) then
         tempout=(Vo13(i,j,k)-Vo13(i-1,j,k))/dx
      else
         tempout=0.0
      endif
   elseif (sloi.eq.1) then
      if (Cscheme.eq.CentralDifference) then
          tempout=(Vo23(i+1,j,k)-Vo23(i-1,j,k))/(2.0*dx)
      elseif (Cscheme.eq.ForwardDifference) then
          tempout=(Vo23(i+1,j,k)-Vo23(i,j,k))/dx
      elseif (Cscheme.eq.BackwardDifference) then
         tempout=(Vo23(i,j,k)-Vo23(i-1,j,k))/dx
      else
         tempout=0.0
      endif
   endif

   dvdx=tempout
   return
end function dvdx

!-----------------------------------------------!
!    dv/dy in i,j,k                             !
!-----------------------------------------------!
function dvdy(i,j,k,sloi) 
   use variables
   use constants
   integer :: i
   integer :: j
   integer :: k   
   integer :: sloi
   real (kind=TypeFloat) :: tempout=0.0
   if ((j.eq.1).or.(j.eq.MaxN)) then
       temout=0.0
   elseif (sloi.eq.0) then
      if (Cscheme.eq.CentralDifference) then
          tempout=(v(i,j+1,k)-v(i,j-1,k))/(2.0*dy)
      elseif (Cscheme.eq.ForwardDifference) then
          tempout=(v(i,j+1,k)-v(i,j,k))/dy
      elseif (Cscheme.eq.BackwardDifference) then
         tempout=(v(i,j,k)-v(i,j-1,k))/dy
      else
         tempout=0.0
      endif
   elseif (sloi.eq.1) then
      if (Cscheme.eq.CentralDifference) then
          tempout=(Vo13(i,j+1,k)-Vo13(i,j-1,k))/(2.0*dy)
      elseif (Cscheme.eq.ForwardDifference) then
          tempout=(Vo13(i,j+1,k)-Vo13(i,j,k))/dy
      elseif (Cscheme.eq.BackwardDifference) then
         tempout=(Vo13(i,j,k)-Vo13(i,j-1,k))/dy
      else
         tempout=0.0
      endif
   elseif (sloi.eq.2) then
      if (Cscheme.eq.CentralDifference) then
          tempout=(Vo23(i,j+1,k)-Vo23(i,j-1,k))/(2.0*dy)
      elseif (Cscheme.eq.ForwardDifference) then
          tempout=(Vo23(i,j+1,k)-Vo23(i,j,k))/dy
      elseif (Cscheme.eq.BackwardDifference) then
         tempout=(Vo23(i,j,k)-Vo23(i,j-1,k))/dy
      else
         tempout=0.0
      endif
   endif
   dvdy=tempout
   return
end function dvdy

!-----------------------------------------------!
!    dv/dz in i,j,k                             !
!-----------------------------------------------!
function dvdz(i,j,k,sloi) 
   use variables
   use constants
   integer :: i
   integer :: j
   integer :: k   
   integer :: sloi
   real (kind=TypeFloat) :: tempout=0.0
   if ((k.eq.1).or.(k.eq.MaxO)) then
       temout=0.0
   elseif (sloi.eq.0) then
      if (Cscheme.eq.CentralDifference) then
          tempout=(v(i,j,k+1)-v(i,j,k-1))/(2.0*dz)
      elseif (Cscheme.eq.ForwardDifference) then
          tempout=(v(i,j,k+1)-v(i,j,k))/dz
      elseif (Cscheme.eq.BackwardDifference) then
         tempout=(v(i,j,k)-v(i,j,k-1))/dz
      else
         tempout=0.0
      endif
   elseif (sloi.eq.1) then
      if (Cscheme.eq.CentralDifference) then
          tempout=(Vo13(i,j,k+1)-Vo13(i,j,k-1))/(2.0*dz)
      elseif (Cscheme.eq.ForwardDifference) then
          tempout=(Vo13(i,j,k+1)-Vo13(i,j,k-1))/dz
      elseif (Cscheme.eq.BackwardDifference) then
         tempout=(Vo13(i,j,k)-Vo13(i,j,k-1))/dz
      else
         tempout=0.0
      endif
   elseif (sloi.eq.2) then
      if (Cscheme.eq.CentralDifference) then
          tempout=(Vo23(i,j,k+1)-Vo23(i,j,k-1))/(2.0*dy)
      elseif (Cscheme.eq.ForwardDifference) then
          tempout=(Vo23(i,j,k+1)-Vo23(i,j,k))/dy
      elseif (Cscheme.eq.BackwardDifference) then
         tempout=(Vo23(i,j,k)-Vo23(i,j,k-1))/dy
      else
         tempout=0.0
      endif
   endif
   dvdz=tempout
   return
end function dvdz

!-----------------------------------------------!
!    dw/dx in i,j,k                             !
!-----------------------------------------------!
function dwdx(i,j,k,sloi) 
   use variables
   use constants
   integer :: i
   integer :: j
   integer :: k
   integer :: sloi
   real (kind=TypeFloat) :: tempout=0.0
   if ((i.eq.1).or.(i.eq.MaxM)) then
       temout=0.0
   elseif (sloi.eq.0) then
      if (Cscheme.eq.CentralDifference) then
          tempout=(w(i+1,j,k)-w(i-1,j,k))/(2.0*dx)
      elseif (Cscheme.eq.ForwardDifference) then
          tempout=(w(i+1,j,k)-w(i,j,k))/dx
      elseif (Cscheme.eq.BackwardDifference) then
         tempout=(w(i,j,k)-w(i-1,j,k))/dx
      else
         tempout=0.0
      endif
   elseif (sloi.eq.1) then
      if (Cscheme.eq.CentralDifference) then
          tempout=(Wo13(i+1,j,k)-Wo13(i-1,j,k))/(2.0*dx)
      elseif (Cscheme.eq.ForwardDifference) then
          tempout=(Wo13(i+1,j,k)-Wo13(i,j,k))/dx
      elseif (Cscheme.eq.BackwardDifference) then
         tempout=(Wo13(i,j,k)-Wo13(i-1,j,k))/dx
      else
         tempout=0.0
      endif
   elseif (sloi.eq.1) then
      if (Cscheme.eq.CentralDifference) then
          tempout=(Wo23(i+1,j,k)-Wo23(i-1,j,k))/(2.0*dx)
      elseif (Cscheme.eq.ForwardDifference) then
          tempout=(Wo23(i+1,j,k)-Wo23(i,j,k))/dx
      elseif (Cscheme.eq.BackwardDifference) then
         tempout=(Wo23(i,j,k)-Wo23(i-1,j,k))/dx
      else
         tempout=0.0
      endif
   endif

   dwdx=tempout
   return
end function dwdx

!-----------------------------------------------!
!    dw/dy in i,j,k                             !
!-----------------------------------------------!
function dwdy(i,j,k,sloi) 
   use variables
   use constants
   integer :: i
   integer :: j
   integer :: k   
   integer :: sloi
   real (kind=TypeFloat) :: tempout=0.0
   if ((j.eq.1).or.(j.eq.MaxN)) then
       temout=0.0
   elseif (sloi.eq.0) then
      if (Cscheme.eq.CentralDifference) then
          tempout=(w(i,j+1,k)-w(i,j-1,k))/(2.0*dy)
      elseif (Cscheme.eq.ForwardDifference) then
          tempout=(w(i,j+1,k)-w(i,j,k))/dy
      elseif (Cscheme.eq.BackwardDifference) then
         tempout=(w(i,j,k)-w(i,j-1,k))/dy
      else
         tempout=0.0
      endif
   elseif (sloi.eq.1) then
      if (Cscheme.eq.CentralDifference) then
          tempout=(Wo13(i,j+1,k)-Wo13(i,j-1,k))/(2.0*dy)
      elseif (Cscheme.eq.ForwardDifference) then
          tempout=(Wo13(i,j+1,k)-Wo13(i,j,k))/dy
      elseif (Cscheme.eq.BackwardDifference) then
         tempout=(Wo13(i,j,k)-Wo13(i,j-1,k))/dy
      else
         tempout=0.0
      endif
   elseif (sloi.eq.2) then
      if (Cscheme.eq.CentralDifference) then
          tempout=(Wo23(i,j+1,k)-Wo23(i,j-1,k))/(2.0*dy)
      elseif (Cscheme.eq.ForwardDifference) then
          tempout=(Wo23(i,j+1,k)-Wo23(i,j,k))/dy
      elseif (Cscheme.eq.BackwardDifference) then
         tempout=(Wo23(i,j,k)-Wo23(i,j-1,k))/dy
      else
         tempout=0.0
      endif
   endif
   dwdy=tempout
   return
end function dwdy

!-----------------------------------------------!
!    dv/dz in i,j,k                             !
!-----------------------------------------------!
function dwdz(i,j,k,sloi) 
   use variables
   use constants
   integer :: i
   integer :: j
   integer :: k   
   integer :: sloi
   real (kind=TypeFloat) :: tempout=0.0
   if ((j.eq.1).or.(j.eq.MaxO)) then
       temout=0.0
   elseif (sloi.eq.0) then
      if (Cscheme.eq.CentralDifference) then
          tempout=(w(i,j,k+1)-w(i,j,k-1))/(2.0*dz)
      elseif (Cscheme.eq.ForwardDifference) then
          tempout=(w(i,j,k+1)-w(i,j,k))/dz
      elseif (Cscheme.eq.BackwardDifference) then
         tempout=(w(i,j,k)-w(i,j,k-1))/dz
      else
         tempout=0.0
      endif
   elseif (sloi.eq.1) then
      if (Cscheme.eq.CentralDifference) then
          tempout=(Wo13(i,j,k+1)-Wo13(i,j,k-1))/(2.0*dz)
      elseif (Cscheme.eq.ForwardDifference) then
          tempout=(Wo13(i,j,k+1)-Wo13(i,j,k-1))/dz
      elseif (Cscheme.eq.BackwardDifference) then
         tempout=(Wo13(i,j,k)-Wo13(i,j,k-1))/dz
      else
         tempout=0.0
      endif
   elseif (sloi.eq.2) then
      if (Cscheme.eq.CentralDifference) then
          tempout=(Wo23(i,j,k+1)-Wo23(i,j,k-1))/(2.0*dy)
      elseif (Cscheme.eq.ForwardDifference) then
          tempout=(Wo23(i,j,k+1)-Wo23(i,j,k))/dy
      elseif (Cscheme.eq.BackwardDifference) then
         tempout=(Wo23(i,j,k)-Wo23(i,j,k-1))/dy
      else
         tempout=0.0
      endif
   endif
   dwdz=tempout
   return
end function dwdz


!-----------------------------------------------!
!    d2u/dx2 in i,j,k                           !
!-----------------------------------------------!
function d2udx2(i,j,k,sloi) 
   use variables
   use constants
   integer :: i
   integer :: j
   integer :: k
   integer :: sloi
   real (kind=TypeFloat) :: tempout=0.0
   if (sloi.eq.0) then
       tempout = (u(i+1,j,k)-2.0*u(i,j,k)+u(i-1,j,k))/(dx*dx)
   elseif (sloi.eq.1) then
       tempout = (Uo13(i+1,j,k)-2.0*Uo13(i,j,k)+Uo13(i-1,j,k))/(dx*dx)
   elseif (sloi.eq.2) then       
       tempout = (Uo23(i+1,j,k)-2.0*Uo23(i,j,k)+Uo23(i-1,j,k))/(dx*dx)
   endif    
   d2udx2 = tempout
   return
end function d2udx2

!-----------------------------------------------!
!    d2v/dx2 in i,j                             !
!-----------------------------------------------!
function d2vdx2(i,j,k,sloi) 
   use variables
   use constants
   integer :: i
   integer :: j
   integer :: k
   real (kind=TypeFloat) :: tempout=0.0
   integer :: sloi
   if (sloi.eq.0) then
       tempout = (v(i+1,j,k)-2.0*v(i,j,k)+v(i-1,j,k))/(dx*dx)
   elseif (sloi.eq.1) then
       tempout = (Vo13(i+1,j,k)-2.0*Vo13(i,j,k)+Vo13(i-1,j,k))/(dx*dx)
   elseif (sloi.eq.2) then
       tempout = (Vo23(i+1,j,k)-2.0*Vo23(i,j,k)+Vo23(i-1,j,k))/(dx*dx)
   endif    
   d2vdx2 = tempout
   return
end function d2vdx2

!-----------------------------------------------!
!    d2w/dx2 in i,j                             !
!-----------------------------------------------!
function d2wdx2(i,j,k,sloi) 
   use variables
   use constants
   integer :: i
   integer :: j
   integer :: k
   real (kind=TypeFloat) :: tempout=0.0
   integer :: sloi
   if (sloi.eq.0) then
       tempout = (w(i+1,j,k)-2.0*w(i,j,k)+w(i-1,j,k))/(dx*dx)
   elseif (sloi.eq.1) then
       tempout = (Wo13(i+1,j,k)-2.0*Wo13(i,j,k)+Wo13(i-1,j,k))/(dx*dx)
   elseif (sloi.eq.2) then
       tempout = (Wo23(i+1,j,k)-2.0*Wo23(i,j,k)+Wo23(i-1,j,k))/(dx*dx)
   endif    
   d2wdx2 = tempout
   return
end function d2wdx2

!-----------------------------------------------!
!    d2u/dy2 in i,j                             !
!-----------------------------------------------!
function d2udy2(i,j,k,sloi) 
   use variables
   use constants
   integer :: i
   integer :: j
   integer :: k
   real (kind=TypeFloat) :: tempout
   integer :: sloi
   if (sloi.eq.0) then
       tempout = (u(i,j+1,k)-2.0*u(i,j,k)+u(i,j-1,k))/(dy*dy)
   elseif (sloi.eq.1) then
       tempout = (Uo13(i,j+1,k)-2.0*Uo13(i,j,k)+Uo13(i,j-1,k))/(dy*dy)
   elseif (sloi.eq.2) then
       tempout = (Uo23(i,j+1,k)-2.0*Uo23(i,j,k)+Uo23(i,j-1,k))/(dy*dy)
   endif    
   d2udy2 = tempout
   return
end function d2udy2

!-----------------------------------------------!
!    d2v/dy2 in i,j                             !
!-----------------------------------------------!
function d2vdy2(i,j,k,sloi) 
   use variables
   use constants
   integer :: i
   integer :: j
   integer :: k
   real (kind=TypeFloat) :: tempout
   integer :: sloi
   if (sloi.eq.0) then
       tempout = (v(i,j+1,k)-2.0*v(i,j,k)+v(i,j-1,k))/(dy*dy)
   elseif (sloi.eq.1) then
       tempout = (Vo13(i,j+1,k)-2.0*Vo13(i,j,k)+Vo13(i,j-1,k))/(dy*dy)
   elseif (sloi.eq.2) then
       tempout = (Vo23(i,j+1,k)-2.0*Vo23(i,j,k)+Vo23(i,j-1,k))/(dy*dy)
   endif    
   d2vdy2 = tempout
   return
end function d2vdy2

!-----------------------------------------------!
!    d2w/dy2 in i,j,k                           !
!-----------------------------------------------!
function d2wdy2(i,j,k,sloi) 
   use variables
   use constants
   integer :: i
   integer :: j
   integer :: k
   real (kind=TypeFloat) :: tempout
   integer :: sloi
   if (sloi.eq.0) then
       tempout = (w(i,j+1,k)-2.0*w(i,j,k)+w(i,j-1,k))/(dy*dy)
   elseif (sloi.eq.1) then
       tempout = (Wo13(i,j+1,k)-2.0*Wo13(i,j,k)+Wo13(i,j-1,k))/(dy*dy)
   elseif (sloi.eq.2) then
       tempout = (Wo23(i,j+1,k)-2.0*Wo23(i,j,k)+Wo23(i,j-1,k))/(dy*dy)
   endif    
   d2wdy2 = tempout
   return
end function d2wdy2

!-----------------------------------------------!
!    d2u/dz2 in i,j                             !
!-----------------------------------------------!
function d2udz2(i,j,k,sloi) 
   use variables
   use constants
   integer :: i
   integer :: j
   integer :: k
   real (kind=TypeFloat) :: tempout
   integer :: sloi
   if (sloi.eq.0) then
       tempout = (u(i,j,k+1)-2.0*u(i,j,k)+u(i,j,k-1))/(dz*dz)
   elseif (sloi.eq.1) then
       tempout = (Uo13(i,j,k+1)-2.0*Uo13(i,j,k)+Uo13(i,j,k-1))/(dz*dz)
   elseif (sloi.eq.2) then
       tempout = (Uo23(i,j,k+1)-2.0*Uo23(i,j,k)+Uo23(i,j,k-1))/(dz*dz)
   endif    
   d2udz2 = tempout
   return
end function d2udz2

!-----------------------------------------------!
!    d2v/dy2 in i,j                             !
!-----------------------------------------------!
function d2vdz2(i,j,k,sloi) 
   use variables
   use constants
   integer :: i
   integer :: j
   integer :: k
   real (kind=TypeFloat) :: tempout
   integer :: sloi
   if (sloi.eq.0) then
       tempout = (v(i,j,k+1)-2.0*v(i,j,k)+v(i,j,k-1))/(dz*dz)
   elseif (sloi.eq.1) then
       tempout = (Vo13(i,j,k+1)-2.0*Vo13(i,j,k)+Vo13(i,j,k-1))/(dz*dz)
   elseif (sloi.eq.2) then
       tempout = (Vo23(i,j,k+1)-2.0*Vo23(i,j,k)+Vo23(i,j,k-1))/(dz*dz)
   endif    
   d2vdz2 = tempout
   return
end function d2vdz2

!-----------------------------------------------!
!    d2w/dy2 in i,j,k                           !
!-----------------------------------------------!
function d2wdz2(i,j,k,sloi) 
   use variables
   use constants
   integer :: i
   integer :: j
   integer :: k
   real (kind=TypeFloat) :: tempout
   integer :: sloi
   if (sloi.eq.0) then
       tempout = (w(i,j,k+1)-2.0*w(i,j,k)+w(i,j,k-1))/(dz*dz)
   elseif (sloi.eq.1) then
       tempout = (Wo13(i,j,k+1)-2.0*Wo13(i,j,k)+Wo13(i,j,k-1))/(dz*dz)
   elseif (sloi.eq.2) then
       tempout = (Wo23(i,j,k+1)-2.0*Wo23(i,j,k)+Wo23(i,j,k-1))/(dz*dz)
   endif    
   d2wdz2 = tempout
   return
end function d2wdz2

!-----------------------------------------------!
!    S11()  in i,j,k                             !
!-----------------------------------------------!
function S11(i,j,k) 
   use variables
   use constants
   integer :: i
   integer :: j
   integer :: k
   real (kind=TypeFloat) :: tempout
   If ((i.gt.MaxM).or.(i.lt.1)) then
       tempout=0.0
   else 
       tempout = dudx(i,j,k,0)
   endif
   S11 = tempout
   return
end function S11

!-----------------------------------------------!
!    S12()  in i,j,k                             !
!-----------------------------------------------!
function S12(i,j,k) 
   use variables
   use constants
   integer :: i
   integer :: j
   integer :: k
   real (kind=TypeFloat) :: tempout
   If ((j.gt.MaxN).or.(i.gt.MaxM).or.(i.lt.1).or.(j.lt.1)) then
       tempout=0.0
   else 
       tempout = 0.5*(dudy(i,j,k,0)+dvdx(i,j,k,0))
   endif   
   S12 = tempout
   return
end function S12

!-----------------------------------------------!
!    S13()  in i,j,k                             !
!-----------------------------------------------!
function S13(i,j,k) 
   use variables
   use constants
   integer :: i
   integer :: j
   integer :: k
   real (kind=TypeFloat) :: tempout
   If ((i.gt.MaxM).or.(k.gt.MaxO).or.(i.lt.1).or.(k.lt.1)) then
       tempout=0.0
   else 
       tempout = 0.5*(dudz(i,j,k,0)+dwdx(i,j,k,0))
   endif
   
   S13 = tempout
   return
end function S13

!-----------------------------------------------!
!    S21()  in i,j,k                             !
!-----------------------------------------------!
function S21(i,j,k) 
   use variables
   use constants
   integer :: i
   integer :: j
   integer :: k
   real (kind=TypeFloat) :: tempout
   If ((i.gt.MaxM).or.(j.gt.MaxN).or.(i.lt.1).or.(j.lt.1)) then
       tempout=0.0
   else
       tempout = 0.5*(dvdx(i,j,k,0)+dudy(i,j,k,0))
   endif
   
   S21 = tempout
   return
end function S21

!-----------------------------------------------!
!    S22()  in i,j,k                             !
!-----------------------------------------------!
function S22(i,j,k) 
   use variables
   use constants
   integer :: i
   integer :: j
   integer :: k
   real (kind=TypeFloat) :: tempout
   If ((j.gt.MaxN).or.(j.lt.1)) then
       tempout=0.0
   else 
       tempout = dvdy(i,j,k,0)
   endif
   
   S22 = tempout
   return
end function S22

!-----------------------------------------------!
!    S23()  in i,j,k                             !
!-----------------------------------------------!
function S23(i,j,k) 
   use variables
   use constants
   integer :: i
   integer :: j
   integer :: k
   real (kind=TypeFloat) :: tempout
   If ((j.gt.MaxN).or.(k.gt.MaxO).or.(j.lt.1).or.(k.lt.1)) then
       tempout=0.0
   else 
       tempout = 0.5*(dvdz(i,j,k,0)+dwdy(i,j,k,0))
   endif
   
   S23 = tempout
   return
end function S23

!-----------------------------------------------!
!    S31()  in i,j,k                             !
!-----------------------------------------------!
function S31(i,j,k) 
   use variables
   use constants
   integer :: i
   integer :: j
   integer :: k
   real (kind=TypeFloat) :: tempout
   If ((i.gt.MaxM).or.(k.gt.MaxO).or.(i.lt.1).or.(k.lt.1)) then
       tempout=0.0
   else 
       tempout = 0.5*(dwdx(i,j,k,0)+dudz(i,j,k,0))
   endif
   S31 = tempout
   return
end function S31

!-----------------------------------------------!
!    S32()  in i,j,k                             !
!-----------------------------------------------!
function S32(i,j,k) 
   use variables
   use constants
   integer :: i
   integer :: j
   integer :: k
   real (kind=TypeFloat) :: tempout
   If ((j.gt.MaxN).or.(k.gt.MaxO).or.(j.lt.1).or.(k.lt.1)) then
       tempout=0.0
   else 
       tempout = 0.5*(dwdy(i,j,k,0)+dvdz(i,j,k,0))
   endif
   
   S32 = tempout
   return
end function S32

!-----------------------------------------------!
!    S33()  in i,j,k                             !
!-----------------------------------------------!
function S33(i,j,k) 
   use variables
   use constants
   integer :: i
   integer :: j
   integer :: k
   real (kind=TypeFloat) :: tempout
   If ((k.gt.MaxO).or.(k.lt.0)) then
       tempout=0.0
   else 
       tempout = dwdz(i,j,k,0)
   endif
   
   S33 = tempout
   return
end function S33

!-----------------------------------------------!
!    vt()  in i,j,k                              !
!-----------------------------------------------!
function vt(tij,dvt,i,j,k) 
   use variables
   use constants
   integer :: i
   integer :: j
   integer :: k
   integer :: tij
   real (kind=TypeFloat) :: C,MS
   integer :: dvt
!  Assume dx=dy=dz
!  Then grid size delta=dx
   C=(Cs*dx)**2
   MS=-990000.0
   if (tij.eq.11) then
      if (dvt.eq.iplus12) MS=0.5*(abs(S11(i+1,j,k))+abs(S11(i,j,k)))
      if (dvt.eq.iminus12) MS=0.5*(abs(S11(i-1,j,k))+abs(S11(i,j,k)))
   elseif (tij.eq.21) then
      if (dvt.eq.iplus12) MS=0.5*(abs(S21(i+1,j,k))+abs(S21(i,j,k)))
      if (dvt.eq.iminus12) MS=0.5*(abs(S21(i-1,j,k))+abs(S21(i,j,k)))
   elseif (tij.eq.31) then
      if (dvt.eq.iplus12) MS=0.5*(abs(S31(i+1,j,k))+abs(S31(i,j,k)))
      if (dvt.eq.iminus12) MS=0.5*(abs(S31(i-1,j,k))+abs(S31(i,j,k)))
   elseif (tij.eq.12) then
      if (dvt.eq.jplus12) MS=0.5*(abs(S12(i,j+1,k))+abs(S12(i,j,k)))
      if (dvt.eq.jminus12) MS=0.5*(abs(S12(i,j-1,k))+abs(S12(i,j,k)))
   elseif (tij.eq.22) then
      if (dvt.eq.jplus12) MS=0.5*(abs(S22(i,j+1,k))+abs(S22(i,j,k)))
      if (dvt.eq.jminus12) MS=0.5*(abs(S22(i,j-1,k))+abs(S22(i,j,k)))
   elseif (tij.eq.32) then
      if (dvt.eq.jplus12) MS=0.5*(abs(S32(i,j+1,k))+abs(S32(i,j,k)))
      if (dvt.eq.jminus12) MS=0.5*(abs(S32(i,j-1,k))+abs(S32(i,j,k)))
   elseif (tij.eq.13) then
      if (dvt.eq.kplus12) MS=0.5*(abs(S13(i,j,k+1))+abs(S13(i,j,k)))
      if (dvt.eq.kminus12) MS=0.5*(abs(S13(i,j,k-1))+abs(S13(i,j,k)))
   elseif (tij.eq.23) then
      if (dvt.eq.kplus12) MS=0.5*(abs(S23(i,j,k+1))+abs(S23(i,j,k)))
      if (dvt.eq.kminus12) MS=0.5*(abs(S23(i,j,k-1))+abs(S23(i,j,k)))
   elseif (tij.eq.33) then
      if (dvt.eq.kplus12) MS=0.5*(abs(S33(i,j,k+1))+abs(S33(i,j,k)))
      if (dvt.eq.kminus12) MS=0.5*(abs(S33(i,j,k-1))+abs(S33(i,j,k)))
   endif
   if (MS.eq.-990000.0) then
       write(*,*) 'Error SGS - tij=',tij,' i=',i,' j=',j,' k=',k,' dvt=',dvt
       Stop 1
   endif
   vt = C*MS
   return
end function vt
