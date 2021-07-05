program mcpolar

use mpi

!shared data
use constants
use photon_vars
use iarray
use opt_prop
use utils, only : blue, bold, colour, red, white_b, black, str
use iso_fortran_env, only: int64

!subroutines
use subs
use gridset_mod
use sourceph_mod
use inttau2
use ch_opt
use stokes_mod
use writer_mod

implicit none

integer(kind=int64) :: nphotons, j
integer          :: iseed, xcell, ycell, zcell, w
logical          :: tflag
double precision :: nscatt
real             :: ran, delta, start,finish,ran2, start2

integer :: id, error, numproc, cnt
real    :: nscattGLOBAL


call cpu_time(start)
call cpu_time(start2)

!set directory paths
call directory

!allocate and set arrays to 0
call alloc_array
call zarray

!init MPI
 call MPI_init(error)
 call MPI_Comm_size(MPI_COMM_WORLD, numproc, error)
 call MPI_Comm_rank(MPI_COMM_WORLD, id, error)
!id = 0
!numproc =0 

!**** Read in parameters from the file input.params
open(10,file=trim(resdir)//'input.params',status='old')
   read(10,*) nphotons
   read(10,*) xmax
   read(10,*) ymax
   read(10,*) zmax
   read(10,*) n1
   read(10,*) n2
   close(10)
   
 !****** Read in optical property files
!call readfile_array2D('g.dat', g,0,1,cnt)
!g_length=cnt
!call readfile_array2D('ua_sc.dat', ua_sc,0,1,cnt)
!ua_sc_length=cnt
!call readfile_array2D('us_sc.dat', us_sc,0,1,cnt)
!us_sc_length=cnt
!call readfile_array2D('ua_e.csv', ua_e,0,2,cnt)
!ua_e_length=cnt
!call readfile_array2D('us_e.dat', us_e,0,1,cnt)
!us_e_length=cnt
!call readfile_array2D('ua_m.dat', ua_m,0,1,cnt)
!ua_m_length=cnt
!call readfile_array2D('us_e.dat', us_m,0,1,cnt)
!us_m_length=cnt
!call readfile_array2D('ua_e.csv', ua_b,0,2,cnt)
!ua_b_length=cnt
!call readfile_array2D('us_e.dat', us_b,0,1,cnt)
!us_b_length=cnt
!call readfile_array2D('ua_d.dat', ua_d,0,1,cnt)
!ua_d_length=cnt
!call readfile_array2D('us_d.dat', us_d,0,1,cnt)
!us_d_length=cnt
!call readfile_array2D('ua_h.dat', ua_h,0,1,cnt)
!ua_h_length=cnt
!call readfile_array2D('us_h.dat', us_h,0,1,cnt)
!us_h_length=cnt

!print*, ua_e

call readfile_array2D('g.txt', g, 0, 2, cnt)
g_length=cnt
call readfile_array2D('ua_sc.txt', ua_sc, 0, 2, cnt)
ua_sc_length=cnt
call readfile_array2D('us_sc.txt', us_sc, 0, 2, cnt)
us_sc_length=cnt
call readfile_array2D('ua_e.csv', ua_e, 0, 2, cnt)
ua_e_length=cnt
call readfile_array2D('us_e.txt', us_e, 0, 2, cnt)
us_e_length=cnt
call readfile_array2D('ua_m.csv', ua_m, 0, 2, cnt)
ua_m_length=cnt
call readfile_array2D('us_e.txt', us_m, 0, 2, cnt)
us_m_length=cnt
call readfile_array2D('ua_b.txt', ua_b, 0, 2, cnt)
ua_b_length=cnt
call readfile_array2D('us_e.txt', us_b, 0, 2, cnt)
us_b_length=cnt
call readfile_array2D('ua_d.txt', ua_d, 0, 2, cnt)
ua_d_length=cnt
call readfile_array2D('us_d.txt', us_d, 0, 2, cnt)
us_d_length=cnt
call readfile_array2D('ua_h.txt', ua_h, 0, 2, cnt)
ua_h_length=cnt
call readfile_array2D('us_h.txt', us_h, 0, 2, cnt)
us_h_length=cnt


do w=607,699 !start loop over chosen wavelength range

call zarray


! set seed for rnd generator. id to change seed for each process
iseed=-734128+id

iseed=-abs(iseed)  ! Random number seed must be negative for ran2

call init_opt(w)

if(id == 0)then
   print*, ''      
   print*,'# of photons to run',nphotons*numproc
end if



!***** Set up density grid *******************************************
call gridset(id,w)

!***** Set small distance for use in optical depth integration routines 
!***** for roundoff effects when crossing cell walls
delta = 1.e-8*(2.*xmax/nxg)
nscatt=0

 call cpu_time(start)

!loop over photons 
print*,'Photons now running on core: ',id

do j = 1, nphotons

   tflag=.FALSE.
   
   !call estimateSimulationTime(start2, j, 10000, nphotons, id)

   if(mod(j,10000) == 0)then
      print *, j,' scattered photons completed on core: ',id,w
   end if

    
!***** Release photon from point source *******************************
   call evenDis(xcell,ycell,zcell,iseed)
!****** Find scattering location
      call tauint1(xcell,ycell,zcell,tflag,iseed,delta)
!******** Photon scatters in grid until it exits (tflag=TRUE)

      do while(tflag.eqv..FALSE.)

         ran = ran2(iseed)
         if(ran < albedoar(xcell,ycell,zcell))then!interacts with tissue
               call stokes(iseed)
               nscatt = nscatt + 1        
            else
               tflag=.true.
               exit
         end if

!************ Find next scattering location

         call tauint1(xcell,ycell,zcell,tflag,iseed,delta)

      end do
end do      ! end loop over loop over n photons


jmeanGLOBAL = jmean
nscattGLOBAL = nscatt
! collate fluence from all processes
call mpi_reduce(jmean, jmeanGLOBAL, (nxg*nyg*nzg),MPI_DOUBLE_PRECISION, MPI_SUM,0,MPI_COMM_WORLD,error)
call mpi_reduce(nscatt,nscattGLOBAL,1,MPI_DOUBLE_PRECISION,MPI_SUM,0,MPI_COMM_WORLD,error)


if(id == 0)then
   print*,'Average # of scatters per photon:',nscattGLOBAL/(nphotons*numproc)
   !write out files
   call writer(w,xmax,ymax,zmax,nphotons, numproc)
   print*,'write done'
end if

end do !ends loop over w

call cpu_time(finish)
if(finish-start.ge.60. .and. id==0)then
   print*,floor((finish-start)/60.)+mod(finish-start,60.)/100.
else
   if(id==0)print*, 'time taken ~',floor(finish-start/60.),'s'
end if
call MPI_Finalize(error)
end program mcpolar
