module writer_mod

implicit none

    contains
        subroutine writer(w, xmax, ymax, zmax, nphotons, numproc)

            use constants, only : nxg,nyg,nzg,fileplace
            use iarray,    only : jmeanGLOBAL, albedoar, rhokap
            use photon_vars, only : d_nzg, e_nzg, m_nzg, b_nzg, sc_nzg, h_nzg
            use iso_fortran_env, only: int64

            implicit none
            
            integer(kind=int64) :: nphotons

            integer           :: i, u, numproc, j,k, w
            real              :: xmax, ymax, zmax, jm(nzg)
            character(len=10) :: file_id
            character(len=50) :: file_name
            logical :: exist
             character(len=70) :: fn
    !integer, parameter :: numfiles=40
            
            
            
!normalise the fluence rate  - different loops for each resolution
           
            do i = 1, nxg 
          
          	 do j = 1, nyg 
            
            		do k = 1, 200
            		
               		jmeanGLOBAL(i,j,k) =jmeanGLOBAL(i,j,k) * ((2.*xmax)**2./(nphotons*numproc*(2.*xmax/nxg) &
                		*(2.*ymax/nyg)*(2.*zmax/sc_nzg)))
                		
                		

           		 end do
            	end do 
           end do 
           
             do i = 1, nxg 
          
          	 do j = 1, nyg 
            
            		do k = 201, 300
                		jmeanGLOBAL(i,j,k) =jmeanGLOBAL(i,j,k) * ((2.*xmax)**2./(nphotons*numproc*(2.*xmax/nxg) &
                		*(2.*ymax/nyg)*(2.*zmax/e_nzg)))
           		 end do
            	end do 
           end do
            
              do i = 1, nxg 
          
          	 do j = 1, nyg 
            
            		do k = 301, 400
                		jmeanGLOBAL(i,j,k) =jmeanGLOBAL(i,j,k) * ((2.*xmax)**2./(nphotons*numproc*(2.*xmax/nxg) &
                		*(2.*ymax/nyg)*(2.*zmax/m_nzg)))
           		 end do
            	end do 
           end do 
           
             do i = 1, nxg 
          
          	 do j = 1, nyg 
            
            		do k = 401, 500
                		jmeanGLOBAL(i,j,k) =jmeanGLOBAL(i,j,k) * ((2.*xmax)**2./(nphotons*numproc*(2.*xmax/nxg) &
                		*(2.*ymax/nyg)*(2.*zmax/b_nzg)))
           		 end do
            	end do 
           end do
           
            do i = 1, nxg 
          
          	 do j = 1, nyg 
            
            		do k = 501, 650
                		jmeanGLOBAL(i,j,k) =jmeanGLOBAL(i,j,k) * ((2.*xmax)**2./(nphotons*numproc*(2.*xmax/nxg) &
                		*(2.*ymax/nyg)*(2.*zmax/d_nzg)))
           		 end do
            	end do 
           end do 
            
           do i = 1, nxg 
          
          	 do j = 1, nyg 
            
            		do k = 651, 800
                		jmeanGLOBAL(i,j,k) =jmeanGLOBAL(i,j,k) * ((2.*xmax)**2./(nphotons*numproc*(2.*xmax/nxg) &
                		*(2.*ymax/nyg)*(2.*zmax/h_nzg)))
           		 end do
            	end do 
           end do 
           

            !jmeanGLOBAL =jmeanGLOBAL * ((2.*xmax)**2./(nphotons*numproc*(2.*xmax/nxg)*(2.*ymax/nyg)*(2.*zmax/nzg)))

            inquire(iolength=i)jmeanGLOBAL

           ! open(newunit=u,file=trim(fileplace)//'jmean400.dat',access='stream',status='REPLACE',form='unformatted')
            !write(u) jmeanGLOBAL
            !close(u)
            
            !open(newunit=u,file=trim(fileplace)//'rhokap.dat',access='stream',status='REPLACE',form='unformatted')
            !write(u) rhokap
            !close(u)
            
         ! build filename -- i.dat
        write(fn,fmt='(i0,a)') w, 'jmean_diffuse.dat'

        ! open it with a fixed unit number
        open(newunit=u,file=trim(fileplace)//fn, status="replace", access='stream', form='unformatted')

        ! write something
        write(u) jmeanGLOBAL

        ! close it 
        close(u)
            
         
            
            

           ! jm = 0.d0
            !do k = 1, nzg
                !do j = 1, nyg
                   ! do i = 1, nxg
                    !    jm(k) = jm(k) + jmeanGLOBAL(i,j,k)
                    !end do
                !end do
            !end do

            !jm = jm / (nxg*nyg)

            !open(newunit=u,file=trim(fileplace)//"jmean/validation-350-full_sc.dat",status="replace")
            !do i = nzg,1,-1
                !write(u,*)real(nzg-i)*(2./nzg),jm(i)
           ! end do
            !close(u)
        end subroutine writer
end module writer_mod
