MODULE subs

implicit none

    contains

        subroutine directory
        !  subroutine defines vars to hold paths to various folders   
        !   
        !   
            use constants, only : cwd, homedir, fileplace, resdir

            implicit none

            !get current working directory

            call get_environment_variable('PWD', cwd)

            ! get 'home' dir from cwd
            homedir = trim(cwd(1:len(trim(cwd))-3))
            ! get data dir
            fileplace = trim(homedir)//'data/'
            ! get res dir
            resdir = trim(homedir)//'res/'

        end subroutine directory


        subroutine zarray

            use iarray

            !sets all arrays to zero
            implicit none


            jmean = 0.
            xface = 0.
            yface = 0.
            zface = 0.
            rhokap = 0.
            jmeanGLOBAL = 0.
            refrac = 0.
            albedoar=0.
            !test=0.
        end subroutine zarray


        subroutine alloc_array
        !  subroutine allocates allocatable arrays
        !   
        !   
            use iarray
            use constants,only : nxg,nyg,nzg
            
            implicit none
                real:: wavelength, datavalue

            allocate(xface(nxg+1), yface(nyg + 1),zface(nzg + 1))
            allocate(rhokap(nxg, nyg, nzg))
            allocate(albedoar(nxg, nyg, nzg))
            allocate(jmean(nxg, nyg, nzg), jmeanGLOBAL(nxg, nyg, nzg))
            allocate(refrac(nxg, nyg, nzg ))
            
            !allocate(test(5,5))
        end subroutine alloc_array
        
        
        
	!subroutine fileread
	!reads in data files and allocates them to arrays
	
	!use iarray
	
	!implicit none
	
	!real :: wavelength, datavalue
	
	!integer:: i,j,k
	
	!open(unit=2, file='test.txt')
	!i=1
	!j=2
	!k=1
	!do i=1,5
	!read(2,*)wavelength,datavalue
	!print*, 'readd'
	!test(k,i)=wavelength
	!test(k,j)=datavalue
	!k=k+1
	!print*, wavelength, datavalue 
	

	
	!end do
	
	

	
	!close(2)
	
	
	
	!end subroutine fileread
	



!        subroutine readfile_array2D(filename, array, flag, colsize, cnt)
        
 !       implicit none 
        !
        ! Reads a file to get its length and allocates an array to store the data and reads it in.
        !
        ! subroutine takes filename, the array for dat to be read into, a flag to toggle
        ! between using square array and other shapes, colsize is an optional argument that
        ! specifies the size of the 2nd dimension of the array
        !
  !          DOUBLE PRECISION, allocatable, dimension(:), intent(inout) :: array 
        !    real, allocatable, intent (inout):: array
   !         integer,                           intent(in)    :: flag
    !        integer,                 optional, intent(in)    :: colsize
     !       character(*),                      intent(in)    :: filename
      !      integer                                          :: io, i, j
       !     integer, intent(out) :: cnt

        !    open(10, file = filename, status = 'OLD', IOSTAT = io)
         !   if(io .ne. 0)then
          !      print'(A,A,I2)',filename,' could not be opened. IOSTAT = ',io
           !     print*,'Exiting...reader'
            !    stop
         !  else
          !      cnt = 0
           !     do       !find file size and allocate array.

            !        read(10, *, IOSTAT = io)
!
  !                  if (io < 0) then
 !                       close(10)
   !                     if(flag .eq. 0)then
    !                        allocate(array(cnt))
     !                          
      !                  elseif(flag .eq. 1)then
       !                     allocate(array(cnt ))
        !                end if
         !               array=0.
          !              exit
           !         else
            !            cnt = cnt + 1
             !       end if

              !  end do
               
              !  open(20, file = filename) 

                !read in data
               ! if(flag .eq. 0)then
                !    do i = 1, cnt 
                 !       read(20,*) array(i)
                  !  end do
               ! elseif(flag .eq. 1)then
                !    do i = 1, cnt 
                 !   read(20,*) array(i)
                  !  end do
                !end if
                !close(20)
           ! end if
            
        
        !end subroutine readfile_array2D

        subroutine readfile_array2D(filename, array, flag, colsize, cnt)

        implicit none
        !
        ! Reads a file to get its length and allocates an array to store the
        ! data and reads it in.
        !
        ! subroutine takes filename, the array for dat to be read into, a flag
        ! to toggle
        ! between using square array and other shapes, colsize is an optional
        ! argument that
        ! specifies the size of the 2nd dimension of the array
        !
            DOUBLE PRECISION, allocatable, dimension(:,:), intent(inout) :: array
            integer,                           intent(in)    :: flag
            integer,                 optional, intent(in)    :: colsize
            character(*),                      intent(in)    :: filename
            integer                                          :: io, i, j
            integer, intent(out) :: cnt

            open(10, file = filename, status = 'OLD', IOSTAT = io)
            if(io .ne. 0)then
                print'(A,A,I2)',filename,' could not be opened. IOSTAT = ',io
                print*,'Exiting...reader'
                stop
           else
                cnt = 0
                do       !find file size and allocate array.

                    read(10, *, IOSTAT = io)

                    if (io < 0) then
                        close(10)
                        if(flag .eq. 0)then
                            allocate(array(cnt , colsize))
                        elseif(flag .eq. 1)then
                            allocate(array(cnt , cnt))
                        end if
                        array=0.
                        exit
                    else
                        cnt = cnt + 1
                    end if

                end do

                open(20, file = filename)
!read in data
                if(flag .eq. 0)then
                    do i = 1, cnt
                        read(20, *) array(i, 1),array(i, 2)
                    end do
                elseif(flag .eq. 1)then
                    do i = 1, cnt
                    read(20, *) (array(i, j), j = 1, cnt)
                    end do
                end if
                close(20)
            end if


        end subroutine readfile_array2D



        subroutine readfile(filename,array)
        implicit none

        DOUBLE PRECISION,  allocatable, intent(inout) :: array(:)
        character(*), intent(in) :: filename
                open(1, file= filename)
                read(1,*) 
                allocate(array(200))
                print*, filename, array
               ! read(1,*) array
                close(1) 

        end subroutine readfile
        

        
         subroutine findvalue(wavelength, array,length, coeff)


          use iarray

        implicit none
        integer, intent(in):: length
          real,    intent(IN)    :: array(length,2)
            real,    intent(OUT)  :: coeff
            integer, intent(IN)   :: wavelength
            real :: x
            integer:: i

         do i=1,length

         x= abs(array(i,1)-wavelength)
         if (FLOOR(x) .eq. 0.)then
         exit
         end if
         end do

         coeff=array(i,2)


        end subroutine findvalue

        
        
        subroutine opticalpropgrid(kmin, kmax, layer_nzg)
        use iarray
        use constants
        use opt_prop
        
        
        implicit none
        
        integer, intent(IN):: kmin, kmax
        real, intent(IN):: layer_nzg
        
        
         real                :: x, y, z,sc_dep, e_dep, m_dep, ba_dep, d_dep
           
         real 			  ::sine, sc, e, m, ba, d
         
         integer :: i,j,k
         
         !call init_opt
         
         !set layer bottom depths
            
            sc_dep=0.002
            e_dep=0.0084 
            m_dep=0.0094
            ba_dep=0.0104
            d_dep=0.21
        
        do i= 1,nxg
	x = xface(i) - xmax + xmax/nxg
	do j=1,nyg
		y = yface(j) - ymax + ymax/nyg
		do k=kmin,kmax
			z = zface(k) - zmax + zmax/layer_nzg
              
           
           
             
             
       
        	sc=-zmax+(sc_dep)
  		e=0.003*(SIN(x/0.0015)*COS(y/0.0015))-(zmax-e_dep)
  		m=0.003*(SIN(x/0.0015)*COS(y/0.0015))-(zmax-m_dep)
  		ba=0.003*(SIN(x/0.0015)*COS(y/0.0015))-(zmax-ba_dep)
  		d=-zmax+(d_dep)
  		
  	
              

 			 if (z.le.sc)then
     				rhokap(i,j,k)=sc_kappa
            			albedoar(i,j,k)=sc_albedo
            			refrac(i,j,k) = n2
  			elseif (z.le.e) then
     			rhokap(i,j,k)=e_kappa
           			albedoar(i,j,k)=e_albedo
            			refrac(i,j,k) = n2
 			 elseif (z.le.m) then
     			rhokap(i,j,k)=m_kappa
           			albedoar(i,j,k)=m_albedo
           			refrac(i,j,k) = n2
  			elseif (z.le.ba)then
     			rhokap(i,j,k)=b_kappa
            			albedoar(i,j,k)=b_albedo
            			refrac(i,j,k) = n2
            			
  			elseif (z .le.d)then
     			rhokap(i,j,k)=d_kappa
           			albedoar(i,j,k)=d_albedo
            			refrac(i,j,k) = n2
            		else
     			rhokap(i,j,k)=h_kappa
           			albedoar(i,j,k)=h_albedo
            			refrac(i,j,k) = n2
 			 endif
 		 
 	
 			 
 	         end do
 	end do
 end do
 

      
        
        end subroutine opticalpropgrid

        
                     
end MODULE subs














