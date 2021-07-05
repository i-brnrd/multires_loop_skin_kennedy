MODULE gridset_mod

    implicit none

    private
    public :: gridset

    contains
        subroutine gridset(id,w)

            use constants, only : nxg, nyg, nzg, xmax, ymax, zmax, PI
            use iarray, only    : rhokap,xface,yface,zface, rhokap, refrac, albedoar, zface_sc, zface_b, zface_d
            use opt_prop, only  : kappa, n1, n2, e_mua, m_mus
            use ch_opt
            use opt_prop
            use photon_vars, only: d_nzg, e_nzg, m_nzg,b_nzg, sc_nzg, h_nzg, x_dep, y_dep
            use subs, only: opticalpropgrid

            implicit none

            integer, intent(IN) :: id

            integer             :: i, j, k,w, sc_vox, e_vox,b_vox, d_vox
            real                :: x, y, z, taueq1, taupole1, taueq2, taupole2
           
            real 			    :: a,b, sine, sc, e, m, ba, d
            real :: Vmel, wavelen, ua_mel, Vmel_adapted, m_vox, pmel_vox
            integer:: sc_z_vox, e_z_vox, m_z_vox,b_z_vox, d_z_vox, h_z_vox
          
            real:: sc_width, e_width, m_width, d_width, h_width, b_width
            
            
            Vmel=0.02
            wavelen= w
            

            if(id == 0)then
                print*, ' '
                print *, 'Setting up density grid....'
            end if
            
            
            !h_depth is to bottom of the grid.
            
            !calculate layer_nzg for each layer
            
            sc_width=0.002 !thicknesess of each resolution layer
            e_width=0.0053
            m_width=0.0032 
            b_width=0.0032
            d_width=0.1970
            h_width=0.3
            
            sc_z_vox=200
            e_z_vox=100
            m_z_vox=100
            b_z_vox=100
            d_z_vox=150
            h_z_vox=150
            
            sc_nzg= (2.*zmax)/(sc_width/sc_z_vox)
            e_nzg= (2.*zmax)/(e_width/e_z_vox)
            m_nzg=(2.*zmax)/(m_width/m_z_vox)
            b_nzg= (2.*zmax)/(b_width/b_z_vox)
            d_nzg= (2.*zmax)/(d_width/d_z_vox)
            h_nzg= (2.*zmax)/(h_width/h_z_vox)
            
            

            ! setup grid faces
            do i = 1, nxg + 1
                xface(i) = (i - 1) * 2. * xmax/nxg
            end do

            do i = 1, nyg + 1
                yface(i) = (i - 1) * 2. * ymax/nyg
            end do
            
            do i = 1, 201
                zface(i) = (i - 1) * 2. * zmax/sc_nzg
                
            end do
            
            do i = 202, 301
               zface(i) =zface(201)+(i - 201) * 2. * zmax/e_nzg
    
            end do
            
              do i = 302, 401
               zface(i) = zface(301)+(i - 301) * 2. * zmax/m_nzg
           
            end do
            
            do i = 402, 501
                zface(i) = zface(401)+(i - 401) * 2. * zmax/b_nzg
            end do
            
            do i = 502, 651
                zface(i) =zface(501)+(i - 501) * 2. * zmax/d_nzg
            end do
            
            do i = 652, 800+1
                zface(i) = zface(651)+(i - 651) * 2. * zmax/h_nzg
            end do
            
        
            
            

            !call init_opt
            refrac(:,:,:) = n1

            !set up optical properties grid 
            
       sine=0.003*(SIN(x/0.0015)*COS(y/0.0015))
       
        
  		


 
  		
!set up inital grid optical properties

call opticalpropgrid(1, 200, sc_nzg) ! sets 0.002cm sc with 200 voxel z depth
 
call opticalpropgrid(201, 300, e_nzg)!sets 0.0064cm epidermis with 200 voxel z depth 
 
call opticalpropgrid(301, 400, m_nzg)!sets 0.001cm melanin 100 voxel z depth
call opticalpropgrid(401, 500, b_nzg)!sets 0.001cm basal with 100 voxel z depth

call opticalpropgrid(501, 650, d_nzg)!sets 0.2cm dermis with 100 voxel z depth

call opticalpropgrid(651, nzg, h_nzg)!sets rest of grid hypodermis with 100 voxel z depth
 

           
             !******************* Count Voxels per layer ******************************************
            
            sc_vox=0
            e_vox=0
            m_vox=0
            b_vox=0
            d_vox=0
            
              do i = 1, nxg           
           	do j = 1, nyg
            		do k = 1,nzg 
            		
            		if (rhokap(i,j,k) .eq. sc_kappa) then
            			sc_vox= sc_vox + 1
            		elseif(rhokap(i,j,k) .eq. e_kappa) then
            			e_vox = e_vox + 1
            		elseif(rhokap(i,j,k) .eq. m_kappa) then
            			m_vox = m_vox + 1
            		elseif (rhokap(i,j,k) .eq. b_kappa) then
            			b_vox=b_vox+1
            		elseif( rhokap(i,j,k) .eq.d_kappa) then
            			d_vox=d_vox + 1
            		end if 
            		            		            		            	
            		end do
            	end do
            end do
            
            
             
            
          
            
            !****************** Calulate melanin layer Vmel and absorption based on layer thickness ***********************
            
            pmel_vox= Vmel * (e_vox + m_vox)
            
            Vmel_adapted = pmel_vox / m_vox
                     
            
            ua_mel = (6.6 * 10.**(11) * wavelen **(-3.33) * Vmel_adapted) + e_mua
            
            m_kappa  = m_mus + ua_mel
            m_albedo = m_mus / m_kappa
            
         
            
     
!set up optcial properties grid with new melanin absorption
            
call opticalpropgrid(1, 200, sc_nzg) ! sets 0.002cm sc with 200 voxel z depth
 
call opticalpropgrid(201, 300, e_nzg)!sets 0.0064cm epidermis with 200 voxel z depth 
 
call opticalpropgrid(301, 400, m_nzg)!sets 0.001cm melanin 100 voxel z depth
call opticalpropgrid(401, 500, b_nzg)!sets 0.001cm basal with 100 voxel z depth

call opticalpropgrid(501, 650, d_nzg)!sets 0.2cm dermis with 100 voxel z depth

call opticalpropgrid(651, nzg, h_nzg)!sets rest of grid hypodermis with 100 voxel z depth
 

            !****************** Add cavity into skin to simulate a cut ****************************
          
            
          
        ! a=4
         !b=2

            
            !do i=1,nxg
            !x = (i-1)*2.*xmax/nxg
            	!do j=1, nyg
            	!y=(j-1)*2.*ymax/nyg
            		!do k=1, nzg
            		!z=(k-1)*2.*zmax/nzg 
           		!if(sqrt(a**2*(x-0.05)**2+b**2*(y-0.05)**2) .le. -(z-0.075)) then 
            		!rhokap(i,j,k)=0
            		!albedoar(i,j,k)=0
            		!end if
            		!end do
            	!end do
            !end do
            		
            

            !****************** Calculate equatorial and polar optical depths ****
            taueq1   = 0.
            taupole1 = 0.
            taueq2   = 0.
            taupole2 = 0.

            do i = 1, nxg
                taueq1 = taueq1 + rhokap(i,nyg/2,nzg/2)
            end do

            do i = 1, nzg
                taupole1 = taupole1 + rhokap(nxg/2,nyg/2,i)
            end do

            taueq1 = taueq1 * 2. * xmax/nxg
            taupole1 = taupole1 * 2. * zmax/nzg
            if(id == 0)then
                print'(A,F9.5,A,F9.5)',' taueq1 = ',taueq1,'  taupole1 = ',taupole1
            end if

            if(id == 0)then
                inquire(iolength=i)refrac(:,:,:nzg)
                open(newunit=j,file='refrac.dat',access='stream',form='unformatted',status='replace')
                write(j)refrac(:,:,:)
                close(j)
            end if
             !call mpi_finalize()
            ! stop
        end subroutine gridset
end MODULE gridset_mod


