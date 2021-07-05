MODULE ch_opt

implicit none

CONTAINS

subroutine init_opt(w)

use opt_prop
use iarray
use subs

implicit none
real :: coeff
integer :: wavelen
integer, intent(in) :: w

!search uploaded data files to find correct optical property value
!g
call findvalue(w, g, g_length,coeff)
hgg=coeff
g2=coeff**2.

!Stratum Corenum
call findvalue(w, ua_sc, ua_sc_length, coeff)
sc_mua=coeff
call findvalue(w, us_sc ,us_sc_length, coeff)
sc_mus=coeff

sc_kappa=sc_mus + sc_mua
sc_albedo=sc_mus/sc_kappa

!Epidermis
call findvalue(w, ua_e, ua_e_length, coeff)
e_mua=coeff
call findvalue(w, us_e ,us_e_length, coeff)
e_mus=coeff

e_kappa=e_mus + e_mua
e_albedo=e_mus/e_kappa

!Melanin
call findvalue(w, ua_m, ua_m_length, coeff)
m_mua=coeff
call findvalue(w, us_m ,us_m_length, coeff)
m_mus=coeff

m_kappa=m_mus + m_mua
m_albedo=m_mus/m_kappa

!Basal
call findvalue(w, ua_b, ua_b_length, coeff)
b_mua=coeff
call findvalue(w, us_b ,us_b_length, coeff)
b_mus=coeff

b_kappa=b_mus + b_mua
b_albedo=b_mus/b_kappa

!Dermis
call findvalue(w, ua_d, us_d_length, coeff)
d_mua=coeff
call findvalue(w, us_d ,us_d_length, coeff)
d_mus=coeff

d_kappa=d_mus + d_mua
d_albedo=d_mus/d_kappa

!Hypodermis
call findvalue(w, ua_h, us_h_length, coeff)
h_mua=coeff
call findvalue(w, us_h ,us_h_length, coeff)
h_mus=coeff

h_kappa=h_mus + h_mua
h_albedo=h_mus/h_kappa


end subroutine init_opt
   
   subroutine init_opt1
!
!  subroutine to set tissue optical properties 630nm
!
   use opt_prop
   
   implicit none

   hgg = 0.9d0
   g2  = hgg**2.
   mua = 0.23d0
   mus = 21.d0/(1.d0 - hgg)

   kappa  = mus + mua
   albedo = mus / kappa

   end subroutine init_opt1
   
   subroutine init_opt2
!
!  subroutine to set tissue optical properties 420nm
!
   use opt_prop

   implicit none

   hgg = 0.9
   g2  = hgg**2.
   mua = 1.8d0
   mus = 82.d0/(1.d0 - hgg)

   kappa  = mus + mua
   albedo = mus / kappa

   end subroutine init_opt2
   
   subroutine init_opt3
!
!  subroutine to set tissue optical properties 705nm
!
   use opt_prop

   implicit none

   hgg = 0.9
   g2  = hgg**2.
   mua = 0.23d0
   mus = 17.d0/(1.d0 - hgg)

   kappa  = mus + mua 
   albedo = mus / kappa

   end subroutine init_opt3
   
    subroutine init_opt4
        !
        !  subroutine to set 5-layer tissue optical properties for 222nm
        !
           use opt_prop

           implicit none
!strarum corneum 
           sc_hgg = 0.78
           sc_g2  = sc_hgg**2.
           sc_mua = 3502
           sc_mus = 2641

           sc_kappa  = 1!sc_mus + sc_mua
           sc_albedo = sc_mus / sc_kappa
           
!epidermis - minus melanin          
           e_hgg = 0.68
           e_g2  = e_hgg**2.
           e_mua = 2151
           e_mus = 2194

           e_kappa  = 2!e_mus + e_mua
           e_albedo = e_mus / e_kappa
           
!melanin layer           
           m_hgg = 0.68
           m_g2  = m_hgg**2.
           m_mua = 1354
           m_mus = 2194
           
           m_kappa  = 3!m_mus + m_mua
           m_albedo = m_mus / m_kappa
           
!basal layer           
           b_hgg = 0.68
           b_g2  = b_hgg**2.
           b_mua = 2151
           b_mus = 2194

           b_kappa  = 4!b_mus + b_mua
           b_albedo = b_mus / b_kappa
           
!dermis          
           d_hgg = 0.68
           d_g2  = d_hgg**2.
           d_mua = 597
           d_mus = 607
           
           d_kappa  = 5!d_mus + d_mua
           d_albedo = d_mus / d_kappa

       end subroutine init_opt4
       
    subroutine init_opt5
        !
        !  subroutine to set 5-layer tissue optical properties for 254nm
        !
           use opt_prop

           implicit none
!strarum corneum 
           sc_hgg = 0.78
           sc_g2  = sc_hgg**2.
           sc_mua = 1113
           sc_mus = 2643

           sc_kappa  = sc_mus + sc_mua
           sc_albedo = sc_mus / sc_kappa
           
!epidermis - minus melanin          
           e_hgg = 0.69
           e_g2  = e_hgg**2.
           e_mua = 872
           e_mus = 1890

           e_kappa  = e_mus + e_mua
           e_albedo = e_mus / e_kappa
           
!melanin layer           
           m_hgg = 0.69
           m_g2  = m_hgg**2.
           m_mua = 1001
           m_mus = 1890
           
           m_kappa  = m_mus + m_mua
           m_albedo = m_mus / m_kappa
           
!basal layer           
           b_hgg = 0.69
           b_g2  = b_hgg**2.
           b_mua = 872
           b_mus = 1890

           b_kappa  = b_mus + b_mua
           b_albedo = b_mus / b_kappa
           
!dermis          
           d_hgg = 0.69
           d_g2  = d_hgg**2.
           d_mua = 59
           d_mus = 446
           
           d_kappa  = d_mus + d_mua
           d_albedo = d_mus / d_kappa

       end subroutine init_opt5
       
    subroutine init_opt6
        !
        !  subroutine to set 5-layer tissue optical properties for 300nm
        !
           use opt_prop

           implicit none
!strarum corneum 
           sc_hgg = 0.78
           sc_g2  = sc_hgg**2.
           sc_mua = 483
           sc_mus = 2519

           sc_kappa  = sc_mus + sc_mua
           sc_albedo = sc_mus / sc_kappa
           
!epidermis - minus melanin          
           e_hgg = 0.71
           e_g2  = e_hgg**2.
           e_mua = 204
           e_mus = 1497

           e_kappa  = e_mus + e_mua
           e_albedo = e_mus / e_kappa
           
!melanin layer           
           m_hgg = 0.71
           m_g2  = m_hgg**2.
           m_mua = 279
           m_mus = 1497
           
           m_kappa  = m_mus + m_mua
           m_albedo = m_mus / m_kappa
           
!basal layer           
           b_hgg = 0.71
           b_g2  = b_hgg**2.
           b_mua = 204
           b_mus = 1497

           b_kappa  = b_mus + b_mua
           b_albedo = b_mus / b_kappa
           
!dermis          
           d_hgg = 0.71
           d_g2  = d_hgg**2.
           d_mua = 21
           d_mus = 304
           
           d_kappa  = d_mus + d_mua
           d_albedo = d_mus / d_kappa

       end subroutine init_opt6
       
 subroutine init_opt7
        !
        !  subroutine to set 5-layer tissue optical properties for 350nm
        !
           use opt_prop

           implicit none
!strarum corneum 
           sc_hgg = 0.78
           sc_g2  = sc_hgg**2.
           sc_mua = 185
           sc_mus = 2310

           sc_kappa  = sc_mus + sc_mua
           sc_albedo = sc_mus / sc_kappa
           
!epidermis - minus melanin          
           e_hgg = 0.72
           e_g2  = e_hgg**2.
           e_mua = 64
           e_mus = 1160

           e_kappa  = e_mus + e_mua
           e_albedo = e_mus / e_kappa
           
!melanin layer           
           m_hgg = 0.72
           m_g2  = m_hgg**2.
           m_mua = 107
           m_mus = 1160
           
           m_kappa  = m_mus + m_mua
           m_albedo = m_mus / m_kappa
           
!basal layer           
           b_hgg = 0.72
           b_g2  = b_hgg**2.
           b_mua = 64
           b_mus = 1160

           b_kappa  = b_mus + b_mua
           b_albedo = b_mus / b_kappa
           
!dermis          
           d_hgg = 0.72
           d_g2  = d_hgg**2.
           d_mua = 13
           d_mus = 214
           
           d_kappa  = d_mus + d_mua
           d_albedo = d_mus / d_kappa

       end subroutine init_opt7
       
 subroutine init_opt8
        !
        !  subroutine to set 5-layer tissue optical properties for 400nm
        !
           use opt_prop

           implicit none
!strarum corneum 
           sc_hgg = 0.78
           sc_g2  = sc_hgg**2.
           sc_mua = 132
           sc_mus = 2196

           sc_kappa  = sc_mus + sc_mua
           sc_albedo = sc_mus / sc_kappa
           
!epidermis - minus melanin          
           e_hgg = 0.74
           e_g2  = e_hgg**2.
           e_mua = 43
           e_mus = 890

           e_kappa  = e_mus + e_mua
           e_albedo = e_mus / e_kappa
           
!melanin layer           
           m_hgg = 0.74
           m_g2  = m_hgg**2.
           m_mua = 72
           m_mus = 890
           
           m_kappa  = m_mus + m_mua
           m_albedo = m_mus / m_kappa
           
!basal layer           
           b_hgg = 0.74
           b_g2  = b_hgg**2.
           b_mua = 43
           b_mus = 890

           b_kappa  = b_mus + b_mua
           b_albedo = b_mus / b_kappa
           
!dermis          
           d_hgg = 0.74
           d_g2  = d_hgg**2.
           d_mua = 8.7
           d_mus = 159
           
           d_kappa  = d_mus + d_mua
           d_albedo = d_mus / d_kappa

       end subroutine init_opt8
   
   

   subroutine sample(array, size_of, cdf, wave, iseed)
!      
!  samples a random value from an array based upon its cdf     
!      
      implicit none
      
      integer, intent(IN)    :: iseed, size_of
      real,    intent(IN)    :: array(size_of, 2), cdf(size_of)
      real,    intent(OUT)   :: wave

      real :: ran2, value
      integer :: nlow
      
      value = ran2(iseed)
      
      call search_1D(size(cdf), cdf, nlow, value)
      call lin_inter_1D(array, cdf, value, size(cdf), nlow, wave)
   
   end subroutine sample
   
   subroutine lin_inter_1D(array, cdf, value, length, nlow, y)
!
!  linear interpolates between values for an array and its cdf
!   
      implicit none
   
      real,    intent(OUT)  :: y
      integer, intent(IN)   :: length
      real,    intent(IN)   :: value,array(length,2),cdf(length-1)
      integer, intent(IN)   :: nlow
   
      y = array(nlow+1,1) + (array(nlow+2,1) - array(nlow+1,1)) * (value - cdf(nlow))/(cdf(nlow+1) - cdf(nlow))
   
   end subroutine lin_inter_1D
   
   subroutine lin_inter_2D(array,value,length,nlow,y)
!
!  linear interpolation for an array
!
      implicit none

      real,    intent(OUT)  :: y
      integer, intent(IN)   :: length
      real,    intent(IN)   :: value,array(length,2)
      integer, intent(IN)   :: nlow
   
      y = array(nlow,2) + (array(nlow+1,2) - array(nlow,2)) * (value - array(nlow,1))/(array(nlow+1,1) - array(nlow,1))
   
   end subroutine lin_inter_2D
   
   subroutine search_1D(length,array,nlow,value)
!
!  search by bisection for 1D array
!
      implicit none
      
      integer              :: nup,length,middle
      integer, intent(OUT) :: nlow
      real,    intent(in)  :: array(length),value
      
      nup = length
      nlow = 1
      middle = int((nup+nlow)/2.)

      do while((nup - nlow).gt.1)
         middle = int((nup + nlow)/2.)
         if(value.gt.array(middle))then
            nlow = middle
         else
            nup = middle   
         end if
      end do
   end subroutine search_1D
   
   subroutine search_2D(length,array,nlow,value)
!
!  search by bisection for 2D array
!
      implicit none
      
      integer              :: nup,length,middle
      integer, intent(OUT) :: nlow
      real,    intent(in)  :: array(length,2),value
      
      nup = length
      nlow = 1
      middle = int((nup+nlow)/2.)

      do while((nup - nlow).gt.1)
         middle = int((nup + nlow)/2.)
         if(value.gt.array(middle,1))then
            nlow = middle
         else
            nup = middle   
         end if
      end do
   end subroutine search_2D
   
   subroutine mk_cdf(array,cdf,length)
!
!  subroutine that creates cdf for an array of values.
!
      implicit none

      integer, intent(IN)    :: length
      real,    intent(IN)    :: array(length,2)
      real,    intent(INOUT) :: cdf(length)
      real                   :: summ
      integer                :: i,j
   
      do j=1,length-1
         summ=0.
         do i=1,j   
            summ=summ+0.5*(array(i+1,2)+array(i,2))*(array(i+1,1)-array(i,1))
         end do
         cdf(j)=summ      
      end do
      cdf=cdf/cdf(length-1)
   
   end subroutine mk_cdf
end module ch_opt
