MODULE photon_vars
!
! Module containing photon vars:
!           %p:current position of photon
!           n%p:current direction of photon
!           sin/cos(%)/phi:various angles related to photons flight 
!           angles measured from ''north'' in thetas case.

    implicit none

    real :: xp,yp,zp,nxp,nyp,nzp,sint,cost,sinp,cosp,phi
    real :: d_nzg, e_nzg, m_nzg, sc_nzg, h_nzg, b_nzg
    real::  x_dep, y_dep

end MODULE photon_vars
