MODULE iarray
!
!  Contains all array var names.
!
    implicit none

    real, allocatable :: xface(:), yface(:), zface(:), zface_sc(:), zface_b(:),zface_d(:)
    real, allocatable :: rhokap(:,:,:)
    real, allocatable :: jmean(:,:,:), jmeanGLOBAL(:,:,:)
    real, allocatable :: refrac(:,:,:)
    real, allocatable :: albedoar(:, :, :)
    real, allocatable :: g(:,:), ua_sc(:,:), us_sc(:,:), ua_e(:,:), us_h(:,:),ua_h(:,:)
    real, allocatable :: us_e(:,:),ua_m(:,:),us_m(:,:),ua_b(:,:),us_b(:,:),ua_d(:,:),us_d(:,:)
end MODULE iarray

