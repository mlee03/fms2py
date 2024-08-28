module cFMS_mod

  use FMS, only : fms_init, fms_mpp_domains_init, fms_string_utils_c2f_string
  use FMS, only : FmsMppDomain2D
  use FMS, only : fms_mpp_domains_define_domains, fms_mpp_domains_define_io_domain
  use FMS, only : fms_mpp_domains_set_compute_domain, fms_mpp_domains_set_data_domain, fms_mpp_domains_set_global_domain
  use iso_c_binding

  implicit none
  private
  
  public :: cFMS_fms_init
  public :: cFMS_get_domain
  public :: cFMS_define_domains2D, cFMS_define_io_domain2D
  public :: cFMS_set_compute_domain2D, cFMS_set_data_domain2D, cFMS_set_global_domain2D

  character(100), parameter :: input_nml_path="./input.nml"
  
  type(FmsMppDomain2D), public :: Domain2D
  
contains

  !> returns module Domain 
  function cFMS_get_domain()
    type(FmsMppDomain2D) :: cFMS_get_domain
    cFMS_get_domain = Domain2D
  end function cFMS_get_domain


  subroutine cFMS_fms_init(localcomm, alt_input_nml_path_ptr) bind(C)

    implicit none
    integer, intent(in), optional     :: localcomm
    type(c_ptr), intent(in), optional :: alt_input_nml_path_ptr
    
    character(100) :: alt_input_nml_path = input_nml_path
    
    if(present(alt_input_nml_path_ptr)) &
         alt_input_nml_path = fms_string_utils_c2f_string(alt_input_nml_path_ptr)
    
    call fms_init(localcomm=localcomm, alt_input_nml_path=alt_input_nml_path)

  end subroutine cFMS_fms_init
  
  !> call mpp_define_domain
  subroutine cFMS_define_domains2D(global_indices, layout,                        &
       n_pelist, n_xextent, n_yextent, n1_maskmap, n2_maskmap, n_memory_size,     &
       pelist, xflags, yflags, xhalo, yhalo, xextent, yextent, maskmap, name_ptr, &
       symmetry, memory_size, whalo, ehalo, shalo, nhalo, is_mosaic, tile_count,  &
       tile_id, complete, x_cyclic_offset, y_cyclic_offset) bind(C)

    implicit none

    integer, intent(in) :: global_indices(4) 
    integer, intent(in) :: layout(2)
    integer, intent(in) :: n_pelist, n_xextent, n_yextent
    integer, intent(in) :: n1_maskmap, n2_maskmap
    integer, intent(in) :: n_memory_size    
    integer, intent(in), optional :: pelist(n_pelist) 
    integer, intent(in), optional :: xflags, yflags
    integer, intent(in), optional :: xhalo, yhalo
    integer, intent(in), optional :: xextent(n_xextent), yextent(n_yextent)
    logical, intent(in), optional :: maskmap(n1_maskmap,n2_maskmap)
    type(c_ptr), intent(in), optional :: name_ptr
    logical, intent(in), optional :: symmetry
    logical, intent(in), optional :: is_mosaic
    integer, intent(in), optional :: memory_size(n_memory_size)
    integer, intent(in), optional :: whalo, ehalo, shalo, nhalo
    integer, intent(in), optional :: tile_count
    integer, intent(in), optional :: tile_id
    logical, intent(in), optional :: complete
    integer, intent(in), optional :: x_cyclic_offset
    integer, intent(in), optional :: y_cyclic_offset

    character(len=20) :: name = input_nml_path

    call fms_mpp_domains_init()

    if(present(name_ptr)) name = fms_string_utils_c2f_string(name_ptr)
    
    call fms_mpp_domains_define_domains(global_indices, layout, Domain2D, pelist, xflags, yflags,    &
         xhalo, yhalo, xextent, yextent, maskmap, name, symmetry,  memory_size,                      &
         whalo, ehalo, shalo, nhalo, is_mosaic, tile_count, tile_id, complete, x_cyclic_offset, y_cyclic_offset)

  end subroutine cFMS_define_domains2D


  subroutine cFMS_define_io_domain2D(io_layout) bind(C)

    implicit none
    integer, intent(in) :: io_layout(2)

    call fms_mpp_domains_define_io_domain(Domain2D, io_layout)

  end subroutine cFMS_define_io_domain2D


  subroutine cFMS_set_compute_domain2D(xbegin, xend, ybegin, yend, xsize, ysize, &
                                       x_is_global, y_is_global, tile_count) bind(C)

    implicit none

    integer, intent(in), optional :: xbegin, xend, ybegin, yend, xsize, ysize
    logical, intent(in), optional :: x_is_global, y_is_global
    integer, intent(in), optional :: tile_count
    
    call fms_mpp_domains_set_compute_domain(Domain2D, xbegin, xend, ybegin, yend, xsize, ysize, &
                                            x_is_global, y_is_global, tile_count)

  end subroutine cFMS_set_compute_domain2D


  subroutine cFMS_set_data_domain2D(xbegin, xend, ybegin, yend, xsize, ysize, &
                                    x_is_global, y_is_global, tile_count) bind(C)
 
    implicit none

    integer, intent(in), optional :: xbegin, xend, ybegin, yend, xsize, ysize
    logical, intent(in), optional :: x_is_global, y_is_global
    integer, intent(in), optional :: tile_count

    call fms_mpp_domains_set_data_domain(Domain2D, xbegin, xend, ybegin, yend, xsize, ysize, &
                                         x_is_global, y_is_global, tile_count)

  end subroutine cFMS_set_data_domain2D


  subroutine cFMS_set_global_domain2D(xbegin, xend, ybegin, yend, xsize, ysize, tile_count) bind(C)

    implicit none

    integer, intent(in), optional :: xbegin, xend, ybegin, yend, xsize, ysize
    integer, intent(in), optional :: tile_count
    integer                        :: tile

    call fms_mpp_domains_set_global_domain(Domain2D, xbegin, xend, ybegin, yend, xsize, ysize, tile_count)

  end subroutine cFMS_set_global_domain2D
  
  
end module cFMS_mod
