module cFMS_mod

  use FMS, only : fms_init, fms_mpp_domains_init, fms_string_utils_c2f_string
  use FMS, only : FmsMppDomain2D, FmsMppDomainsNestDomain_type
  use FMS, only : fms_mpp_domains_define_domains, fms_mpp_domains_define_io_domain
  use FMS, only : fms_mpp_domains_set_compute_domain, fms_mpp_domains_set_data_domain, fms_mpp_domains_set_global_domain
  use FMS, only : fms_mpp_domains_define_nest_domains
  
  use iso_c_binding

  implicit none
  private
  
  public :: cFMS_fms_init
  public :: cFMS_get_domain
  public :: cFMS_define_domains2D, cFMS_define_io_domain2D
  public :: cFMS_set_compute_domain2D, cFMS_set_data_domain2D, cFMS_set_global_domain2D
  public :: cFMS_define_nest_domain
  
  character(100), parameter :: input_nml_path="./input.nml"
  
  type(FmsMppDomain2D), public :: domain
  type(FmsMppDomainsNestDomain_type), public :: nest_domain
  
contains

  !> returns module Domain 
  function cFMS_get_domain()
    type(FmsMppDomain2D) :: cFMS_get_domain
    cFMS_get_domain = domain
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
    
    call fms_mpp_domains_define_domains(global_indices, layout, domain, pelist, xflags, yflags, &
         xhalo, yhalo, xextent, yextent, maskmap, name, symmetry,  memory_size,                 &
         whalo, ehalo, shalo, nhalo, is_mosaic, tile_count, tile_id, complete, x_cyclic_offset, y_cyclic_offset)

  end subroutine cFMS_define_domains2D


  subroutine cFMS_define_io_domain2D(io_layout) bind(C)

    implicit none
    integer, intent(in) :: io_layout(2)

    call fms_mpp_domains_define_io_domain(domain, io_layout)

  end subroutine cFMS_define_io_domain2D


  subroutine cFMS_set_compute_domain2D(xbegin, xend, ybegin, yend, xsize, ysize, &
                                       x_is_global, y_is_global, tile_count) bind(C)

    implicit none

    integer, intent(in), optional :: xbegin, xend, ybegin, yend, xsize, ysize
    logical, intent(in), optional :: x_is_global, y_is_global
    integer, intent(in), optional :: tile_count
    
    call fms_mpp_domains_set_compute_domain(domain, xbegin, xend, ybegin, yend, xsize, ysize, &
                                            x_is_global, y_is_global, tile_count)

  end subroutine cFMS_set_compute_domain2D


  subroutine cFMS_set_data_domain2D(xbegin, xend, ybegin, yend, xsize, ysize, &
                                    x_is_global, y_is_global, tile_count) bind(C)
 
    implicit none

    integer, intent(in), optional :: xbegin, xend, ybegin, yend, xsize, ysize
    logical, intent(in), optional :: x_is_global, y_is_global
    integer, intent(in), optional :: tile_count

    call fms_mpp_domains_set_data_domain(domain, xbegin, xend, ybegin, yend, xsize, ysize, &
                                         x_is_global, y_is_global, tile_count)

  end subroutine cFMS_set_data_domain2D


  subroutine cFMS_set_global_domain2D(xbegin, xend, ybegin, yend, xsize, ysize, tile_count) bind(C)

    implicit none

    integer, intent(in), optional :: xbegin, xend, ybegin, yend, xsize, ysize
    integer, intent(in), optional :: tile_count
    integer                        :: tile

    call fms_mpp_domains_set_global_domain(domain, xbegin, xend, ybegin, yend, xsize, ysize, tile_count)

  end subroutine cFMS_set_global_domain2D
  

  subroutine cFMS_define_nest_domain(num_nest, nest_level, tile_fine, tile_coarse,                               &
                                     istart_coarse, icount_coarse, jstart_coarse, jcount_coarse, npes_nest_tile, &
                                     x_refine, y_refine, extra_halo, name_ptr) bind(C)

    implicit none

    integer, intent(in) :: num_nest
    integer, intent(in) :: nest_level(num_nest)
    integer, intent(in) :: tile_fine(num_nest)
    integer, intent(in) :: tile_coarse(num_nest)
    integer, intent(in) :: istart_coarse(num_nest)
    integer, intent(in) :: icount_coarse(num_nest)
    integer, intent(in) :: jstart_coarse(num_nest)
    integer, intent(in) :: jcount_coarse(num_nest)
    integer, intent(in) :: npes_nest_tile(6+num_nest) !fix this
    integer, intent(in) :: x_refine(num_nest)
    integer, intent(in) :: y_refine(num_nest)
    integer, intent(in), optional :: extra_halo
    type(c_ptr), intent(in), optional :: name_ptr

    character(100) :: name = "nest"    
    
    if(present(name_ptr)) name = fms_string_utils_c2f_string(name_ptr)

    call fms_mpp_domains_define_nest_domains(nest_domain, domain, num_nest, nest_level, tile_fine, tile_coarse, &
                                  istart_coarse, icount_coarse, jstart_coarse, jcount_coarse, npes_nest_tile,   &
                                  x_refine, y_refine, extra_halo, name)
    
  end subroutine cFMS_define_nest_domain
    
end module cFMS_mod
