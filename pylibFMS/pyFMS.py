#!/usr/bin/env python3

import ctypes as c
import numpy as np
import dataclasses
import os
from pyFMS_utils import *

#TODO:  get localcomm from pace
#TODO:  define domain via GFDL_atmos_cubed_sphere/tools/fv_mp_mod


def fms_init( pylibFMS: c.CDLL=None, localcomm: int=None, alt_input_nml_path: str="input/input.nml") :

    _fms_init = pylibFMS.cfms_fms_init

    localcomm_p, localcomm_t = setscalar_Cint32(localcomm)

    alt_input_nml_path_p, alt_input_nml_path_t = set_Cchar(alt_input_nml_path)
    
    _fms_init.argtypes = [ localcomm_t, alt_input_nml_path_t ]
    _fms_init.restype  = None
    
    _fms_init( localcomm_p, alt_input_nml_path_p )


@dataclasses.dataclass
class FMS() :

    pylibFMS_path      : str = None
    pylibFMS           : c.CDLL = None
    alt_input_nml_path : str = "input/input.nml"
    localcomm          : int = None    
    
    def __post_init__(self) :

        if self.pylibFMS_path == None :
            raise ValueError("Please define the library file path, e.g., as  libFMS(pylibFMS_path=./pylibFMS.so)")

        if not os.path.isfile(self.pylibFMS_path) :
            raise ValueError(f"Library {self.pylibFMS_path} does not exist")

        if self.pylibFMS == None : self.pylibFMS = c.cdll.LoadLibrary(self.pylibFMS_path)
        
        fms_init(self.pylibFMS, self.localcomm, self.alt_input_nml_path)
        
        
    def define_domains2d(self, global_indices, layout, pelist=[None],
                         xflags=None, yflags=None, xhalo=None, yhalo=None, xextent=[None], yextent=[None],
                         maskmap=[None], name=None, symmetry=None, memory_size=[None],
                         whalo=None, ehalo=None, shalo=None, nhalo=None, is_mosaic=None, tile_count=None,
                         tile_id=None, complete=None, x_cyclic_offset=None, y_cyclic_offset=None) :

        _mpp_define_domains2d = self.pylibFMS.cfms_define_domains2d

        global_indices_p, global_indices_t = setarray_Cint32(global_indices)
        layout_p, layout_t = setarray_Cint32(layout)
        
        pelist_p, pelist_t = setarray_Cint32(pelist)    #array(:)
        xflags_p, xflags_t = setscalar_Cint32(xflags)
        yflags_p, yflags_t = setscalar_Cint32(xflags)
        xhalo_p, xhalo_t = setscalar_Cint32(xflags)
        yhalo_p, yhalo_t = setscalar_Cint32(yflags)
        xextent_p, xextent_t = setarray_Cint32(xextent) #array(:)
        yextent_p, yextent_t = setarray_Cint32(yextent) #array(:)
        maskmap_p, maskmap_t = setarray_Cbool(maskmap)  #array(:)
        name_p, name_t       = set_Cchar(name)
        symmetry_p, symmetry_t       = setscalar_Cbool(symmetry)
        memory_size_p, memory_size_t = setarray_Cint32(memory_size) #array(:,:)
        whalo_p, whalo_t = setscalar_Cint32(whalo)
        ehalo_p, ehalo_t = setscalar_Cint32(ehalo)
        shalo_p, shalo_t = setscalar_Cint32(shalo)
        nhalo_p, nhalo_t = setscalar_Cint32(nhalo)
        is_mosaic_p, is_mosaic_t   = setscalar_Cbool(is_mosaic)
        tile_count_p, tile_count_t = setscalar_Cint32(tile_count)
        tile_id_p, tile_id_t   = setscalar_Cint32(tile_id)
        complete_p, complete_t = setscalar_Cbool(complete)
        x_cyclic_offset_p, x_cyclic_offset_t = setscalar_Cint32(x_cyclic_offset)
        y_cyclic_offset_p, y_cyclic_offset_t = setscalar_Cint32(y_cyclic_offset)    

        npelist,  npelist_t  = set_sizevars(pelist,1)
        nxextent, nxextent_t = set_sizevars(xextent,1)
        nyextent, nyextent_t = set_sizevars(yextent,1)
        nmaskmap, nmaskmap_t = set_sizevars(maskmap,2)        
        nmemory_size, nmemory_size_t = set_sizevars(memory_size,1)

        _mpp_define_domains2d.argtypes = [ global_indices_t, layout_t, *npelist_t, *nxextent_t, *nyextent_t,
                                           *nmaskmap_t, *nmemory_size_t, pelist_t, xflags_t, yflags_t,
                                           xhalo_t, yhalo_t, xextent_t, yextent_t, maskmap_t, name_t, symmetry_t,
                                           memory_size_t, whalo_t, ehalo_t, shalo_t, nhalo_t, is_mosaic_t, tile_count_t,
                                           tile_id_t, complete_t, x_cyclic_offset_t, y_cyclic_offset_t ]

        _mpp_define_domains2d.restype = None
        
        _mpp_define_domains2d( global_indices_p, layout_p, *npelist, *nxextent, *nyextent,
                               *nmaskmap, *nmemory_size, pelist_p, xflags_p, yflags_p,
                               xhalo_p, yhalo_p, xextent_p, yextent_p, maskmap_p, name_p, symmetry_p,
                               memory_size_p, whalo_p, ehalo_p, shalo_p, nhalo_p, is_mosaic_p, tile_count_p,
                               tile_id_p, complete_p, x_cyclic_offset_p, y_cyclic_offset_p )


    def define_io_domain2D(self, io_layout) :

        _define_io_domain2D = self.pylibFMS.cfms_define_io_domain2d

        io_layout_p, io_layout_t = setarray_Cint32(io_layout)

        _define_io_domain2D.argtypes = [ io_layout_t ]
        _define_io_domain2D.restype = None

        _define_io_domain2D(io_layout_p)

    def set_compute_domain2D(self, xbegin=None, xend=None, ybegin=None, yend=None,
                             xsize=None, ysize=None, x_is_global=None, y_is_global=None, tile_count=None) :

        _set_compute_domain2D = self.pylibFMS.cfms_set_compute_domain2d
        
        xbegin_p, xbegin_t = setscalar_Cint32(xbegin)
        xend_p,   xend_t   = setscalar_Cint32(xend)
        ybegin_p, ybegin_t = setscalar_Cint32(ybegin)
        yend_p,   yend_t   = setscalar_Cint32(yend)
        xsize_p, xsize_t   = setscalar_Cint32(xsize)
        ysize_p, ysize_t   = setscalar_Cint32(ysize)
        x_is_global_p, x_is_global_t = setscalar_Cbool(x_is_global)
        y_is_global_p, y_is_global_t = setscalar_Cbool(y_is_global)

        _set_compute_domain2D.argtypes = [xbegin_t, xend_t, ybegin_t, yend_t,
                                          xsize_t, ysize_t, x_is_global_t, y_is_global_t]
        _set_compute_domain2D.restype = None

        _set_compute_domain2D(xbegin_p, xend_p, ybegin_p, yend_p,
                              xsize_p, xsize_p, ysize_p, x_is_global_p, y_is_global_p)


    def set_data_domain2D(self, xbegin=None, xend=None, ybegin=None, yend=None,
                          xsize=None, ysize=None, x_is_global=None, y_is_global=None, tile_count=None) :

        _set_data_domain2D = self.pylibFMS.cfms_set_data_domain2d
        
        xbegin_p, xbegin_t = setscalar_Cint32(xbegin)
        xend_p,   xend_t   = setscalar_Cint32(xend)
        ybegin_p, ybegin_t = setscalar_Cint32(ybegin)
        yend_p,   yend_t   = setscalar_Cint32(yend)
        xsize_p,  xsize_t  = setscalar_Cint32(xsize)
        ysize_p,  ysize_t  = setscalar_Cint32(ysize)
        x_is_global_p, x_is_global_t = setscalar_Cbool(x_is_global)
        y_is_global_p, y_is_global_t = setscalar_Cbool(y_is_global)

        _set_data_domain2D.argtypes = [xbegin_t, xend_t, ybegin_t, yend_t,
                                          xsize_t, ysize_t, x_is_global_t, y_is_global_t]
        _set_data_domain2D.restype = None

        _set_data_domain2D(xbegin_p, xend_p, ybegin_p, yend_p,
                           xsize_p, xsize_p, ysize_p, x_is_global_p, y_is_global_p)


    def set_global_domain2D(self, xbegin=None, xend=None, ybegin=None, yend=None,
                            xsize=None, ysize=None, tile_count=None) :

        _set_global_domain2D = self.pylibFMS.cfms_set_global_domain2d

        xbegin_p, xbegin_t = setscalar_Cint32(xbegin)
        xend_p,   xend_t   = setscalar_Cint32(xend)
        ybegin_p, ybegin_t = setscalar_Cint32(ybegin)
        yend_p,   yend_t   = setscalar_Cint32(yend)
        xsize_p,  xsize_t  = setscalar_Cint32(xsize)
        ysize_p,  ysize_t  = setscalar_Cint32(ysize)
        tile_count_p, tile_count_t = setscalar_Cint32(tile_count)

        _set_global_domain2D.argtype = [ xbegin_t, xend_t, ybegin_t, yend_t,
                                         xsize_t, ysize_t, tile_count_t ]
        _set_global_domain2D.restype = None

        _set_global_domain2D(xbegin_p, xend_p, ybegin_p, yend_p, xsize_p, ysize_p, tile_count_p)


    def define_nest_domain(self, num_nest, nest_level, tile_fine, tile_coarse,                               
                           istart_coarse, icount_coarse, jstart_coarse, jcount_coarse, npes_nest_tile, 
                           x_refine, y_refine, extra_halo=None, name=None) :

        _define_nest_domain = self.pylibFMS.cfms_define_nest_domain

        num_nest_p, num_nest_t = setscalar_Cint32(num_nest)
        nest_level_p, nest_level_t = setarray_Cint32(nest_level)
        tile_fine_p, tile_fine_t = setarray_Cint32(tile_fine)
        tile_coarse_p, tile_coarse_t = setarray_Cint32(tile_coarse)
        istart_coarse_p, istart_coarse_t = setarray_Cint32(istart_coarse)
        icount_coarse_p, icount_coarse_t = setarray_Cint32(icount_coarse)
        jstart_coarse_p, jstart_coarse_t = setarray_Cint32(jstart_coarse)
        jcount_coarse_p, jcount_coarse_t = setarray_Cint32(jcount_coarse)
        npes_nest_tile_p, npes_nest_tile_t = setarray_Cint32(npes_nest_tile)
        x_refine_p, x_refine_t = setarray_Cint32(x_refine)
        y_refine_p, y_refine_t = setarray_Cint32(y_refine)
        extra_halo_p, extra_halo_t = setscalar_Cint32(extra_halo)
        name_p, name_t = set_Cchar(name)

        _define_nest_domain.argtype = [ num_nest_t, nest_level_t, tile_file_t, tile_coarse_t,
                                        istart_coarse_t, icount_coarse_t, jstart_coarse_t, jcount_coarse_t,
                                        npes_nest_tile_t, x_refine_t, y_refine_t, extra_halo_t, name_t ]
        _define_nest_domain.restype = None

        _define_nest_domain(num_nest_p, nest_level_p, tile_fine_p, tile_coarse_p,
                            istart_coarse_p, icount_coarse_p, jstart_coarse_p, jcount_coarse_p,
                            npes_nest_tile_p, x_refine_p, y_refine_p, extra_halo_p, name_p)
                                        


        
    
    
    
