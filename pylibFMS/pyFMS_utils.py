#!/usr/bin/env python3                                                                                                           

import ctypes as c
import numpy as np
import dataclasses


def set_ndpointer(arg, c_type) :
    return np.ctypeslib.ndpointer( dtype=c_type, shape=np.shape(arg), flags='FORTRAN')

def setarray_Cint32(arg) :    
    if arg[0] == None :
        return None, c.POINTER(c.c_int)
    else :
        return arg, set_ndpointer(arg, c.c_int)

def setarray_Cfloat(arg) :
    if arg[0] == None :
        return None,  c.POINTER(c.c_float32)
    else :
        return arg, set_ndpointer(arg, c.c_float)
    return arg, arg_t


def setarray_Cdouble(arg) :
    if arg[0] == None :
        return None, c.POINTER(c.c_double)
    else :
        return arg, set_ndpointer(arg, c.c_double)
    
def set_Cchar(arg) :
    if arg == None :
        return arg, c.POINTER(c.c_char_p)
    else :
        return c.byref( c.c_char_p( arg.encode('ascii') ) ), c.POINTER(c.c_char_p)


def setarray_Cbool(arg) :
    if arg[0] == None :
        return None, c.POINTER(c.c_bool)
    else :
        return arg, set_ndpointer(arg, c.c_bool)


def setscalar_Cint32(arg) :
    if arg == None :
        return arg, c.POINTER(c.c_int)
    else :
        return c.byref(c.c_int(arg)), c.POINTER(c.c_int)


def setscalar_Cfloat(arg) :
    if arg == None :
        return arg, c.POINTER(c.c_float)
    else :
        return c.byref(c.c_float(arg)), c.POINTER(c.c_float)

    
def setscalar_Cdouble(arg) :
    if arg == None :
        return arg, c.POINTER(c.c_double)
    else :
        return c.byref(c.c_double(arg)), c.POINTER(c.c_double)


def setscalar_Cbool(arg) :
    if arg == None :
        return arg, c.POINTER(c.c_bool)
    else :
        return c.byref(c.c_bool(arg)), c.POINTER(c.c_bool)
    
    
def set_sizevars(arg, ndim) :
    if arg[0] == None :
        argsize = [ c.byref(c.c_int(1)) for i in range(ndim) ]
        argsize_t = [ c.POINTER(c.c_int) for i in range(ndim) ]
    else :
        argsize = [ c.byref(c.c_int(isize)) for isize in np.shape(arg) ]
        argsize_t = [ c.POINTER(c.c_int) for i in range(ndim) ]
    return argsize, argsize_t
