# Platform specific makefile for HIP code, on linux with hipcc (ROCm) installed
#
# this is included from main.mk -file, which is in turn included from 
# application makefile
#
#


CC := hipcc


GPU_ARCH=gfx1030
LD := $(CC) -O3 --std=c++17 --amdgpu-target=${GPU_ARCH} -fgpu-rdc --hip-link

CXXFLAGS := -O3 --std=c++17 -x hip -fgpu-rdc --hip-link#-nogpulib
# 20050 is a warning about ignored inline in __global__ functions - it's not ignored though, it allows multiple
# definitions as per c++ standard!
# CXXFLAGS += -Xcudafe "--display_error_number --diag_suppress=177 --diag_suppress=20050"
#CXXFLAGS = -g -x c++ --std=c++17 


LDLIBS := -lm -L/opt/rocm-5.3.0/hiprand/lib -L/opt/rocm-5.3.0/hipfft/lib -L/opt/rocm-5.3.0/hipcub/lib

# Need to give include directory to mpi for hilapp and nvcc - here 2 common ones
# This in general works with OpenMPI: --showme:incdirs gives the include path of the mpic++
# MPI_INCLUDE_DIRS := -I/opt/hpcx/hpcx-v2.6.0-gcc-MLNX_OFED_LINUX-5.0-1.0.0.0-ubuntu18.04-x86_64/ompi/include
MPI_INCLUDE_DIRS := -I/usr/lib/x86_64-linux-gnu/openmpi/include -I/usr/lib/openmpi/include
MPI_LIBS := -L/usr/lib/openmpi/lib -lmpi

LDLIBS += -lrocrand -lrocfft -lhipfft -lhiprand $(MPI_LIBS)

HIP_PATH ?= $(shell hipconfig --path)
HIP_INCLUDE_DIRS := -I$(HIP_PATH)/include -I$(HIP_PATH)/hiprand/include -I$(HIP_PATH)/hipfft/include -I$(HIP_PATH)/rocrand/include -I$(HIP_PATH)/rocfft/include -I$(HIP_PATH)/hipcub/include
#HIP_INCLUDE_DIRS += -I$(HIP_PATH)/rocrand/include -I$(HIP_PATH)/rocfft/include

# extra cuda objects here
HILA_OBJECTS += build/hila_gpu.o build/memory_pool.o

################

# These variables must be defined here
#
HILAPP_OPTS := -target:HIP -D__HIP_PLATFORM_AMD__  $(STD_HILAPP_INCLUDES) $(HIP_INCLUDE_DIRS)
HILA_OPTS := -DHIP $(MPI_INCLUDE_DIRS) $(HIP_INCLUDE_DIRS)