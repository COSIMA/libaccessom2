# cmake version, project name, language
cmake_minimum_required(VERSION 3.5 FATAL_ERROR)
project(matm Fortran)

# set output paths for modules, archives, and executables
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/include)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# if build type not specified, default to release
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE "release")
endif()

# MPI settings
find_package(MPI REQUIRED)
add_definitions(${MPI_Fortran_COMPILE_FLAGS})
include_directories(${MPI_Fortran_INCLUDE_PATH})
link_directories(${MPI_Fortran_LIBRARIES})

# compiler flags for gfortran
if(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
  set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -C -fbacktrace -std=f2008")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -std=f2008")
endif()

# compiler flags for ifort
if(CMAKE_Fortran_COMPILER_ID MATCHES Intel)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -assume realloc_lhs -heap-arrays")
  set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -C -traceback")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
endif()

include_directories(contrib/datetime-fortran/build/include contrib/json-fortran/lib contrib/oasis3-mct/Linux/build/lib/psmile.MPI1/)

# libcouple.a library
file(GLOB LIB_SOURCES libcouple/src/*.F90)
add_library(couple ${LIB_SOURCES})

# atm executable
file(GLOB ATM_SOURCES atm/src/*.F90)
add_executable(atm ${ATM_SOURCES})
target_link_libraries(atm couple)

# tests
# enable_testing()
# add_executable(matm_tests tests/matm_tests.F90)
# target_link_libraries(matm_tests matm)
# add_test(datetime_tests bin/datetime_tests)
