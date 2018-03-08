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

if("${NETCDF_DIR}" STREQUAL "")
  execute_process(COMMAND nf-config --prefix OUTPUT_VARIABLE NETCDF_DIR
    OUTPUT_STRIP_TRAILING_WHITESPACE)
endif()
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}")
set (NETCDF_F90 "YES")
find_package(NetCDF REQUIRED)
include_directories(${NETCDF_F90_INCLUDE_DIRS})

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

# get external projects
include(ExternalProject)
ExternalProject_Add(datetime-fortran
    GIT_REPOSITORY    https://github.com/wavebitscientific/datetime-fortran.git
    GIT_TAG          914d21e497a29a85c9ac6ec8c269aeada244d285
    INSTALL_COMMAND cmake -E echo "Skipping datetime-fortran install step."
)
ExternalProject_Get_property(datetime-fortran BINARY_DIR)
include_directories(${BINARY_DIR}/include)
link_directories(${BINARY_DIR}/lib)

ExternalProject_Add(json-fortran
    GIT_REPOSITORY    https://github.com/jacobwilliams/json-fortran.git
    GIT_TAG          37de3ade555e84779ebf8a26c23e86a3ae268390
    INSTALL_COMMAND cmake -E echo "Skipping json-fortran install step."
)
ExternalProject_Get_property(json-fortran BINARY_DIR)
include_directories(${BINARY_DIR}/include)
link_directories(${BINARY_DIR}/lib)

include_directories(contrib/oasis3-mct/Linux/build/lib/psmile.MPI1/)

# libcouple.a library
file(GLOB LIB_SOURCES libcouple/src/*.F90)
add_library(couple ${LIB_SOURCES})

# atm executable
file(GLOB ATM_SOURCES atm/src/*.F90)
add_executable(atm ${ATM_SOURCES})
target_link_libraries(atm couple ${NETCDF_LIBRARIES} ${MPI_Fortran_LIBRARIES})

# ice stub executable
file(GLOB ICE_STUB_SOURCES ice_stub/src/*.F90)
add_executable(ice_stub ${ATM_SOURCES})
target_link_libraries(ice_stub couple ${NETCDF_LIBRARIES} ${MPI_Fortran_LIBRARIES})

# tests
# enable_testing()
# add_executable(matm_tests tests/matm_tests.F90)
# target_link_libraries(matm_tests matm)
# add_test(datetime_tests bin/datetime_tests)
