{ stdenv, fetchurl, fetchFromGitHub, cmake, pkgconfig,
  cudatoolkit, opencl-clhpp, ocl-icd, fftw, fftwFloat, mkl,
  blas, openblas, liblapack, boost, mesa_noglu, libGLU_combined,
  freeimage, python, doxygen
}:

with {
  clfftSource = fetchFromGitHub {
    owner  = "arrayfire";
    repo   = "clFFT";
    rev    = "16925fb93338b3cac66490b5cf764953d6a5dac7";
    sha256 = "0y35nrdz7w4n1l17myhkni3hwm37z775xn6f76xmf1ph7dbkslsc";
    fetchSubmodules = true;
  };

  clblasSource = fetchFromGitHub {
    owner  = "arrayfire";
    repo   = "clBLAS";
    rev    = "1f3de2ae5582972f665c685b18ef0df43c1792bb";
    sha256 = "154mz52r5hm0jrp5fqrirzzbki14c1jkacj75flplnykbl36ibjs";
    fetchSubmodules = true;
  };

  cl2hppSource = fetchurl {
    url    = "https://github.com/KhronosGroup/OpenCL-CLHPP/releases/download/v2.0.10/cl2.hpp";
    sha256 = "1v4q0g6b6mwwsi0kn7kbjn749j3qafb9r4ld3zdq1163ln9cwnvw";
  };

  lapack = liblapack.overrideAttrs (old: {
    cmakeFlags = (old.cmakeFlags or []) ++ [
      "-DLAPACKE=ON"
      "-DCBLAS=ON"
    ];
  });
};

stdenv.mkDerivation {
  name = "arrayfire-3.6.1";

  src = fetchFromGitHub {
    owner  = "arrayfire";
    repo   = "arrayfire";
    rev    = "25bb360659b091bbca711b463c0ad5f0cf818e9c";
    sha256 = "1d0fcwgg9dicbhwp4shmddqjbjcd05w7x5j1cx2cd4j5iwwg1q50";
    fetchSubmodules = true;
  };

  cmakeFlags = [
    "-DAF_BUILD_OPENCL=OFF"
    "-DAF_BUILD_EXAMPLES=OFF"
    "-DBUILD_TESTING=OFF"
    "-DCMAKE_LIBRARY_PATH=${cudatoolkit}/lib/stubs"
  ];

  # cmakeFlags = [
  #   "-DLAPACK_INCLUDE_DIR=${lapack}/include"
  #   "-DLAPACK_LIBRARIES=${lapack}/lib/liblapacke.a"
  #   "-DCBLAS_INCLUDE_DIR=${lapack}/include"
  #   "-DCBLAS_LIBRARIES=${lapack}/lib/libcblas.a"
  # ];

  patches = [ ./no-download.patch ];

  postPatch = ''
    mkdir -p ./build/third_party/clFFT/src
    cp -R --no-preserve=mode,ownership ${clfftSource}/ ./build/third_party/clFFT/src/clFFT-ext/
    mkdir -p ./build/third_party/clBLAS/src
    cp -R --no-preserve=mode,ownership ${clblasSource}/ ./build/third_party/clBLAS/src/clBLAS-ext/
    mkdir -p ./build/include/CL
    cp -R --no-preserve=mode,ownership ${cl2hppSource} ./build/include/CL/cl2.hpp
  '';

  preBuild = ''
    export CUDA_PATH="${cudatoolkit}"
  '';

  enableParallelBuilding = true;

  buildInputs = [
    cmake pkgconfig
    cudatoolkit
    opencl-clhpp ocl-icd fftw fftwFloat
    lapack
    mkl
    openblas
    libGLU_combined
    mesa_noglu freeimage
    boost.out boost.dev python
    #doxygen
  ];
}
