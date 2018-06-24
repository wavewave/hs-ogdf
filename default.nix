{ stdenv, fetchgit, cmake, doxygen }:

stdenv.mkDerivation rec {
 
  name = "ogdf-${version}";
  version = "20180328";
  src = fetchgit {
    url = "http://github.com/wavewave/ogdf.git";
    rev = "9baf3646ee745249e3bef0499f341474d2fb9a83";
    sha256 = "09dalmavk6b69zv4py2pbmlviaw903dzny42jqkwf5la408vc8yv";
  };

  hardeningDisable = [ "all" ];
  nativeBuildInputs = [ cmake doxygen ];
  cmakeFlags = [ "-DOGDF_WARNING_ERRORS=OFF" ];

}