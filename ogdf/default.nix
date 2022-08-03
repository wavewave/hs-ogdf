{ stdenv, fetchgit, cmake, doxygen }:

stdenv.mkDerivation rec {
  name = "ogdf-${version}";
  version = "2022.02";
  src = fetchgit {
    url = "http://github.com/ogdf/ogdf.git";
    # dogwood-202202
    rev = "7596a518d2b8a964523c2bbd86af360aa405c6cf";
    sha256 = "sha256-zkQ6sS0EUmiigv3T7To+tG3XbFbR3XEbFo15oQ0bWf0=";
  };

  hardeningDisable = [ "all" ];
  nativeBuildInputs = [ cmake doxygen ];
  cmakeFlags = [ "-DOGDF_WARNING_ERRORS=OFF"
                 "-DBUILD_SHARED_LIBS=ON"
                 "-DCMAKE_CXX_FLAGS=-fPIC"
               ];
}
