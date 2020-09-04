.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Warning: Accessing the IRIS Warehouse requires that you are VPNed in with PulseSecure")
}
