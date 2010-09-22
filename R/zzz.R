
.First.lib <- function(lib, pkg) {
	library.dynam("mfr",pkg,lib)
    gver <- read.dcf(file=system.file("DESCRIPTION", package=pkg), 
                      fields="Version")
    gdate <- read.dcf(file=system.file("DESCRIPTION", package=pkg), 
                      fields="Date")
	 cat(pkg,"version",gver,"\nDate:",gdate,"\n") 
}
