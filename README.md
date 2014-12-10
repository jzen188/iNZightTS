iNZightTS
=========

The time series functionality designed for use within iNZight. 
For more info see: http://www.stat.auckland.ac.nz/~wild/iNZight/

Remarks (as at 10/12/2014)
============================

1. You may find that clicking the 'Time Series Plot - Animate' yields a plot that flickers slightly. In that case, you 
   can try changing the number inside dev.hold() and dev.flush() within the drawImage() function in 'utils.R' from 1 to 
   say 2 or 3, and see if that makes any difference.
   
2. At present, the 'Time Series Plot - Animate' option calls a cross-platform Cairo() device (within the newdevice() function
   in 'utils.R', provided by the cairoDevice package. Support for this package may discontinue in the future versions of R -
   in which case, simply use X11(type = "cairo") instead.
   
(N.B. If you are still experiencing issues, please email Chris Park <cpar137@aucklanduni.ac.nz>)
