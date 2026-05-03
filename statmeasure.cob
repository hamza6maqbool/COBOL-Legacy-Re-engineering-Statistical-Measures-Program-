      *> statmeasure.cob
      *> Modern re-engineering of statmold.cob.
      *>
      *> Reads a plain text file of floating-point numbers (one per
      *> line, no padding or sentinel required) and computes five
      *> statistical measures:
      *>
      *>   Measures of central tendency:
      *>     - Arithmetic Mean
      *>     - Geometric Mean  (GM)
      *>     - Harmonic Mean   (HM)
      *>
      *>   Measures of dispersion:
      *>     - Standard Deviation (SD)
      *>     - Root Mean Square   (RMS)
      *>
      *> Results are written to standard output (screen).
      *>
      *> Compile: cobc -free -x -Wall statmeasure.cob
      *> Run:     ./statmeasure

       identification division.
       program-id. statmeasure.

      *> --------------------------------------------------------
      *> Environment Division
      *> --------------------------------------------------------
       environment division.
       input-output section.
       file-control.
           select input-file
               assign to dynamic ws-filename
               organization is line sequential
               file status is ws-file-status.

      *> --------------------------------------------------------
      *> Data Division
      *> --------------------------------------------------------
       data division.
       file section.

      *> Each input record is a plain text line up to 40 chars.
      *> Values are stored as floating-point text (e.g. " 130.37").
       fd  input-file.
       01  input-record              pic x(40).

       working-storage section.

      *> --- File handling ---
       77  ws-filename               pic x(80).
       77  ws-file-status            pic xx value spaces.

      *> --- Loop counter and array size ---
       77  ws-n                      pic 9(4)   value 0.
       77  ws-i                      pic 9(4)   value 0.

      *> --- Raw value read from each input line ---
       77  ws-input-val              pic s9(6)v9(2).

      *> --- Data array: holds up to 1000 values ---
       01  ws-data-array.
           02  ws-x                  pic s9(6)v9(2) occurs 1000 times.

      *> --- Accumulators for the five statistics ---
       77  ws-sum                    pic s9(12)v9(6) value 0.
       77  ws-sum-sq-dev             pic  9(14)v9(6) value 0.
       77  ws-sum-sq                 pic  9(14)v9(6) value 0.
       77  ws-sum-recip              pic  9(8)v9(8)  value 0.
       77  ws-log-sum                pic s9(8)v9(8)  value 0.

      *> --- Computed results ---
       77  ws-mean                   pic s9(8)v9(4).
       77  ws-std-dev                pic  9(8)v9(4).
       77  ws-geo-mean               pic  9(8)v9(4).
       77  ws-harm-mean              pic  9(8)v9(4).
       77  ws-rms                    pic  9(8)v9(4).

      *> --- Intermediate geometric mean computation ---
       77  ws-log-val                pic s9(8)v9(8).
       77  ws-avg-log                pic s9(8)v9(8).

      *> --- Formatted output fields (screen display) ---
       77  ws-out-val                pic -zzz9.99.
       77  ws-out-mean               pic -zzz9.99.
       77  ws-out-sd                 pic  zzz9.99.
       77  ws-out-gm                 pic  zzz9.99.
       77  ws-out-hm                 pic  zzz9.99.
       77  ws-out-rms                pic  zzz9.99.
       77  ws-out-sum                pic -zzzz9.99.

      *> --- Display separator line ---
       77  ws-sep                    pic x(40)
           value "----------------------------------------".

      *> --------------------------------------------------------
      *> Procedure Division
      *> --------------------------------------------------------
       procedure division.

           perform get-filename.
           perform open-input-file.
           perform read-all-values.
           perform close-input-file.
           perform calc-arithmetic-mean.
           perform calc-std-deviation.
           perform calc-geometric-mean.
           perform calc-harmonic-mean.
           perform calc-rms.
           perform display-results.
           stop run.

      *> --------------------------------------------------------
      *> get-filename: prompt user for the input file name.
      *> --------------------------------------------------------
       get-filename.
           display ws-sep.
           display "  STATISTICAL MEASURES CALCULATOR".
           display ws-sep.
           display " Enter input file name: " with no advancing.
           accept ws-filename.

      *> --------------------------------------------------------
      *> open-input-file: open the user-specified file,
      *>   abort with a message on failure.
      *> --------------------------------------------------------
       open-input-file.
           open input input-file.
           if ws-file-status not equal "00"
               display "ERROR: Cannot open file [" ws-filename "]"
               display "File status: " ws-file-status
               stop run
           end-if.

      *> --------------------------------------------------------
      *> read-all-values: read every line into the data array.
      *>   EOF ends the loop naturally - no sentinel needed.
      *> --------------------------------------------------------
       read-all-values.
           move 0 to ws-n.
           perform until exit
               read input-file into ws-input-val
                   at end exit perform
               end-read

               if ws-file-status not equal "00"
                   display "File read error: " ws-file-status
                   stop run
               end-if

               add 1 to ws-n
               move ws-input-val to ws-x(ws-n)

               if ws-n >= 1000
                   display "Warning: data limit of 1000 reached."
                   exit perform
               end-if
           end-perform.

           if ws-n = 0
               display "ERROR: No data values found in file."
               stop run
           end-if.

      *> --------------------------------------------------------
      *> close-input-file: close the input file after reading.
      *> --------------------------------------------------------
       close-input-file.
           close input-file.

      *> --------------------------------------------------------
      *> calc-arithmetic-mean: sum all values, divide by count.
      *> --------------------------------------------------------
       calc-arithmetic-mean.
           move 0 to ws-sum.
           perform varying ws-i from 1 by 1
               until ws-i > ws-n
               compute ws-sum = ws-sum + ws-x(ws-i)
           end-perform.
           compute ws-mean rounded = ws-sum / ws-n.

      *> --------------------------------------------------------
      *> calc-std-deviation: population standard deviation.
      *>   SD = sqrt( sum((x - mean)^2) / n )
      *> --------------------------------------------------------
       calc-std-deviation.
           move 0 to ws-sum-sq-dev.
           perform varying ws-i from 1 by 1
               until ws-i > ws-n
               compute ws-sum-sq-dev = ws-sum-sq-dev
                   + (ws-x(ws-i) - ws-mean) ** 2
           end-perform.
           compute ws-std-dev rounded =
               (ws-sum-sq-dev / ws-n) ** 0.5.

      *> --------------------------------------------------------
      *> calc-geometric-mean: nth root of the product of all values.
      *>   Computed via logarithms to avoid overflow:
      *>   GM = exp( (1/n) * sum(ln(x)) )
      *>
      *>   Note: requires all values > 0.
      *> --------------------------------------------------------
       calc-geometric-mean.
           move 0 to ws-log-sum.
           perform varying ws-i from 1 by 1
               until ws-i > ws-n
               if ws-x(ws-i) <= 0
                   display "WARNING: Geometric mean requires "
                       "all values > 0. Skipping GM."
                   move 0 to ws-geo-mean
                   exit perform
               end-if
               compute ws-log-val =
                   function log(ws-x(ws-i))
               compute ws-log-sum = ws-log-sum + ws-log-val
           end-perform.
           compute ws-avg-log = ws-log-sum / ws-n.
           compute ws-geo-mean rounded =
               function exp(ws-avg-log).

      *> --------------------------------------------------------
      *> calc-harmonic-mean: n divided by sum of reciprocals.
      *>   HM = n / (1/x1 + 1/x2 + ... + 1/xn)
      *>
      *>   Note: requires all values > 0.
      *> --------------------------------------------------------
       calc-harmonic-mean.
           move 0 to ws-sum-recip.
           perform varying ws-i from 1 by 1
               until ws-i > ws-n
               if ws-x(ws-i) <= 0
                   display "WARNING: Harmonic mean requires "
                       "all values > 0. Skipping HM."
                   move 0 to ws-harm-mean
                   exit perform
               end-if
               compute ws-sum-recip =
                   ws-sum-recip + (1 / ws-x(ws-i))
           end-perform.
           compute ws-harm-mean rounded = ws-n / ws-sum-recip.

      *> --------------------------------------------------------
      *> calc-rms: root mean square.
      *>   RMS = sqrt( (x1^2 + x2^2 + ... + xn^2) / n )
      *> --------------------------------------------------------
       calc-rms.
           move 0 to ws-sum-sq.
           perform varying ws-i from 1 by 1
               until ws-i > ws-n
               compute ws-sum-sq =
                   ws-sum-sq + ws-x(ws-i) ** 2
           end-perform.
           compute ws-rms rounded = (ws-sum-sq / ws-n) ** 0.5.

      *> --------------------------------------------------------
      *> display-results: print the data values and all statistics
      *>   to standard output in a clean, readable format.
      *> --------------------------------------------------------
       display-results.
           display " ".
           display ws-sep.
           display "  INPUT FILE : " function trim(ws-filename).
           display "  DATA COUNT : " ws-n " values".
           display ws-sep.
           display "  DATA VALUES".
           display ws-sep.

           perform varying ws-i from 1 by 1
               until ws-i > ws-n
               move ws-x(ws-i) to ws-out-val
               display "  " ws-out-val
           end-perform.

           move ws-sum  to ws-out-sum.
           move ws-mean to ws-out-mean.
           move ws-std-dev  to ws-out-sd.
           move ws-geo-mean to ws-out-gm.
           move ws-harm-mean to ws-out-hm.
           move ws-rms  to ws-out-rms.

           display ws-sep.
           display "  MEASURES OF CENTRAL TENDENCY".
           display ws-sep.
           display "  Sum              = " ws-out-sum.
           display "  Arithmetic Mean  = " ws-out-mean.
           display "  Geometric Mean   = " ws-out-gm.
           display "  Harmonic Mean    = " ws-out-hm.
           display ws-sep.
           display "  MEASURES OF DISPERSION".
           display ws-sep.
           display "  Standard Dev     = " ws-out-sd.
           display "  Root Mean Square = " ws-out-rms.
           display ws-sep.
