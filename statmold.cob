      *> statmold.cob
      *> Original COBOL statistical measures program.
      *> Reads a fixed-width line-sequential file of numeric values
      *> and calculates the arithmetic mean and standard deviation,
      *> writing results to an output file.
      *>
      *> Compile: cobc -free -x -Wall statmold.cob
      *> Run:     ./statmold

       identification division.
       program-id. statmold.

      *> --------------------------------------------------------
      *> Environment Division: links logical file names to the
      *> physical files on disk.
      *> --------------------------------------------------------
       environment division.
       input-output section.
       file-control.
           select input-file
               assign to "testfile20.txt"
               organization is line sequential
               file status is ws-input-status.

           select output-file
               assign to "statmold_output.txt"
               organization is line sequential
               file status is ws-output-status.

      *> --------------------------------------------------------
      *> Data Division: file record layouts and working storage.
      *> --------------------------------------------------------
       data division.
       file section.

      *> Input file: each record is 80 columns.
      *> IN-X holds an 8-digit value with implied 2 decimal places,
      *> e.g. "   13037" is interpreted as 130.37.
       fd  input-file.
       01  input-record              pic x(80).

      *> Output file: variable-length text lines.
       fd  output-file.
       01  output-line               pic x(80).

       working-storage section.

      *> File status codes for I/O error detection.
       77  ws-input-status           pic xx value spaces.
       77  ws-output-status          pic xx value spaces.

       77  sum-of-x-sqr              pic 9(14)v9(2).
       77  sum-of-x                  pic s9(10)v9(2).
       77  n                         pic s9(4).
       77  mean                      pic s9(6)v9(2).
       77  i                         pic s9(4).

       01  array-area.
           02  x                     pic s9(6)v9(2) occurs 1000 times.

      *> Overlay used to extract the numeric field from the raw record.
       01  input-value-record.
           02  in-x                  pic s9(6)v9(2).
           02  filler                pic x(72).

       01  output-title-line.
           02  filler                pic x(28) value
               " MEAN AND STANDARD DEVIATION".

       01  output-underline.
           02  filler                pic x(28) value
               "----------------------------".

       01  output-col-heads.
           02  filler                pic x(10) value spaces.
           02  filler                pic x(11) value "DATA VALUES".

       01  output-data-line.
           02  filler                pic x(10) value spaces.
           02  out-x                 pic -(6)9.9(2).

       01  output-results-line-1.
           02  filler                pic x(9)  value " MEAN=   ".
           02  out-mean              pic -(6)9.9(2).

       01  output-results-line-2.
           02  filler                pic x(9)  value " STD DEV=".
           02  std-deviation         pic -(6)9.9(2).

      *> --------------------------------------------------------
      *> Procedure Division
      *> --------------------------------------------------------
       procedure division.

           open input  input-file
                output output-file.

           move zero to in-x.

           perform proc-body.

           perform end-of-job.

      *> --------------------------------------------------------
      *> proc-body: write headings, read data, compute statistics.
      *> --------------------------------------------------------
       proc-body.
           write output-line from output-title-line
               after advancing 0 lines.
           write output-line from output-underline
               after advancing 1 line.
           write output-line from output-col-heads
               after advancing 1 line.
           write output-line from output-underline
               after advancing 1 line.

           move zero to sum-of-x.

           read input-file into input-value-record
               at end perform end-of-job.

           perform input-loop
               varying n from 1 by 1
               until n is greater than 1000
                  or in-x is not less than 999999.98.

           subtract 1 from n.

           divide n into sum-of-x giving mean rounded.

           move zero to sum-of-x-sqr.

           perform sum-loop
               varying i from 1 by 1
               until i is greater than n.

           compute std-deviation rounded =
               (sum-of-x-sqr / n) ** 0.5.

           write output-line from output-underline
               after advancing 1 line.

           move mean to out-mean.
           write output-line from output-results-line-1
               after advancing 1 line.

           write output-line from output-results-line-2
               after advancing 1 line.

      *> --------------------------------------------------------
      *> input-loop: store each value, display it, accumulate sum.
      *> --------------------------------------------------------
       input-loop.
           move in-x to x(n), out-x.
           write output-line from output-data-line
               after advancing 1 line.
           add x(n) to sum-of-x.
           read input-file into input-value-record
               at end perform end-of-job.

      *> --------------------------------------------------------
      *> sum-loop: accumulate sum of squared deviations from mean.
      *> --------------------------------------------------------
       sum-loop.
           compute sum-of-x-sqr =
               sum-of-x-sqr + (x(i) - mean) ** 2.

      *> --------------------------------------------------------
      *> end-of-job: close files and stop.
      *> --------------------------------------------------------
       end-of-job.
           close input-file, output-file.
           stop run.
