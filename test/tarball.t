Tests of the tar binary
  $ mkdir archive
  $ echo "Hello World" > archive/foo
  $ otar archive/ v.tar.gz
  $ otar list v.tar.gz
  archive/ (0 byte)
  archive/foo (12.00 B)
