Tests of the tar binary
  $ mkdir archive
  $ cd archive
  $ echo "Hello World" > foo
  $ cd ..
  $ otar archive v.tar.gz
  $ otar list v.tar.gz
  archive (0 byte)
  archive/foo (12.00 B)
