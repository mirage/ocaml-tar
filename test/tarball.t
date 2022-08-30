Tests of the tar binary
  $ mkdir archive
  $ echo "Hello World" > archive/foo
  $ tar archive/ v.tar.gz
  $ tar list v.tar.gz
  archive/ (0 byte)
  archive/foo (12.00 B)
