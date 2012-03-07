subroutine dgemm(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
! Simplified DGEMM that does
!     C := A*B
! We follow the BLAS signature for compatibility.  For more, 
! see http://netlib.org/blas/.
  implicit none
  ! input/output
  character*1, intent(in)         :: transa, transb
  integer, intent(in)             :: m, n, k, lda, ldb, ldc
  double precision, intent(in)    :: alpha, beta
  double precision, intent(in)    :: A(m, n)
  double precision, intent(in)    :: B(k, n)
  double precision, intent(inout) :: C(m, n) 
  ! local
  integer :: ii, jj, kk
  do ii= 1, m
    do jj = 1, k
      do kk = 1, n
        C(ii, jj) = C(ii, jj) + A(ii, kk) * B(kk, jj)
      end do
    end do
  end do
end subroutine dgemm
