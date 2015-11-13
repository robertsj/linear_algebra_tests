! Driver program to test matrix-matrix multiplication
program matmat_driver
  implicit none
  double precision, allocatable :: A(:, :), B(:, :), C(:, :)
  double precision :: alpha = 1.0, beta = 0.0, t, t2, omp_get_wtime, flops
  integer :: n, m, k, lda, ldb, ldc, incx = 1, incy = 1, i, j, maxi
  character*1 :: trans = 'n'
  do j = 1, 300
    ! Matrix size
    m   = 8 * j
    n   = m
    k   = m
    lda = m
    ldb = m
    ldc = m
    ! Create the matrices
    allocate(A(m, k), B(k, n), C(m, n))
    ! Initialize A and B
    A = 1.0
    B = 1.0
    ! Start the timer.
    t = omp_get_wtime()
    ! Loop over and apply A several times for consistent timing
    if (j .lt. 100) then
      maxi = 4
    else
      maxi = 1
    end if
    do i = 1, maxi
      ! Reset 
      C = 0.0
      call dgemm('n', 'n', m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
    end do
    t2 = omp_get_wtime()-t
    flops = dble(m*m) / dble(1e9) / t2
    flops = dble(2*maxi*m)*flops
    print '(i6, a,f18.6)', m, " ", flops
    ! print ('i4  *, m, ",", flops / dble(1e6) / t2, ",", t2
    deallocate(A, B, C)
  end do
end program matmat_driver
