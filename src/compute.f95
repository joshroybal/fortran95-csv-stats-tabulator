! subroutine computes many stats at once - avoids repeated processes
subroutine compute(x, n, m, pv, ps, sv, ss, lo, hi, mdn, mad, aad, skw)
   use stats_module
   implicit none
   ! dummy arguments
   integer, intent(in) :: n
   real, intent(in), dimension(n) :: x
   real, intent(out) :: m, pv, ps, sv, ss, mdn, mad, aad, lo, hi, skw
   ! local variables
   integer :: i, s
   real, dimension(n) :: mdevs, mdndevs
   ! processing
   ! compute the mean
   m = mean(x, n)
   ! compute deviations about the mean for later use
   do i = 1, n
      mdevs(i) = x(i) - m
   end do
   ! compute the population variance
   pv = 0
   do i = 1, n
      pv = (((i - 1) * pv) + (mdevs(i))**2) / i
   end do
   ! compute the population standard deviation
   ps = sqrt(pv)
   ! compute the sample variance
   sv = real(n) / (n - 1) * pv
   ! compute the sample standard deviation
   ss = sqrt(sv)
   ! compute the median
   mdn = median(x, n)
   ! compute the median abasolute deviation
   do i = 1, n
      mdndevs(i) = abs(x(i) - mdn)
   end do
   mad = median(mdndevs, n)
   ! compute the mean deviation
   aad = 0
   do i = 1, n
      aad = (((i - 1) * aad) + abs(mdevs(i))) / i
   end do
   ! compute the miniumum
   lo = quick_select(1, n, x)
   ! compute the maximum
   hi = quick_select(n, n, x)
   ! compute the skewness
   skw = 0
   do i = 1, n
      skw = (((i - 1) * skw) + (mdevs(i))**3) / i
   end do
   skw = skw / ps**3
end subroutine compute
