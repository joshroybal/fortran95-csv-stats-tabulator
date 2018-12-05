! subroutine emits standard normal distribution figures and graph
subroutine report(x, n, m, ss)
   use stats_module
   implicit none
   ! dummy arguments
   integer, intent(in) :: n
   real, intent(in), dimension(n) :: x
   real, intent(in) :: m, ss
   ! local variables and arrays
   integer, parameter :: NS = 25 
   integer, dimension(NS) :: slices
   integer :: i, j, idx, smax
   real :: v, xmin, xmax, span, interval_length
   ! processing
   xmin = m - (3.0 * ss)
   xmax = m + (3.0 * ss)
   span = xmax - xmin
   interval_length = span / real(NS)
   slices = 0
   do i = 1, n
      v = x(i) - xmin
      if (v >= 0 .and. v <= span) then
         idx = (v / interval_length) + 1
         slices(idx) = slices(idx) + 1
      end if
   end do
   ! output
   smax = quick_select(NS, NS, slices)
   do i = 1, NS
     v = real(slices(i)) / real(smax)
     idx = NS * v
     write (*,1000) xmin + interval_length * (i - 1), ('=', j = 1, idx)
   end do
   return
   1000 format(F15.2,1X,72A1)
end subroutine report
