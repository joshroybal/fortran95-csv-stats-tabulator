! cgi-bin online statistical calculator program
program cgistats
   use csv_module
   use stats_module
   implicit none
   ! interface declaration
   interface
      ! subroutine to read CGI query string
      subroutine read_query_string(card)
         character (len=*) :: card
      end subroutine read_query_string
      ! function returns formatted floating point numeric value
      function real_str(r, n)
         character (len=25) :: real_str
         real, intent(in) :: r
         integer, intent(in) :: n
      end function real_str      
      ! z values computation subroutine
      subroutine compute_z(x, n, avg, dev, z)
         integer, intent(in) :: n
         real, intent(in) :: avg, dev
         real, intent(in), dimension(n) :: x
         real, intent(out), dimension(n) :: z
      end subroutine compute_z
      ! cumulative from average table computation subroutine
      subroutine compute_tables(z, n, right_tail, left_tail, both_tails)
         integer, intent(in) :: n
         real, intent(in), dimension(n) :: z
         real, intent(out), dimension(10,41) :: right_tail, left_tail, &
            both_tails
      end subroutine compute_tables
      ! optimize statistical computations
      subroutine compute(x, n, m, pv, ps, sv, ss, lo, hi, mdn, mad, aad, skw)
         integer, intent(in) :: n
         real, intent(in), dimension(n) :: x
         real, intent(out) :: m, pv, ps, sv, ss, lo, hi, mdn, mad, aad, skw
      end subroutine compute
      ! ascii graphics (bell curve when distribution is standard normal)
      subroutine report(x, n, m, sd)
         integer, intent(in) :: n
         real, intent(in), dimension(n) :: x
         real, intent(in) :: m, sd
      end subroutine report
   end interface
   ! variable declarations
   integer, parameter :: LIM=1000000
   integer :: ioerr, eof, n, noflds, colno, i, j, idx
   real :: tmp, m, pv, ps, sv, ss, mdn, mad, aad, lo, hi, skw, t1, t2
   character (len=80) :: card, record
   character (len=25) :: field   
   integer, dimension(9) :: std_devs, mdn_devs, avg_devs
   real, dimension(LIM) :: x, z
   real, dimension(10,41) :: std_right, std_left, std_both, mdn_right, &
      mdn_left, mdn_both,  mean_right, mean_left, mean_both
   ! processing
   call read_query_string(card)
   write (*,1000) 'Content-Type: text/html; charset=utf-8'
   print *,  '<!DOCTYPE html>'
   print *,  '<html>'
   print *,  '<head>'
   print *,  '<meta name="viewport" content="width=device-width, &
   initial-scale=1.0">'
   print *,  '<link rel="stylesheet" media="all" &
      href="/includes/gradienttable.css">'
   print *,  '<title>Fortran Statistics</title>'
   print *,  '</head>'
   print *,  '<body>'
   print *,  '<header><p>Fortran Statistics</p></header>'
   print *,  '<div><a href="/index.php">Home</a> | &
   <a href="/f95stats.html">Back</a></div>'
   if (card == 'step1') then ! step1 generates the initial form
      print *, '<p></p>'
      print*, '<form action="f95stats.cgi?step2" method="post" enctype="&
      multipart/form-data">&
      <div>enter column no. to process : &
      <input type="text" name="colno" size="2" value=""></div>&
      <p></p>&
      <div><label><input type="file" name="csvdata"></div>&
      <p></p>&
      <div><input type="submit" value="Upload"></label></div></form>'
      print *, '<p></p>'
   else if (card == 'step2') then ! step2 is main processing step
      ! begin I/O processing
      call cpu_time(t1)
      do
         read (*,'(A)',iostat=eof) record
         if (eof /= 0) exit
         idx = index(record, 'colno')
         if (idx /= 0) exit
      end do

      do
         read (*,*,iostat=ioerr) colno
         if (ioerr == 0) exit
      end do

      n = 0
      do
         read (*,'(A)',iostat=eof) record
         if (eof /= 0) exit
         field = get_field(record, colno)
         read (field,*,iostat=ioerr) tmp
         if (ioerr == 0) then
            n = n + 1
            x(n) = tmp
         end if
      end do
      ! I/O processing tasks completed
      call cpu_time(t2)
      field = adjustl(real_str(t2-t1, 3))
      print *,  '<p>input processing time = ', trim(field), ' seconds.</p>'
      print *, '<p>working . . .</p>'
      call cpu_time(t1)
      ! begin core processing tasks
      ! compute stats
      call compute(x, n, m, pv, ps, sv, ss, lo, hi, mdn, mad, aad, skw)
      ! compute z values and cumulative from central tendencies tables
      ! standard deviations about the mean
      call compute_z(x, n, m, ps, z)
      call compute_tables(z, n, std_right, std_left, std_both)
      ! median deviations about the median
      call compute_z(x, n, mdn, mad, z)
      call compute_tables(z, n, mdn_right, mdn_left, mdn_both)
      ! mean deviations about the mean
      call compute_z(x, n, m, aad, z)
      call compute_tables(z, n, mean_right, mean_left, mean_both)
      call cpu_time(t2)
      ! output report
      field = adjustl(real_str(t2-t1, 3))
      print *, '<p>core processing time = ', trim(field), ' seconds</p>'
      ! ascii graphics first
      print *, '<pre>'
      call report(x, n, m, ss)
      print *, '</pre>'
      print *, '<p>general statistical summary</p>'
      print *, '<ul>'
      print *, '<li>size = ', n, '</li>'
      print *, '<li>mean = ', m, '</li>'
      print *, '<li>population variance = ', pv, '</li>'
      print *, '<li>population standard deviation = ', ps, '</li>'
      print *, '<li>sample variance = ', sv, '</li>'
      print *, '<li>sample standard deviation = ', ss, '</li>'
      print *, '<li>mininum = ', lo, '</li>'
      print *, '<li>maximum = ', hi, '</li>'
      print *, '<li>median = ', mdn, '</li>'
      print *, '<li>median deviation = ', mad, '</li>'
      print *, '<li>mean deviation = ', aad, '</li>'
      print *, '<li>skewness = ', skw, '</li>'
      print *, '<li>median skewness = ', 3*(m-mdn)/ps, '</li>'
      print *, '</ul>'
      ! print standard deviation cumulative from mean tables
      print *, '<p>standard deviation tables</p>'
      print *, '<div>cumulative from mean - right tail</div>'
      print *, '<table class="gradienttable">'
      print 3000, 'z',  ((i-1)/100.0,i=1,10)
      do j = 1, 41
         print 4000, (j-1)/10.0, (std_right(i,j),i=1,10)
      end do      
      print *, '</table>'
      print *, '<div>cumulative from mean - left tail</div>'
      print *, '<table class="gradienttable">'
      print 3000, 'z',  ((i-1)/100.0,i=1,10)
      do j = 1, 41
         print 4000, (j-1)/10.0, (std_left(i,j),i=1,10)
      end do      
      print *, '</table>'
      print *, '<div>cumulative from mean - left and right tails</div>'
      print *, '<table class="gradienttable">'
      print 3000, 'z',  ((i-1)/100.0,i=1,10)
      do j = 1, 41
         print 4000, (j-1)/10.0, (std_both(i,j),i=1,10)
      end do      
      print *, '</table>'
      ! print median deviation cumulative from median tables
      print *, '<p>median deviation tables</p>'
      print *, '<div>cumulative from median - right tail</div>'
      print *, '<table class="gradienttable">'
      print 3000, 'z',  ((i-1)/100.0,i=1,10)
      do j = 1, 41
         print 4000, (j-1)/10.0, (mdn_right(i,j),i=1,10)
      end do
      print *, '</table>'
      print *, '<div>cumulative from median - left tail</div>'
      print *, '<table class="gradienttable">'
      print 3000, 'z',  ((i-1)/100.0,i=1,10)
      do j = 1, 41
         print 4000, (j-1)/10.0, (mdn_left(i,j),i=1,10)
      end do
      print *, '</table>'
      print *, '<div>cumulative from median - left and right tails</div>'
      print *, '<table class="gradienttable">'
      print 3000, 'z',  ((i-1)/100.0,i=1,10)
      do j = 1, 41
         print 4000, (j-1)/10.0, (mdn_both(i,j),i=1,10)
      end do
      print *, '</table>'
      ! print mean deviation cumulative from mean tables
      print *, '<p>mean deviation tables</p>'
      print *, '<div>cumulative from mean - right tail</div>'
      print *, '<table class="gradienttable">'
      print 3000, 'z',  ((i-1)/100.0,i=1,10)
      do j = 1, 41
         print 4000, (j-1)/10.0, (mean_right(i,j),i=1,10)
      end do
      print *, '</table>'
      print *, '<div>cumulative from mean - left tail</div>'
      print *, '<table class="gradienttable">'
      print 3000, 'z',  ((i-1)/100.0,i=1,10)
      do j = 1, 41
         print 4000, (j-1)/10.0, (mean_left(i,j),i=1,10)
      end do
      print *, '</table>'
      print *, '<div>cumulative from mean - left and right tails</div>'
      print *, '<table class="gradienttable">'
      print 3000, 'z',  ((i-1)/100.0,i=1,10)
      do j = 1, 41
         print 4000, (j-1)/10.0, (mean_both(i,j),i=1,10)
      end do   
      print *, '</table>'
   end if
   print *,  '<div><a href=/index.php>Home</a> | &
   <a href=/f95stats.html>Back</a></div>'
   print *,  '<footer><p>Copyright (c) Josh Roybal 2017-2018</p></footer>'
   print *,  '</body>'
   print *,  '</html>'
   ! format defintions
   1000 format (a,/)
   2000 format (a256)
   3000 format ('<tr><th>'a,'</th>',10('<th>+',f4.2,'</th>'),'</tr>')
   4000 format ('<tr><th>',f3.1,'</th>',10('<td>',f7.3,'</td>'),'</tr>')   
end program cgistats

! subroutine reads CGI query string
subroutine read_query_string(card)
   implicit none
   ! dummy arguments
   character (len=*) :: card
   ! processing
   call getenv('QUERY_STRING', card)
end subroutine read_query_string

! function returns formatted floating point numeric value
function real_str(r, n)
   implicit none
   ! dummy arguments
   character (len=25) :: real_str
   real, intent(in) :: r
   integer, intent(in) :: n
   ! local variables
   character (len=25) :: nstr
   ! local variables
   character (len=25) :: fmtstr
   ! processing
   if (n < 0 .or. n > 20) then
      real_str = 'ERR'
      return
   end if
   if (n /= 0) then
      write (fmtstr,*) n
      fmtstr='(f25.'//trim(adjustl(fmtstr))//')'
      real_str=fmtstr
      write (nstr,fmtstr) r
   else
      write (nstr,*) int(r)
   end if
   real_str=adjustl(nstr)
end function real_str

! subprogram computes cumulative from average table
subroutine compute_z(x, n, avg, dev, z)
   implicit none
   ! dummy arguments
   integer, intent(in) :: n
   real, intent(in) :: avg, dev
   real, intent(in), dimension(n) :: x
   real, intent(out), dimension(n) :: z
   ! local variables
   integer :: i, j
   ! processing
   do i = 1, n
      z(i) = (x(i) - avg) / dev
   end do
end subroutine compute_z

! subprogram computes cumulative from average tables
subroutine compute_tables(z, n, right_tail, left_tail, both_tails)
   implicit none
   ! dummy arguments
   integer, intent(in) :: n
   real, intent(in), dimension(n) :: z
   real, intent(out), dimension(10,41) :: right_tail, left_tail, both_tails
   ! local variables
   integer :: i, j, k, idx
   integer, dimension(410) :: right_count, left_count
   ! processing
   right_count = 0
   left_count = 0
   do k = 1, n
      if (z(k) >= 0) then
         idx = 100 * z(k) + 2
         if (idx >= 1 .and. idx <= 410) then
            do i = idx, 410
               right_count(i) = right_count(i) + 1
            end do
         end if
      else
         idx = 100 * abs(z(k)) + 2
         if (idx >= 1 .and. idx <= 410) then
            do i = idx, 410
               left_count(i) = left_count(i) + 1
            end do
         end if         
      end if
   end do

   idx = 1
   do j = 1, 41
      do i = 1, 10
         right_tail(i,j) = right_count(idx+i-1) / real(n)
         left_tail(i,j) = left_count(idx+i-1) / real(n)
      end do
      idx = idx + 10
   end do

   both_tails = right_tail + left_tail
end subroutine compute_tables
