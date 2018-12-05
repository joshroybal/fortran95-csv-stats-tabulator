! Modern Fortran .csv data processing module

module csv_module
contains
   ! integer function computes no. of fields by counting header line commas
   function count_fields(header) result(noflds)
      implicit none
      ! dummy arguments
      character (len=*), intent(in) :: header
      ! local variables
      integer :: i, length, noflds
      ! processing
      noflds=1
      length=LEN(TRIM(header))
      do i=1,length
         if (header(i:i) .eq. ',') noflds=noflds+1
      end do
   end function

   ! subroutine fetches all fields from csv record
   subroutine read_fields(fields, record, noflds)
      implicit none
      ! dummy arguments
      integer, intent(in) :: noflds
      character (len=*), intent(in) :: record
      character (len=*), intent(out), dimension(noflds) :: fields
      ! local data
      integer :: i, k, reclen, fldno
      ! clear the fields array
      do i=1,noflds
        fields(i)=''
      end do
      i=1
      fldno=1
      k=1
      reclen=LEN(TRIM(record))
      do while (i .le. reclen)
         do while (record(i:i) /= ',' .and. i <= reclen)
            if (record(i:i) /= '"') then
               if (k <= 25) fields(fldno)(k:k)=record(i:i)
               i=i+1
               k=k+1
            else
               ! skip through double quoted escaped sections of the csv record
               i=i+1
               do while (record(i:i) /= '"')
                  if (k <= 25) fields(fldno)(k:k)=record(i:i)
                  i=i+1
                  k=k+1
               end do
               i=i+1
            end if
         end do
         i=i+1
         fldno=fldno+1
         k=1
      end do
   end subroutine

   ! subroutine fetches all fields from csv record
   function get_field(record, n) result(field)
      implicit none
      ! dummy arguments
      integer, intent(in) :: n
      character (len=*), intent(in) :: record
      ! local variables
      integer :: i, k, length, fldno
      character (len=25) :: field
      ! processing
      ! clear function string
      field=''
      i=1
      fldno=1
      k=1
      length=LEN(TRIM(record))
      do while (i .le. length)
         do while (record(i:i) .ne. ',' .and. i .le. length)
            if (record(i:i) .ne. '"') then
               if (fldno == n) field(k:k)=record(i:i)
               i=i+1
               k=k+1
               if (k > 25) return
            else
               ! skip through double quoted escaped sections of the csv record
               i=i+1
               do while (record(i:i) .ne. '"')
                  if (fldno == n) field(k:k)=record(i:i)
                  i=i+1
                  k=k+1
                  if (k > 25) return
               end do
               i=i+1
            end if
         end do
         i=i+1
         fldno=fldno+1
         k=1
      end do
   end function
end module
