      Subroutine uuidgen(uuid)

C $Id: $


C     Generates a version 4 uuid as 32 lowercase hexadecimal digits
C     formatted into a 36 char string as I found defined in:
C     http://en.wikipedia.org/wiki/Universally_unique_identifier#Version_4_.28random.29
C     Version 4 UUIDs use a scheme relying only on random numbers. 
C     This algorithm sets the version number (4 bits) as well as two 
C     reserved bits. All other bits (the remaining 122 bits) are set 
C     using a random or pseudorandom data source. Version 4 UUIDs have 
C     the form xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx where x is any 
C     hexadecimal digit and y is one of 8, 9, A, or B 
C     (e.g., f47ac10b-58cc-4372-a567-0e02b2c3d479).
C            46d1464d-2070-4f54-bf27-7a8ab95bdc82
C     In this implimentation, y is picked at random at each call
C     and the random number generator is only reseeded if the routine 
C     "thinks" it has not been seeded...reseeding at every call is avoided
C     due to the use of the clock on systems that have no /dev/urandom

      implicit none
      logical init/.TRUE./
      save init
      character*36 uuid
      character*8  aschar(8)
      character*5 y/"89AB8"/
      character*6 up/"ABCDEF"/,lo/"abcdef"/
      real :: r(8)
      integer i,k
      if (init) then
        call init_random_seed
        init=.FALSE.
      endif
      call random_number(r)
      do i=1,8
        write (aschar(i),fmt="(z8)") r(i)
      enddo
      i = int(r(1)*4)+1 ! if r==1, i=5, so y is 5 chars long.
      ! use different parts of the double, avoid the first and last
      ! portions 
      uuid = aschar(1)(3:6) // aschar(2)(4:7) // '-' // aschar(3)(3:6)
     >       // '-4' // aschar(4)(4:6) // '-' // y(i:i) 
     >       // aschar(5)(3:5) // '-' // aschar(6)(3:6) 
     >       // aschar(7)(4:7) // aschar(8)(3:6)
      do i=1,36
        k = index(up,uuid(i:i))
        if (k.ne.0) uuid(i:i) = lo(k:k)
      enddo
      return
      contains

       ! This code is from: 
       ! https://gcc.gnu.org/onlinedocs/gfortran/RANDOM_005fSEED.html
       ! accessed 2014/08/21

        subroutine init_random_seed
          use iso_fortran_env, only: int64
          implicit none
          integer, allocatable :: seed(:)
          integer :: i, n, un, istat, dt(8), pid, getpid
          integer(int64) :: t
      
          call random_seed(size = n)
          allocate(seed(n))
          ! First try if the OS provides a random number generator
          open(newunit=un, file="/dev/urandom", access="stream",
     >       form="unformatted", action="read", status="old", 
     >       iostat=istat)
          if (istat == 0) then
             read(un) seed
             close(un)
          else
          ! if no /dev/urandom, then use the clock and pid.
            call system_clock(t)
            if (t == 0) then
               call date_and_time(values=dt)
               t = (dt(1) - 1970) * 365_int64 * 24 * 60 * 60 * 1000 
     >             + dt(2) * 31_int64 * 24 * 60 * 60 * 1000 
     >             + dt(3) * 24_int64 * 60 * 60 * 1000 
     >             + dt(5) * 60 * 60 * 1000 
     >             + dt(6) * 60 * 1000 + dt(7) * 1000 
     >             + dt(8)
            end if
            pid = getpid()
            t = ieor(t, int(pid, kind(t)))
            do i = 1, n
               seed(i) = lcg(t)
            end do
          endif
          call random_seed(put=seed)
        end subroutine init_random_seed

          ! This simple PRNG might not be good enough for real work, but is
          ! sufficient for seeding a better PRNG.
          function lcg(s)
          use iso_fortran_env, only: int64
            integer :: lcg
            integer(int64) :: s
            if (s == 0) then
               s = 104729
            else
               s = mod(s, 4294967296_int64)
            end if
            s = mod(s * 279470273_int64, 4294967291_int64)
            lcg = int(mod(s, int(huge(0), int64)), kind(0))
          end function lcg
      end subroutine uuidgen


