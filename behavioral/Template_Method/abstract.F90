!! Author Ryo Urano
!! This source code is re-written from Java code in the book of 増補改訂版JAVA言語で学ぶデザインパターン入門 written by Yuki Hiroshi.
!! Fortran code is written by Urano based on Java code.


  module absdisplay
    implicit none 
    private
    public::  AbstractDisplay

    ! declaration of named constants

    ! derived type definition
    type,abstract::AbstractDisplay
    ! interfaces
    ! variables whose scope is the entire modules 
     contains
       procedure(open_abs_IFace),deferred :: open_abs
       procedure(print_abs_IFace),deferred :: print_abs
       procedure(close_abs_IFace),deferred :: close_abs
       procedure :: display_abs
       ! procedure(display_abs_IFace),deferred :: display_abs
       generic :: open=> open_abs
       generic :: print=> print_abs
       generic :: close=> close_abs
       generic :: display=> display_abs
    end type AbstractDisplay
    abstract interface
       subroutine open_abs_IFace(self)
         import AbstractDisplay
         class(AbstractDisplay),intent(inout) ::self
       end subroutine open_abs_IFace
       subroutine print_abs_IFace(self)
         import AbstractDisplay
         class(AbstractDisplay),intent(inout) ::self
       end subroutine print_abs_IFace
       subroutine close_abs_IFace(self)
         import AbstractDisplay
         class(AbstractDisplay),intent(inout) ::self
       end subroutine close_abs_IFace
       subroutine display_abs_IFace(self)
         import AbstractDisplay
         class(AbstractDisplay),intent(inout) ::self
       end subroutine display_abs_IFace
    end interface

    ! subroutine open_abs(self)
   !   class(AbstractDisplay),intent(inout)::self

contains

    subroutine display_abs(self)
      class(AbstractDisplay),intent(inout) ::self
      integer ::i

      call self%open()

      do i=1,5
         call self%print
      enddo

      call self%close()

    end subroutine display_abs

 end module absdisplay


 module display
   use absdisplay
    implicit none 
    private
    public ::new_CharDisplay
!    public ::CharDisplay,new_CharDisplay

    type,extends(AbstractDisplay)::CharDisplay
       character(len=:),allocatable::ch
     contains
       procedure::open_abs => open_char
       procedure:: close_abs=>close_char
       procedure:: print_abs=>print_char
       ! generic:: print=>print_char
       ! generic:: open=>open_char
       ! generic:: close=>close_char
       procedure:: init_charD
       generic:: init=>init_charD
    end type CharDisplay


    interface myclass
       module procedure new_CharDisplay
    end interface myclass


  contains

   type(CharDisplay) function new_CharDisplay(ch) result(out)
     character(len=*),intent(in)::ch
     out%ch=ch

   end function new_Chardisplay


    subroutine init_charD(self,char)
      class(CharDisplay),intent(out):: self
      character(len=*),intent(in)::char
      self%ch = char
    end subroutine init_charD


    subroutine open_char(self)
      class(CharDisplay),intent(inout):: self
      ! character(len=*),intent(in)::char
      ! self%ch = char
      write(*,*)">>>>\n\n"
    end subroutine open_char

    subroutine print_char(self)
      class(CharDisplay),intent(inout):: self
      
      write(*,*) self%ch

    end subroutine print_char



    subroutine close_char(self)
      class(CharDisplay),intent(inout):: self
      write(*,*)">>>>\n\n"
    end subroutine close_char
 end module display

 module stringD
   use absdisplay
    implicit none 
    private
    ! public ::StringDisplay,new_StringDisplay
    public ::new_StringDisplay


    type,extends(AbstractDisplay)::StringDisplay
       character(:),allocatable:: string
       integer(4):: width
     contains
       procedure::open_abs => open_string
       procedure:: close_abs=>close_string
       procedure:: print_abs=>print_string
       ! generic:: print=>print_string
       ! generic:: open=>open_string
       ! generic:: close=>close_string
       procedure:: init_stringD
       generic:: init=>init_stringD
    end type StringDisplay

    interface myclass
       module procedure new_StringDisplay
    end interface myclass


  contains

   type(StringDisplay) function new_StringDisplay(string) result(out)
     character(len=*),intent(in)::string
      integer:: nlen
     out%string=string
     nlen=len_trim(string)
     out%width=nlen
   end function new_StringDisplay

    subroutine init_stringD(self,string)
      class(StringDisplay),intent(out):: self
      character(len=*),intent(in)::string
      integer:: nlen
      self%string = string
      nlen=len_trim(string)
      self%width=nlen
    end subroutine init_stringD

    subroutine print_string(self)
      class(StringDisplay),intent(inout):: self
      write(*,*) "|" , self%string ,"|"
    end subroutine print_string

    subroutine open_string(self)
      class(StringDisplay),intent(inout):: self
      call printLine(self)      
! write(*,*)"-------------------------------------=="
     end subroutine open_string

    subroutine close_string(self)
      class(StringDisplay),intent(inout):: self
      call printLine(self)
! write(*,*)"-------------------------------------=="
    end subroutine close_string

    subroutine printLine(self)
      implicit none
      class(StringDisplay),intent(inout):: self    
      integer(4)::i

      write(*,"(1a)",advance="no")"+"
      do i=0,self%width
         write(*,"(1a)",advance="no")"-"   
      enddo
      write(*,*)"+"

    end subroutine printLine

  end module stringD


 program main
   use absdisplay
   use display
   use stringD
   implicit none
   class(AbstractDisplay),allocatable::AD
   ! type(CharDisplay):: CD
   ! type(StringDisplay):: SD

   ! call CD%init("H")
!   AD=CD
   AD=new_CharDisplay("H")
   call AD%display()
   deallocate(AD)
   ! call SD%init("Hello World!")
   ! AD=SD
   AD= new_StringDisplay("Hello World!")
   call AD%display()


 end program main
