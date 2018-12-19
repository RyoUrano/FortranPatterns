


  module  visitor
    implicit none 
    private
    ! protected:: access-id-list
    ! public::  access-id-list

    type,abstract::Entry
     contains
       procedure(getNametype),deferred:: getName_abs
       procedure(getSizetype),deferred:: getSize_abs
       procedure(printListtype),deferred:: printList_abs
       procedure(addtype),deferred:: add_abs
       generic ::getSize=> getSize_abs
       generic :: printList=>printList_abs
       generic :: getName=>getName_abs
       generic :: add=>add_abs
    end type Entry

    abstract interface
       function getNametype (self) result(name)
         import Entry
         class(Entry),intent(in)::self
         character(len=:),allocatable:: name
       end function getNametype

       function getSizetype (self) result(size)
         import Entry
         class(Entry),intent(in)::self
         integer ::size
       end function getSizetype

      subroutine printListtype (self,string)
         import Entry
         class(Entry),intent(in)::self
      character(len=:),allocatable:: string
       end subroutine printListtype
       
      subroutine addtype (self, obj)
         import Entry
         class(Entry),intent(inout)::self
         class(Entry),intent(in)::obj
       end subroutine addtype
    end interface

    type,extends(Entry)::File
       private
       character(len=:) ,allocatable::name
       integer::size
     contains
       procedure:: getName_abs=>getName_file
       procedure ::getSize_abs=> getSize_file
       procedure :: printList_abs=> printList_file
       procedure :: add_abs => add_file
    end type File

    type,extends(Entry)::Directory
       private
       character(len=:) ,allocatable::name
       class(Entry),allocatable::entries
       integer::last
       contains
       procedure:: getName_abs=>getName_directory
       procedure ::getSize_abs=> getSize_directory
       procedure :: printList_abs=> printList_directory
       procedure :: add_abs => add_directory

      end type Directory

  contains
    function getName_directory(self) result(name)
      class(Directory),intent(in)::self
      character(len=:),allocatable:: name      
      name =self%name   
    end function getName_directory

    function getSize_directory (self) result(size)
      class(Directory),intent(in)::self
      ! class(Iterator):: it
      integer ::size
      type(File) ::this_file
      type(Directory) ::this_directory
      size=0

    !   it = self%iterator()
    !   do while(it%hasNext()) 
    !      ! write(*,*)"test"
    !      tag: associate (its => it%next()) 
    !      select type (its)
    !      type is (File) 
    !         this_file= its
    !         size= size+ this_file%size
    !         ! write (*,*)  this%getName()
    !      type is (Directory)
    !         size= size+ this_directory%getSize()
    !      class default
    !         write(*,*) "No match error"
    !      end select
    !    end associate tag
       
    ! end do

  end function getSize_directory


    
    subroutine printList_directory (self,string)
      class(Directory),intent(in)::self
      character(len=:),allocatable:: string
      ! class(Iterator):: it
      class(Entry),pointer ::this
       write(*,*) string,"/",self%name

    !   it = self%iterator()
    !   do while(it%hasNext()) 
    !      ! write(*,*)"test"
    !      tag: associate (its => it%next()) 
    !      select type (its)
    !      class is (Entry) 
    !         this=it
    !         ! write (*,*)  "/",this%name()
    !      class default
    !         write(*,*) "No match error"
    !      end select
    !    end associate tag
       
    ! end do
    end subroutine printList_directory

    subroutine add_directory(self,obj)
      class(Directory),intent(inout)::self
      class(Entry),intent(in)::obj
      
    ! select type (obj)
       self%entries(self%last) = obj
       
    ! end select

      write(*,*) "File:: throw execption"
    end subroutine add_directory


    function getName_file(self) result(name)
      class(File),intent(in)::self
      character(len=:),allocatable:: name      
      name =self%name   
    end function getName_file
    
    function getSize_file (self) result(size)
      class(File),intent(in)::self
      integer ::size
      size=self%size
    end function getSize_file


    
    subroutine printList_file (self,string)
      class(File),intent(in)::self
      character(len=:),allocatable:: string


      write(*,*) string,"/",self%name

    end subroutine printList_file

    subroutine add_file (self,obj)
      class(File),intent(inout)::self
      class(Entry),intent(in)::obj

      write(*,*) "File:: throw execption"
    end subroutine add_file

  end module visitor

  program main
    use visitor
    implicit none
    
    type(Directory):: rootdir,bindir,tmpdir,usrdir
    type(Directory)::yuki,hanako,tomura


    rootdir=new_Directory("root")
    bindir=new_Directory("bin")
    usrdir=new_Directory("usr")    
    tmpdir=new_Directory("tmp")    

    call    rootdir%add(bindir)
    call    rootdir%add(usrdir)
    call    rootdir%add(tmpdir)
    call    bindir%add(new_File("vi",10000))
    call    bindir%add(new_File("latex",20000))
    call rootdir%printList()

    write(*,*)" "
    write(*,*)"Making useer entries"
    
    yuki=new_Directory("yuki")
    hanako=new_Directory("hanako")
    tomura=new_Directory("tomura")

    call usrdir%add(yuki)
    call usrdir%add(hanako)
    call usrdir%add(tomura)

    call yuki%add(new_File("diary.html",100))
    call yuki%add(new_File("Composite.java",200))
    call hanako%add(new_File("memo.tex",300))
    call tomura%add(new_File("game.doc",400))
    call tomura%add(new_File("junk.mail",500))
    call rootdir%printList()

    


  end  program main
