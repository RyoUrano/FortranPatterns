!! Author Ryo Urano
!! This source code is re-written from Java code in the book of 増補改訂版JAVA言語で学ぶデザインパターン入門 written by Yuki Hiroshi.
!! Fortran code is written by Urano based on Java code.


module abstractIterator
  implicit none 
  private
  protected:: iterator,aggregate
  public::  iterator,aggregate


  type, abstract::iterator
   contains
     procedure (hasnext),deferred :: hasNext_abs
     procedure (nextobj),deferred :: next_abs
     generic :: hasNext=>hasNext_abs
     generic:: next=>next_abs
  end type iterator
  
  
  abstract interface 
     logical   function hasnext(self)  result(bool)
       import iterator
       class(iterator),intent(inout) ::self
     end function hasnext

     function nextobj(self) result(obj)
       import iterator
       class(iterator),intent(inout) ::self
       class(*),allocatable::obj
     end function nextobj
  end interface
  

  type,abstract :: aggregate
     class(iterator),allocatable:: it
  end type aggregate

end module abstractIterator



module  bookiterator
  use abstractIterator
  implicit none 
  private
  ! protected:: access-id-list
  public:: Book,BookShelf,BookShelfIterator,new_Book,new_Bookshelf
  

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
  type ::Book
     character(len=:),allocatable::name
   contains
     procedure ::getname
  end type Book

  type ,extends(aggregate):: BookShelf
     type(Book),allocatable:: books(:)
     integer ::last=1
   contains
     procedure :: getBookAt
     procedure:: appendBook 
     procedure::getLength
     procedure::iteratorbook
     generic :: iterator =>iteratorbook
  end type BookShelf
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
!! Iterator for Book object in Bookshelf 
  type, extends(iterator)::BookShelfIterator
     type(BookShelf):: bookshelf_
     integer :: index
   contains
     procedure ::hasnext_abs=> hasNext_bookshelf
     procedure ::next_abs=> next_bookshelf
  end type BookShelfIterator

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
!! Constructor
  interface news
     module procedure new_Book
     module procedure new_Bookshelf
     module procedure new_BookShelfItrator
  end interface news



contains
  
  logical   function hasNext_bookshelf(self)  result(bool)
    ! class(iterator),intent(inout),target ::self
    class(BookshelfIterator),intent(inout) ::self

    if ( self %index  < self%bookshelf_%getLength () )then
       bool=.true.
    else
       bool=.false.
    endif

  end function hasNext_bookshelf

  function next_bookshelf(self) result(obj)
    ! class(iterator),target,intent(inout) ::self
    class(BookshelfIterator),intent(inout)::self
    class(*),allocatable::obj
    ! class(Book),allocatable::obj

    allocate(obj,source=self%bookshelf_%getBookAt(self%index) )
    self%index=self%index+1

  end function next_bookshelf



  function getBookAt(self,index) result(book0)
    class(BookShelf),intent(in)::self
    integer ,intent(in)::index
    type(book) ::book0
    book0 = self%books(index)
  end function getBookAt

  subroutine appendBook(self,newbook)
    class(BookShelf),intent(inout)::self
    type(book),intent(in) ::newbook
    self%books(self%last)= newbook
    self%last=self%last+1
  end subroutine appendBook
  
  function getLength(self) result(len)
    class(BookShelf),intent(in)::self
    integer::len
    len=self%last
  end function getLength

  function iteratorbook(self) result(it)
    class(BookShelf),intent(in)::self
    class(bookshelfiterator),allocatable::it
    it= new_BookShelfItrator(self) 
  end function iteratorbook

  function getname (self) result(name)
    class(Book),intent(in)::self
    character(len=:),allocatable::name
    name =self%name
  end function getname

!! Concrete constructor definition 
  function new_Book(param) result(out)
    character(len=*),intent(in)::param
    type(Book)::out
    out%name=param
  end function new_Book

  function new_BookShelf(param) result(out)
    integer,intent(in)::param
    type(BookShelf)::out
    allocate(out%books(param))
  end function new_BookShelf

  function new_BookShelfItrator(inbookshelf) result(out)
   class(BookShelf)::inbookshelf
   type(BookshelfIterator)::out
   out%bookshelf_ = inbookshelf
   out %index=1
  end function new_BookShelfItrator



end module bookiterator


program main
use bookiterator
implicit none

type(bookshelf):: bookShelfm
class(bookshelfiterator),allocatable:: it
type(Book) ::this
bookShelfm = new_bookshelf(4)
call bookShelfm%appendBook( new_Book("Around the world in 80days"))
call bookShelfm%appendBook( new_Book("Bible"))
call bookShelfm%appendBook( new_Book("Cinderella"))
call bookShelfm%appendBook( new_Book("Daddy-Long-Legs"))

it = bookShelfm%iterator()

      ! write(*,*)"it%hasNext()",it%hasNext()

do while(it%hasNext()) 
      ! write(*,*)"test"
   tag: associate (its => it%next()) 
   select type (its)
   type is (Book) 
      this= its

      write (*,*)  this%getName()
   end select
   ! this= it%next()


 end associate tag

end do


end program main
