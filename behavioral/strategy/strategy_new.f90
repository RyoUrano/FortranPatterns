!!! This code is rewritten from Java code to Fortran code
!! based on the content in the book of "Design Pattern" by Hiroshi Yuuki, 


module module1
  implicit none 
  ! private
  ! protected:: access-id-list
  ! public::  access-id-list
  
     integer :: HAND_GUU=0,HAND_CHO=1,HAND_PAA=2
     character(3) :: &
          & CHAND_GUU="GUU", &
          & CHAND_CHO="CHO", &
          & CHAND_PAA="PAA"

  type:: hand 
     integer::hand_value_
   contains
     procedure:: init=>init_hand
     procedure ::set_hand
     procedure ::get_hand
     procedure ::fight
     procedure:: is_stronger
     procedure :: is_weaker
     ! procedure:: comp
  end type hand

  type(hand) :: hands(3)
  ! hands(1) = hand(hand_value_ = hand%HAND_GUU)

  ! call hand(1)%init(hand(1)%HAND_GUU)
  ! call hand(2)%init(HAND_CHO)
  ! call hand(3)%init(HAND_PAA)

  type,abstract :: abs_strategy
     
   contains
     ! procedure(nexthand),deferred :: nexthand_run
     ! procedure(study),deferred :: study_run
     ! generic :: nexthand => nexthand_run
     ! generic :: study => study_run
     procedure(nexthand),deferred :: nexthand
     procedure(study),deferred:: study
  end type abs_strategy

  abstract interface
     subroutine nexthand(this,hand0) 
       import abs_strategy
       import hand
      class(abs_strategy) ::this
      type(hand),intent(inout)::hand0

     end subroutine nexthand

     subroutine study(this,win) 
       import abs_strategy
       class(abs_strategy),intent(inout) ::this
       logical,intent(in)::win
       ! class(abs_strategy) ::this
       ! logical::win
     end subroutine study

  end interface
  ! interface
  !    subroutine get_hand(int,char)
  !      implicit none
  !      integer, intent(in):: int
  !      character(*), intent(out):: char
  !    end subroutine get_hand
  ! end interface

  type, extends(abs_strategy) :: win_strat
     logical :: won=.false.
     class(hand),allocatable :: prevHand
     real(8) :: random
   contains
     procedure:: init=>init_win
     procedure:: nexthand=>nexthand_win
     procedure:: study=> study_win
  end type win_strat

  ! interface
  !    subroutine nexthand_win(this)
  !      import win_strat
  !      class(win_strat) ::this
  !    end subroutine nexthand_win

  !    subroutine study_win(this)
  !      import win_strat
  !      class(win_strat) ::this
  !    end subroutine study_win

  ! end interface

contains



  subroutine init_hand(this,int)
    implicit none 
    class(hand),intent(inout)::this
    integer, intent(in) ::int
    this%hand_value_= int
  end subroutine init_hand

  subroutine get_hand(this,int,char)
    implicit none 
    class(hand),intent(inout)::this
    integer, intent(in) ::int
    character(*), intent(out):: char

    if(int .eq. 0  ) then
       char=CHAND_GUU
    else        if(int .eq. 1  ) then
       char= CHAND_CHO

    else        if(int .eq. 2  ) then
       char=CHAND_PAA
    else
       write(*,*) "no such value"
    endif
  end subroutine get_hand

  subroutine set_hand(this,int)
    implicit none 
    class(hand),intent(inout)::this
    integer, intent(in) ::int
    this%hand_value_= int
  end subroutine set_hand

  subroutine is_stronger(this,that,result)
    implicit none 
    class(hand),intent(inout):: this
    class(hand),intent(inout):: that
    integer:: int_res
    logical,intent(out)::result

   ! write(*,*) "this%hand_value_=",this%hand_value_

    result=.false.
    call this%fight(that,int_res)
    if (int_res .eq. 1 )   result=.true.

  end subroutine is_stronger


  subroutine is_weaker(this,that,result)
    implicit none 
    class(hand),intent(inout):: this
    class(hand),intent(inout):: that
    integer:: int_res
    logical,intent(out)::result


    result=.false.
    call this%fight(that,int_res)
    if (int_res .eq. -1 )   result=.true.
  end subroutine is_weaker

  subroutine fight(this,that,result)
    implicit none 
    class(hand),intent(inout):: this
    class(hand),intent(inout):: that
    integer, intent(out)::result
    integer ::val1,val2

    val1= this%hand_value_  ;
    val2= that%hand_value_  ;

    if( val1 .eq. val2 ) then 
       result=0;
    else if(mod( val1+1,3)  == val2) then
       result=1;
    else 
       result=-1;
    endif

  end subroutine fight

  ! declaration of named constants
  ! derived type definition
  ! interfaces
  ! variables whose scope is the entire modules 


  subroutine init_win(this)
    class(win_strat),intent(inout) ::this
    ! integer,intent(in):: seed
    real(8) :: x

    integer:: seedsize
    integer,allocatable:: seeds(:)

    call random_seed(size=seedsize)  ! シードの格納に必要なサイズを取得する
    allocate(seeds(seedsize))         ! シード格納領域を確保
    call random_seed(get=seeds)       ! 次回同じ乱数を発生できるように
    ! call random_seed(put=1001)   
    call random_number(x)
    this%random= x;
    this%won=.false.

    ! write(*,*)"    this%random=" ,this%random," ; this%won",this%won
  end subroutine init_win


  subroutine nexthand_win(this,hand0)
    class(win_strat) ::this
    type(hand),intent(inout)::hand0

    integer :: random_int ! 1,2,3
    
    call this%init()
    
    random_int=  Int(this%random*3.0)+1;

    ! write(*,*) "random_int",random_int
    call hands(1)%init(HAND_GUU)
    call hands(2)%init(HAND_CHO)
    call hands(3)%init(HAND_PAA)

    
    if ( this%won ) then
    else 
       if(allocated(this%prevHand)) then
          this%prevHand= hands(random_int)
       else
          allocate(this%prevHand,source=hands(random_int))
       end if
    end if

    ! hand0 = hands(1)
    hand0=this%prevHand
      

    ! return this%prevhenad
  end subroutine nexthand_win

  subroutine study_win(this,win)
    class(win_strat),intent(inout) ::this
    logical,intent(in)::win
    this%won=win
  

  end subroutine study_win


end module module1

  module module2
    use module1
    implicit none 
    private
    ! protected:: access-id-list
    public:: player

    ! declaration of named constants
    ! derived type definition
    ! interfaces
    ! variables whose scope is the entire modules 
    type ::player
       class(abs_strategy),allocatable :: strategy
       character(:), allocatable :: name
       integer:: gamecount
       integer:: nwin
       integer:: nlose

       contains
         procedure ::init=>init_player
         procedure ::nexthand =>nexthand_player
         procedure :: win=> win_player
         procedure :: even=> even_player
         procedure :: lose=> lose_player
    end type player


  contains
    subroutine init_player(this,s,name)
      class(player),intent(inout) ::this
      class(abs_strategy),intent(inout)::s
      character(*) :: name
      
      this%name=name
      ! this%strategy=s
      if(allocated(this%strategy) )then
         this%strategy=s
      else
         allocate(this%strategy,source=s) 
      end if

    end subroutine init_player

    subroutine nexthand_player(this,hand0)
      class(player),intent(inout) ::this
      type(hand),intent(out)::hand0
      call    this%strategy%nexthand(hand0)
    end subroutine nexthand_player

    subroutine win_player(this)
      class(player),intent(inout) ::this
      call  this%strategy%study(.true.)
      this%gamecount=this%gamecount+1
      this%nwin=this%nwin+1
    end subroutine win_player
    subroutine even_player(this)
      class(player),intent(inout) ::this
      this%gamecount=this%gamecount+1
    end subroutine even_player
    subroutine lose_player(this)
      class(player),intent(inout) ::this
      call  this%strategy%study(.false.)
      this%gamecount=this%gamecount+1
      this%nlose=this%nlose+1
    end subroutine lose_player


  end module module2

  program    strategy_main2
    use module1 
    use module2
    implicit none
    
    type(win_strat)::wins
    character(:),allocatable:: name1,name2
    type(player) :: player1,player2
    type(hand) :: nexthand1,nexthand2
    logical :: bval1,bval2
    integer::i

    name1="James"
    name2="Jane"

    call player1%init(wins,name1)
    call player2%init(wins,name2)


do i =1 , 10
    call player1%nexthand(nexthand1)
    call player2%nexthand(nexthand2)

    call nexthand1%is_stronger(nexthand2,bval1)
    if(bval1)   then
       write(*,*) "Player1 won" 
       call       player1%win()
       call       player2%lose()
    endif

    call nexthand2%is_stronger(nexthand1,bval2)
    if(bval2)   then
       write(*,*) "Player2 won" 
       call       player2%win()
       call       player1%lose()
    endif
    
!!!even
    if(bval1) then
    else if(bval2 ) then
    else
       ! write(*,*) "even game"
       call player1%even()
       call player2%even()
       endif
enddo


write(*,*) "Result:"
write(*,*) "total game=",player1%gamecount
write(*,*) "player ",player1%name," #win=" ,player1%nwin
write(*,*) "player ",player2%name," #win=" ,player2%nwin


  end program strategy_main2
