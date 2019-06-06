module regex_mod
use iso_fortran_env
use iso_c_binding
implicit none

private

integer(4),parameter :: n_groups = 50

! regex.h typs/stuctures   >>>>
type regex_t
    INTEGER(1),contiguous,pointer :: buffer(:) => null()
    type(c_ptr) :: rgx = c_null_ptr
  contains
    procedure :: regex_t_final
end type

type, bind(C) :: regmatch_t
    integer(c_int) :: rm_so
    integer(c_int) :: rm_eo
end type
! regex.h typs/stuctures   <<<<

type match_group
    integer(4) :: start_pos
    integer(4) :: end_pos
    integer(4) :: length
    character(len=:),allocatable :: m_string
  contains
    procedure, private :: write => write_match_group
    generic :: write(formatted) => write
end type

type re_match
    character(len=:),allocatable :: m_string
    integer(4) :: start_pos
    integer(4) :: end_pos
    type(match_group) :: group(n_groups)
    integer(4) :: length
  contains
    procedure,private :: no_match => re_match_no_match
    
    procedure, private :: write => write_re_match
    generic :: write(formatted) => write
end type

type re_posix
    type(regex_t),private :: rgxt
    logical :: compiled = .false.
    integer(c_int),private :: c_comp_opt
    integer(c_int),private :: c_exec_opt
    character(kind=c_char,len=:),allocatable :: opts
    character(kind=c_char,len=:),allocatable :: patt
  contains
    procedure :: clean => re_posix_clean
    procedure :: match => posix_match
    procedure :: match_N => posix_match_N
    procedure :: match_all => posix_match_all
    procedure :: replace => posix_replace
    procedure :: replace_all => posix_replace_all
    
    procedure, private :: write => write_re_posix
    generic :: write(formatted) => write
end type

interface re_posix ! re_posix type
    module procedure :: re_posix_init
end interface

interface ! regex_cmod.c
    integer(c_int) function c_sizeof_regex_t(part) bind(c, name='c_sizeof_regex_t')
        import
        integer(c_int),value :: part
    end function
    
    subroutine c_set_flags(cflags,eflags,opt) bind(c, name='c_set_flags')
        import
        character(len=1,kind=C_char),intent(in) :: opt(*)
        integer(c_int),intent(out) :: cflags, eflags
    end subroutine
end interface

interface ! regex.h
    function C_regcomp(reg, pattern, flags) result(stts) bind(C,name="regcomp")
        import
        type(C_ptr), value :: reg
        character(len=1,kind=C_char), intent(in) :: pattern(*)
        integer(c_int), intent(in), value :: flags
        integer(C_int) :: stts
    end function
    
    function C_regexec(reg,string,nmatch,matches,flags) result(stts) bind(C,name="regexec")
        import
        type(C_ptr), intent(in), value :: reg
        character(len=1,kind=C_char), intent(in) :: string(*)
        type(regmatch_t), intent(inout) :: matches(*)
        integer(C_size_t), intent(in), value :: nmatch
        integer(C_int), value :: flags
        integer(C_int) :: stts
    end function
    
    function C_regerror(errcode, reg, errbuf, errbuf_size) result(regerror) bind(C,name="regerror")
        import
        integer(C_size_t) :: regerror
        integer(C_int), value :: errcode
        type(C_ptr), intent(in), value :: reg
        character(len=1,kind=C_char), intent(out) :: errbuf
        integer(C_size_t), value :: errbuf_size
    end function
    
    subroutine C_regfree(reg) bind(C,name="regfree")
        import
        type(C_ptr), intent(in), value :: reg
    end subroutine
end interface

interface ! scan_verify_smod.f90
    module pure function verify_all(string, set) result(res)
        character(len=*),intent(in) :: string
        character(len=*),intent(in) :: set
        integer(4),allocatable :: res(:)
    end function
    
    module pure function scan_all(string, set) result(res)
        character(len=*),intent(in) :: string
        character(len=*),intent(in) :: set
        integer(4),allocatable :: res(:)
    end function
    
    module pure function index_all(str, substr) result(res)
        character(len=*),intent(in) :: str
        character(len=*),intent(in) :: substr
        integer(4),allocatable :: res(:)
    end function
    
    module pure function replace(str, tarstr, substr) result(res)
        character(len=*),intent(in) :: str
        character(len=*),intent(in) :: tarstr
        character(len=*),intent(in) :: substr
        character(len=:),allocatable :: res
    end function
    
    module pure function replace_all(str, tarstr, substr) result(res)
        character(len=*),intent(in) :: str
        character(len=*),intent(in) :: tarstr
        character(len=*),intent(in) :: substr
        character(len=:),allocatable :: res
    end function
    
    module pure subroutine addsize_int4_arr(arr, nadd)
        integer(4),allocatable,intent(inout) :: arr(:)
        integer(4),intent(in) :: nadd
        integer(4),allocatable :: arr_tmp(:)
    end subroutine
    
    module pure subroutine resize_int4_arr(arr, new_size)
        integer(4),allocatable,intent(inout) :: arr(:)
        integer(4),intent(in) :: new_size
        integer(4),allocatable :: arr_tmp(:)
    end subroutine
end interface

interface assignment(=)
  procedure :: assig_re_match_char, assig_re_posix_char
end interface

interface operator(==)
  procedure :: equal_re_match_re_match
  procedure :: equal_re_posix_re_posix
end interface

interface match
    procedure :: posix_match, alone_match
end interface

interface match_N
    procedure :: posix_match_N, alone_match_N
end interface

interface match_all
    procedure :: posix_match_all, alone_match_all
end interface

interface re_replace
    procedure :: posix_replace, alone_re_replace
end interface

interface re_replace_all
    procedure :: posix_replace_all, alone_re_replace_all
end interface

! types
public :: re_posix, re_match, operator(==), assignment(=)

! this module functions
public :: match, match_N, match_all, re_replace, re_replace_all

! scan_verify_smoc.f90 funstions
public :: scan_all, verify_all, index_all, replace, replace_all

contains
!========================================================================
subroutine write_re_match(rem, unit, iotype, v_list, iostat, iomsg)
    class(re_match), intent(in) :: rem
    integer, intent(in) :: unit
    character(len=*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(len=*), intent(inout) :: iomsg
    
    write(unit, '(*(g0))', iostat=iostat) "{(",rem%start_pos,",",rem%end_pos,") ", &
        rem%length,' - "',rem%m_string,'"}'
end subroutine
!========================================================================
subroutine write_match_group(rem, unit, iotype, v_list, iostat, iomsg)
    class(match_group), intent(in) :: rem
    integer, intent(in) :: unit
    character(len=*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(len=*), intent(inout) :: iomsg
    
    write(unit, '(*(g0))', iostat=iostat) "{(",rem%start_pos,",",rem%end_pos,") ", &
        rem%length,' - "',rem%m_string,'"}'
end subroutine
!========================================================================
subroutine write_re_posix(re, unit, iotype, v_list, iostat, iomsg)
    class(re_posix), intent(in) :: re
    integer, intent(in) :: unit
    character(len=*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(len=*), intent(inout) :: iomsg

    character(len=10) :: stt
   
    if(re%compiled) then
        stt = "compiled  "
    else
        stt = "uncompiled"
    endif

    write(unit, '(*(g0))', iostat=iostat) "{",trim(stt),' regex: "', re%patt(1:len(re%patt)-1),&
        '", with options: "',re%opts(1:len(re%opts)-1),'"}'
end subroutine
!========================================================================
logical function equal_re_match_re_match(rem1, rem2) result(res)
    type(re_match),intent(in) :: rem1, rem2
    res = (rem1%m_string == rem2%m_string .and. &
           rem1%start_pos == rem2%start_pos .and. &
           rem1%end_pos == rem2%end_pos .and. &
           rem1%length == rem2%length)
end function
!=======================================================================
logical function equal_re_posix_re_posix(re1, re2) result(res)
    type(re_posix),intent(in) :: re1, re2
    res = (re1%compiled .eqv. re2%compiled .and. &
           re1%c_comp_opt == re2%c_comp_opt .and. &
           re1%c_exec_opt == re2%c_exec_opt .and. &
           re1%patt == re2%patt)
end function
!=======================================================================
subroutine assig_re_match_char(ch, rem)
    type(re_match),intent(in) :: rem
    character(len=:),allocatable,intent(out) :: ch
    ch = rem%m_string
end subroutine
!=======================================================================
subroutine assig_re_posix_char(ch, re)
    type(re_posix),intent(in) :: re
    character(len=:),allocatable,intent(out) :: ch
    ch = re%patt
end subroutine
!=======================================================================
function posix_match_all(this,str) result(res)
    class(re_posix),intent(in) :: this
    character(kind=c_char,len=*),intent(in) :: str
    type(re_match),allocatable :: res(:)    
    integer(4) i, start, n
    
    n = 10
    
    if(.not. this%compiled) then
        write(*,*) "Regex is not compiled"
        return
    endif
    
    call addsize_re_match_arr(res,n)

    start = 1
    i = 0
    do
        i = i + 1
        if(i > size(res)) call addsize_re_match_arr(res,n)
        res(i) = this%match(str(start:))
        start = start + res(i)%end_pos
        if(start > len(str) .or. res(i)%length == 0) exit
    end do
    
    call resize_re_match_arr(res, i-1)
    
    contains
    !-------------------------------------------------------------------
    pure subroutine addsize_re_match_arr(arr, nadd)
        type(re_match),allocatable,intent(inout) :: arr(:)
        integer(4),intent(in) :: nadd
        type(re_match),allocatable :: arr_tmp(:)
        
        integer(4) old_size
        
        if(.not. allocated(arr)) then
            allocate(arr(nadd))
        else
            old_size = size(arr)
            call move_alloc(from = arr, to = arr_tmp)
            allocate(arr(old_size+nadd))
            arr(1:old_size) = arr_tmp
            deallocate(arr_tmp)
        endif
    end subroutine
    !-------------------------------------------------------------------
    pure subroutine resize_re_match_arr(arr, new_size)
        type(re_match),allocatable,intent(inout) :: arr(:)
        integer(4),intent(in) :: new_size
        type(re_match),allocatable :: arr_tmp(:)
        
        integer(4) old_size
        
        if(.not. allocated(arr)) then
            allocate(arr(new_size))
        else
            old_size = size(arr)
            call move_alloc(from = arr, to = arr_tmp)
            allocate(arr(new_size))
            if(new_size > old_size) then
                 arr(1:old_size) = arr_tmp
            else
                 arr = arr_tmp(1:new_size)
            endif
            deallocate(arr_tmp)
        endif
    end subroutine
end function
!=======================================================================
function posix_match_N(this,str,n) result(res)
    class(re_posix),intent(in) :: this
    integer(4), intent(in) :: n
    character(kind=c_char,len=*),intent(in) :: str
    type(re_match),allocatable :: res(:)    
    integer(4) i, start
    
    if(.not. this%compiled) then
        write(*,*) "Regex is not compiled"
        return
    endif
    
    allocate(res(n))

    start = 1
    do i = 1,n
        if(start <= len(str)) then
            res(i) = this%match(str(start:))
            start = start + res(i)%end_pos
        else
            call res(i)%no_match()
        end if
    end do
    
end function
!=======================================================================
type(re_match) function alone_match(str, patt, opt) result(res)
    character(len=*),intent(in) :: str
    character(len=*),intent(in) :: patt
    character(len=*),optional,intent(in) :: opt
    
    character(len=:),allocatable :: options
    type(re_posix) :: re
    
    if(present(opt)) then
        options = opt
    else
        options = ""
    endif
    
    re = re_posix(patt,options)
    res = re%match(str)
    call re%clean()
    
end function
!=======================================================================
function alone_match_all(str, patt, opt) result(res)
    character(len=*),intent(in) :: str
    character(len=*),intent(in) :: patt
    character(len=*),optional,intent(in) :: opt
    type(re_match),allocatable :: res(:)
    
    character(len=:),allocatable :: options
    type(re_posix) :: re
        
    if(present(opt)) then
        options = opt
    else
        options = ""
    endif
    
    re = re_posix(patt,options)
    res = re%match_all(str)
    call re%clean()
    
end function
!=======================================================================
function alone_match_N(str, patt, n, opt) result(res)
    character(len=*),intent(in) :: str
    character(len=*),intent(in) :: patt
    integer(4),intent(in) :: n
    character(len=*),optional,intent(in) :: opt
    type(re_match),allocatable :: res(:)
    
    character(len=:),allocatable :: options
    type(re_posix) :: re
        
    if(present(opt)) then
        options = opt
    else
        options = ""
    endif
    
    re = re_posix(patt,options)
    res = re%match_n(str,n)
    call re%clean()
    
end function
!=======================================================================
function alone_re_replace_all(str, patt, substr, opt) result(res)
    character(len=*),intent(in) :: str
    character(len=*),intent(in) :: patt
    character(len=*),intent(in) :: substr
    character(len=*),optional,intent(in) :: opt
    character(len=:),allocatable :: res
    
    integer(4) ofs
    character(len=:),allocatable :: options
    type(re_posix) :: re
    type(re_match) :: rem
        
    if(present(opt)) then
        options = opt
    else
        options = ""
    endif
    
    re = re_posix(patt,options)
    res = ""
    ofs = 1
    do
        rem = re%match(str(ofs:))
        if(rem%start_pos > 1) then
          res = res // str(ofs:rem%start_pos+ofs-2) // substr
          ofs = ofs + rem%start_pos + rem%length - 1
        else if(rem%start_pos == 1) then
          res = res // substr
          ofs = ofs + rem%length
        else
          res = res // str(ofs:)
          exit
        endif
    enddo
    
    call re%clean()
end function
!=======================================================================
function alone_re_replace(str, patt, substr, opt) result(res)
    character(len=*),intent(in) :: str
    character(len=*),intent(in) :: patt
    character(len=*),intent(in) :: substr
    character(len=*),optional,intent(in) :: opt
    character(len=:),allocatable :: res
    
    character(len=:),allocatable :: options
    type(re_posix) :: re
    type(re_match) :: rem
        
    if(present(opt)) then
        options = opt
    else
        options = ""
    endif
    
    re = re_posix(patt,options)
    rem = re%match(str)
    
    if(rem%start_pos > 1) then
      res = str(1:rem%start_pos-1) // substr // str(rem%start_pos+rem%length:)
    else if(rem%start_pos == 1) then
      res = substr // str(rem%start_pos+rem%length:)
    else
      res = str
    endif
    
    call re%clean()
end function
!=======================================================================
function posix_replace(this,str,substr) result(res)
    class(re_posix),intent(in) :: this
    character(len=*),intent(in) :: str
    character(len=*),intent(in) :: substr
    character(len=:),allocatable :: res
    
    type(re_match) :: rem
    
    rem = this%match(str)
    
    if(rem%start_pos > 1) then
      res = str(1:rem%start_pos-1) // substr // str(rem%start_pos+rem%length:)
    else if(rem%start_pos == 1) then
      res = substr // str(rem%start_pos+rem%length:)
    else
      res = str
    endif
    
end function
!=======================================================================
function posix_replace_all(this,str,substr) result(res)
    class(re_posix),intent(in) :: this
    character(len=*),intent(in) :: str
    character(len=*),intent(in) :: substr
    character(len=:),allocatable :: res
    
    integer(4) :: ofs
    type(re_match) :: rem
    
    res = ""
    ofs = 1
    do
        rem = this%match(str(ofs:))
        if(rem%start_pos > 1) then
          res = res // str(ofs:rem%start_pos+ofs-2) // substr
          ofs = ofs + rem%start_pos + rem%length - 1
        else if(rem%start_pos == 1) then
          res = res // substr
          ofs = ofs + rem%length
        else
          res = res // str(ofs:)
          exit
        endif
    enddo
    
end function
!=======================================================================
type(re_match) function posix_match(this,str) result(res)
    class(re_posix),intent(in) :: this
    character(len=*),intent(in) :: str
    
    integer(c_int) ret
    type(regmatch_t) pmatch(0:n_groups-1)
    character(kind=c_char,len=115) :: errorbuf = ""
    character(kind=c_char,len=1) :: tmp
    integer(c_size_t) :: regerror
    integer(c_size_t) :: nmch = int(n_groups,kind=c_size_t)
    integer(4) :: i
    
    call res%no_match()
    
    if(.not. this%compiled) then
        write(*,*) "Regex is not compiled"
        return
    endif

    ret = c_regexec(this%rgxt%rgx, str//C_NULL_char, nmch, pmatch, this%c_exec_opt);
    
    if (ret == 0) then
        res%start_pos = pmatch(0)%rm_so+1
        res%end_pos = pmatch(0)%rm_eo
        res%m_string = str(res%start_pos:res%end_pos)
        res%length = pmatch(0)%rm_eo - pmatch(0)%rm_so
        
        do i = 1, n_groups
            res%group(i)%start_pos = pmatch(i-1)%rm_so+1
            res%group(i)%end_pos = pmatch(i-1)%rm_eo
            res%group(i)%m_string = str(res%group(i)%start_pos:res%group(i)%end_pos)
            res%group(i)%length = pmatch(i-1)%rm_eo - pmatch(i-1)%rm_so
        enddo
    else if(ret > 1) then
        regerror = C_regerror(ret, this%rgxt%rgx, errorbuf, c_sizeof(tmp))
        write(*,'(*(g0))') "regex.h > regexec(): error #",ret,": ",trim(errorbuf)
        res%start_pos = -1;
        res%end_pos = -1;
        res%length = 0;
    endif
    
    !write(*,*) res%start_pos, res%end_pos, res%length, res%m_string
    
end function
!=======================================================================
type(re_posix) function re_posix_init(patt,opt) result(re)
    character(len=*),intent(in) :: patt
    character(len=*),intent(in),optional :: opt
    
    integer(c_int) :: i
    integer(C_SIGNED_CHAR) :: tmp
    character(kind=c_char,len=115) :: errorbuf = ""
    character(kind=c_char,len=1) :: tmp2
    integer(c_size_t) :: regerror
    
    re%patt = patt//C_NULL_char
    
    if(present(opt)) then
        re%opts = opt//C_NULL_char
    else
        re%opts = C_NULL_char
    endif
    
    call c_set_flags(re%c_comp_opt, re%c_exec_opt, re%opts)
    
    allocate( &
        re%rgxt%buffer( &
            c_sizeof_regex_t( &
                int( &
                    c_sizeof(tmp),kind=c_int &
                   ) &
                ) &
            ) &
        )
        
    re%rgxt%rgx = c_loc(re%rgxt%buffer(1))
    
    i = c_regcomp(re%rgxt%rgx, re%patt, re%c_comp_opt)
    
    if(i /= 0) then
        regerror = C_regerror(i, re%rgxt%rgx, errorbuf, 115*c_sizeof(tmp2))
        write(*,'(*(g0))') "Error #",i,": ",trim(errorbuf)
    else
        re%compiled = .true.
    endif
    
end function
!=======================================================================
subroutine re_match_no_match(this)
    class(re_match) :: this
    this%m_string=""
    this%start_pos=-1
    this%end_pos=-1
    this%length=0
end subroutine
!=======================================================================
subroutine re_posix_clean(this)
    class(re_posix), intent(inout) :: this
    
    call this%rgxt%regex_t_final()
    this%compiled = .false.
    this%c_comp_opt = int(0,kind=c_int)
    this%c_exec_opt = int(0,kind=c_int)
    if(allocated(this%opts)) deallocate(this%opts)
    if(allocated(this%patt)) deallocate(this%patt)

end subroutine
!=======================================================================
subroutine regex_t_final(this)
    class(regex_t), intent(inout) :: this
    if(associated(this%buffer)) then
      call C_regfree(this%rgx)
      deallocate(this%buffer)
    endif
end subroutine
end module
