submodule (regex_mod) scan_verify

    use iso_c_binding
    use iso_fortran_env

    implicit none
   
contains
!=======================================================================
module pure function verify_all(string, set) result(res)
    character(len=*),intent(in) :: string
    character(len=*),intent(in) :: set
    integer(4),allocatable :: res(:)

    integer(4) ns, ofst, str_size
    
    call addsize_int4_arr(res, 10)
    ns = 0
    ofst = 1
    str_size = len(string)
    do
        ns = ns + 1
        if(ns > size(res)) call addsize_int4_arr(res, 10)
        res(ns) = verify(string(ofst:),set)
        if(res(ns) == 0) exit
        res(ns) = res(ns) + ofst -1
        ofst = res(ns) + 1
        if(ofst > str_size) exit
    enddo
    
    call resize_int4_arr(res, ns - 1)
    
end function
!=======================================================================
module pure function scan_all(string, set) result(res)
    character(len=*),intent(in) :: string
    character(len=*),intent(in) :: set
    integer(4),allocatable :: res(:)

    integer(4) ns, ofst, str_size
    
    call addsize_int4_arr(res, 10)
    ns = 0
    ofst = 1
    str_size = len(string)
    do
        ns = ns + 1
        if(ns > size(res)) call addsize_int4_arr(res, 10)
        res(ns) = scan(string(ofst:),set)
        if(res(ns) == 0) exit
        res(ns) = res(ns) + ofst -1
        ofst = res(ns) + 1
        if(ofst > str_size) exit
    enddo
    
    call resize_int4_arr(res, ns - 1)
    
end function
!=======================================================================
module pure function replace_all(str, tarstr, substr) result(res)
    character(len=*),intent(in) :: str
    character(len=*),intent(in) :: tarstr
    character(len=*),intent(in) :: substr
    character(len=:),allocatable :: res
    
    integer(4) i, ofs, tar_len
   
    tar_len = len(tarstr)
    
    res = ""
    ofs = 1
    do
        i = index(str(ofs:), tarstr)
        if(i>1) then
          res = res // str(ofs:i+ofs-2) // substr
          ofs = ofs + i + tar_len - 1
        else if(i==1) then
          res = res // substr
          ofs = ofs + tar_len
        else
          res = res // str(ofs:)
          exit
        endif
    enddo
        
end function
!=======================================================================
module pure function replace(str, tarstr, substr) result(res)
    character(len=*),intent(in) :: str
    character(len=*),intent(in) :: tarstr
    character(len=*),intent(in) :: substr
    character(len=:),allocatable :: res
    
    integer(4) i
    
    i = index(str, tarstr)
    if(i>1) then
      res = str(1:i-1) // substr // str(i+len(tarstr):)
    else if(i==1) then
      res = substr // str(i+len(tarstr):)
    else
      res = str
    endif
end function
!=======================================================================
module pure function index_all(str, substr) result(res)
    character(len=*),intent(in) :: str
    character(len=*),intent(in) :: substr
    integer(4),allocatable :: res(:)

    integer(4) ns, ofst, str_size, substr_size
    
    call addsize_int4_arr(res, 10)
    ns = 0
    ofst = 1
    str_size = len(str)
    substr_size = len(substr)
    do
        ns = ns + 1
        if(ns > size(res)) call addsize_int4_arr(res, 10)
        res(ns) = index(str(ofst:),substr)
        if(res(ns) == 0) exit
        res(ns) = res(ns) + ofst -1
        ofst = res(ns) + substr_size
        if(ofst > str_size) exit
    enddo
    
    call resize_int4_arr(res, ns - 1)
    
end function
!=======================================================================
module pure subroutine addsize_int4_arr(arr, nadd)
    integer(4),allocatable,intent(inout) :: arr(:)
    integer(4),intent(in) :: nadd
    integer(4),allocatable :: arr_tmp(:)
    
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
!=======================================================================
module pure subroutine resize_int4_arr(arr, new_size)
    integer(4),allocatable,intent(inout) :: arr(:)
    integer(4),intent(in) :: new_size
    integer(4),allocatable :: arr_tmp(:)
    
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
end submodule
