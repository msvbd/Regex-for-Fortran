program regex_test
use iso_fortran_env
use iso_c_binding
use regex_mod
implicit none
  
  character(len=:),allocatable :: string
  character(len=:),allocatable :: string2
  
  character(len=:),allocatable :: my_match
  character(len=:),allocatable :: my_regex
  
  integer(4) :: i

  type(re_posix) :: re
  type(re_match) :: match_scalar
  type(re_match),allocatable :: match_array(:)
    
  string = "asdf ghjkl asdf ghjkl"
  string2 = "assssdf ghjkkkkl asddddf ghjjjjkl"
  
  write(*,*) "scan example:"
  write(*,*) scan(string,"aj")
  write(*,*) scan_all(string,"aj")
  write(*,*) string
  write(*,*) ( merge(string(i:i)," ", any(scan_all(string,"aj")==i) ) ,i = 1, len(string))
  
  write(*,*) "---------------------------------------------"
  
  write(*,*) "verify example:"
  write(*,*) verify(string,"adhjk")
  write(*,*) verify_all(string,"adhjk")
  write(*,*) string
  write(*,*) ( merge(string(i:i)," ", any(verify_all(string,"adhjk")==i) ) ,i = 1, len(string))
  
  write(*,*) "---------------------------------------------"
  
  write(*,*) "index example:"
  write(*,*) index(string,"f g")
  write(*,*) index_all(string,"f g")
  write(*,*) string
  write(*,*) ( merge(string(i:i)," ", any(index_all(string,"f g")==i) ) ,i = 1, len(string))
  
  write(*,*) "---------------------------------------------"
  
  write(*,*) "replace example:"
  write(*,*) string
  write(*,*) replace(string,"f g","12 345")
  write(*,*) replace_all(string,"f g","123 45")
  
  write(*,*) "---------------------------------------------"
  
  write(*,*) "regex crete example"
  re = re_posix("[skdj]+","xn")
  write(*,*) re
  
  write(*,*) "---------------------------------------------"
  
  write(*,*) "regex match example:"
  write(*,*) string
  match_scalar = re%match(string)
  write(*,*) match_scalar
  
  match_scalar = match(re,string)
  write(*,*) match_scalar
  
  match_scalar = match(string, "[skdj]+","xn")
  write(*,*) match_scalar
  
  write(*,*) "---------------------------------------------"
  
  write(*,*) "regex match_N example:"
  write(*,*) string
  match_array = re%match_N(string, 5)
  write(*,*) match_array
  
  match_array = match_N(re,string, 5)
  write(*,*) match_array
  
  match_array = match_N(string, "[skdj]+",5,"xn")
  write(*,*) match_array
  
  write(*,*) "---------------------------------------------"
  
  write(*,*) "regex match_all example:"
  write(*,*) string
  match_array = re%match_all(string)
  write(*,*) match_array
  
  match_array = match_all(re,string)
  write(*,*) match_array
  
  match_array = match_all(string, "[skdj]+","xn")
  write(*,*) match_array
  
  write(*,*) "---------------------------------------------"
  
  write(*,*) "regex replace example:"
  write(*,*) string2
  write(*,*) re%replace(string2, "<......>")
  write(*,*) re_replace(re, string2, "<......>")
  write(*,*) re_replace(string2, "[skdj]+","<......>","xn")
  
  write(*,*) "---------------------------------------------"
  
  write(*,*) "regex replace_all example:"
  write(*,*) string2
  write(*,*) re%replace_all(string2, "<......>")
  write(*,*) re_replace_all(string2, "[skdj]+","<......>","xn")
  write(*,*) re_replace_all(re,string2, "<......>")
  
  write(*,*) "---------------------------------------------"
  
  write(*,*) "regex assignment (=) example:"
  re = re_posix("[skdj]+","xn")
  write(*,*) re
  write(*,*) string
  match_scalar = re%match(string)
  write(*,*) match_scalar
  
  my_regex = re
  write(*,*) my_regex
  
  my_match = match_scalar
  write(*,*) my_match
  
  write(*,*) "---------------------------------------------"
  
  write(*,*) "regex operator (==) example:"
  re = re_posix("[skdj]+","xn")
  match_scalar = re%match(string)
  
  write(*,*) match_scalar == match(string, "[skdj]+","xn")
  write(*,*) re == re_posix("[skdj]+","xn")
  write(*,*)
  write(*,*) match_scalar == match(string, "[0-9]+","xn")
  write(*,*) re == re_posix("[0-9]+","xn")
  write(*,*)
  write(*,*) match_scalar == match(string, "[skdj]+","xni")
  write(*,*) re == re_posix("[skdj]+","xni")
  
  write(*,*) "---------------------------------------------"
  
  write(*,*) "regex match group example:"
  
  match_scalar = match("asdf@jklp.test", "([a-zA-Z]+)@(([a-zA-Z]+).([a-zA-Z]+))","xn")
  write(*,*) match_scalar
  write(*,*) "group: 1",match_scalar%group(1)
  write(*,*) "group: 2",match_scalar%group(2)
  write(*,*) "group: 3",match_scalar%group(3)
  write(*,*) "group: 4",match_scalar%group(4)
  write(*,*) "group: 5",match_scalar%group(5)
  write(*,*) "group: 6",match_scalar%group(6)
  write(*,*) "group: 7",match_scalar%group(7)
  
  
  
end program
