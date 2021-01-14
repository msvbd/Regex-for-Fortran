## Regex for Fortran

Functional & Object-oriented regex wrapper for Fortran. 

* [Getting started](#getting-started)
* [Why this module?](#why-this-module)
* [What's included?](#whats-included)
* [Example usage](#example-usage)
	* [`scan_all` example](#scan_all-example)
	* [`verify_all` example](#verify_all-example)
	* [`index_all` example](#index_all-example)
	* [`replace` (no regex) example](#replace-no-regex-example)
	* [`create `re_posix` type example](#create-re_posix-type-example)
	* [`match` example](#match-example)
	* [`match_N` example](#match_N-example)
	* [`match_all` example](#match_all-example)
	* [`re_replace` example](#re_replace-example)
	* [`re_replace_all` example](#re_replace_all-example)
	* [`operator (==)` example](#operator==example)
	* [`assignment (=)` example](#assignment=example)
* [Regex options](#regex-options)
* [References](#references)

## Getting started
You should have regex.h library.

Installation:
```
make
```
And then you can include the module in your code:
```
use regex_mod
```
Don't forget to compile with: `-lregex -Lpath/to/lib -Ipath/to/include`

## Why this module?

Module contains extension for intrinsic functions (`scan_all`, `verify_all`,`index_all`) and 
standard regex functions (`match`, `match_all`, `replace`, `replace_all`, ...).
And user-friendly framework based on C interoperability with 
[regex.h](http://man7.org/linux/man-pages/man3/regex.3.html) library wraps all of them.
The library supports [POSIX regular expression](https://en.wikipedia.org/wiki/Regular_expression#POSIX_basic_and_extended).

## What's included?

Types:
* `re_posix` - Regex type
	* Variables:
		* `compiled` - logical - Equal `true` if `patt` is compiled.
		* `opts` - character - Contains options.
		* `patt` - character - Contains regex pattern.
	* Methods:
		* `match` - match the first occurrence - Return `re_match` type.
		* `match_N` - match first N occurrences - Return an array of `re_match` types.
		* `match_all` - match all occurrences - Return array of `re_match` types.
		* `replace` - replace the first occurrence - Return character.
		* `replace_all` - replace all occurrences  - Return character.
		* `test_match` - Return `.true.` if regex is matched and `.false.` if not.
		* `clean` - deallocate
		
* `re_match` - Match type
	* Variables:
		* `m_string` - A matched substring.
		* `start_pos` - The start position of the matched substring in the parent string.
		* `end_pos` - The end position of the matched substring in the parent string.
		* `length` - Length of the matched substring.
		* `group` - array(50) of `match_group` types.
		
* `match_group` - Group type
	* Variables:
		* `m_string` - A matched substring.
		* `start_pos` - The start positiont of matched substring in the parent string.
		* `end_pos` - The end positiont of matched substring in the parent string.
		* `length` - Length of matched substring.
		
_Note: Both types have overloaded `write(formatted)` and `operator(==)`_

stand-alone methods:
* `scan_all` - extended intrinsic function `scan` - Returns array of integers.
* `verify_all` - extended intrinsic function `verify` - Returns array of integers.
* `index_all` - extended intrinsic function `index` - Returns array of integers.
* `match` - match the first occurrence - Returns `re_match` type.
* `match_N` - match the first N occurrences - Returns array of `re_match` types.
* `match_all` - match all occurrences - Returns array of `re_match` types.
* `test_match` - Return `.true.` if regex is matched and `.false.` if not.
* `replace` - replace the first occurrence found by `index` intrinsic - !!! It doesn't use regex !!! - Returns character.
* `replace_all` - replace all occurrences found by `index` intrinsic - !!! It doesn't use regex !!! - Returns character.
* `re_replace` - replace the first occurrence found by `match` - Returns character.
* `re_replace_all` - replace all occurrences found by `match` - Returns character.

## Example usage
You can found examples in `regex_test.f90`.
_Note: Sorry for silly examples. I have it on the list._
  
### `scan_all` example
The intrinsic `scan` found the first occurrence one of the character ("a" or "j") in the `string`. 
While `scan_all` returns an returns array of all occurrences. 
```
  string = "asdf ghjkl asdf ghjkl"
  write(*,*) scan(string,"aj")
  write(*,*) scan_all(string,"aj")
  write(*,*) string
  write(*,*) ( merge(string(i:i)," ", any(scan_all(string,"aj")==i) ) ,i = 1, len(string))
```
return:
```
           1
           1           8          12          19
 asdf ghjkl asdf ghjkl
 a      j   a      j  
```
### `verify_all` example
The intrinsic `verify` found position of the first character in the `string` which is not
 one of those characters (no "a", nor "d", nor "h", nor "j", nor "k"). 
While `verify_all` returns an array of integers of all "non-occurrences". 
```
  string = "asdf ghjkl asdf ghjkl"
  write(*,*) verify(string,"adhjk")
  write(*,*) verify_all(string,"adhjk")
  write(*,*) string
  write(*,*) ( merge(string(i:i)," ", any(verify_all(string,"adhjk")==i) ) ,i = 1, len(string))
```
return:
```
           2
           2           4           5           6          10          11          13          15          16          17
 asdf ghjkl asdf ghjkl
  s f g   l  s f g
```
### `index_all` example
The intrinsic `index` found the first occurrence of a substring ("f g") in the `string`. 
While `index_all` returns an array of integers of all occurrences. 
```
  string = "asdf ghjkl asdf ghjkl"
  write(*,*) index(string,"f g")
  write(*,*) index_all(string,"f g")
  write(*,*) string
  write(*,*) ( merge(string(i:i)," ", any(index_all(string,"f g")==i) ) ,i = 1, len(string))
```
return:
```
           4
           4          15
 asdf ghjkl asdf ghjkl
    f          f    
```
### `replace` (no regex) example
`replace` found substring ("f g") and replaces it by other substrings ("12 345")
`replace_all` replace all occurrences. 
```
  string = "asdf ghjkl asdf ghjkl"
  write(*,*) string
  write(*,*) replace(string,"f g","12 345")
  write(*,*) replace_all(string,"f g","123 45")
```
return:
```
 replace example:
 asdf ghjkl asdf ghjkl
 asd12 345hjkl asdf ghjkl
 asd123 45hjkl asd123 45hjkl

```
### create `re_posix` type example
`re_posix` is type and constructor function. Its interface is:
```
type(re_posix) function re_posix_init(patt,opt) result(re)
    character(len=*),intent(in) :: patt
    character(len=*),intent(in),optional :: opt
end function
```
where `patt` is a regex pattern and `opt` are options.
Example:
```
  type(re_posix) :: re
  re = re_posix("[skdj]+","xn")
  write(*,*) re
```
return:
```
 {compiled regex: "[skdj]+", with options: "xn"}
```
### `match` example
You have three choices on how to match a regex pattern in a `string`.
1. Create `re_posix` and call its method `%match` with `string` as an argument.
1. Create `re_posix` and apply to it `match` function with `re_posix` and `string` as arguments
1. Use `match` function with these arguments: `string`, `pattern` and regex options

All of them retune a `re_match` type variable. If no macht then the variables of `re_match` type have 
values: `m_string = ""`, `start_pos = -1`, `end_pos = -1` and `length = 0`.

```
  type(re_match) :: match_scalar
  type(re_posix) :: re
  
  re = re_posix("[skdj]+","xn")
  
  string = "asdf ghjkl asdf ghjkl"
  write(*,*) "regex match example:"
  write(*,*) string
  
  match_scalar = re%match(string)
  write(*,*) match_scalar
  
  match_scalar = match(re,string)
  write(*,*) match_scalar
  
  match_scalar = match(string, "[skdj]+","xn")
  write(*,*) match_scalar
```
return:
```
 asdf ghjkl asdf ghjkl
 {(2,3) 2 - "sd"}
 {(2,3) 2 - "sd"}
 {(2,3) 2 - "sd"}
```
### `match_group` example
Group(1) is equal to full match. Each other group is match of one of any pattern between brackets `(..)`.
In example is group(1) the match of the pattern: `([a-zA-Z]+)@([a-zA-Z]+).([a-zA-Z]+)`; 
group(2) the match of the pattern :`[a-zA-Z]+`; group(3) the match of the pattern :`(([a-zA-Z]+).([a-zA-Z]+))`; 
group(4) and group(5) are the matches of the pattern: `[a-zA-Z]+` (subpatterns of the previous pattern).
```
  type(re_match) :: match_scalar
  
  match_scalar = match("asdf@jklp.test", "([a-zA-Z]+)@(([a-zA-Z]+).([a-zA-Z]+))","xn")
  
  write(*,*) match_scalar
  write(*,*) "group: 1",match_scalar%group(1)
  write(*,*) "group: 2",match_scalar%group(2)
  write(*,*) "group: 3",match_scalar%group(3)
  write(*,*) "group: 4",match_scalar%group(4)
  write(*,*) "group: 5",match_scalar%group(5)
  write(*,*) "group: 6",match_scalar%group(6)
  write(*,*) "group: 7",match_scalar%group(7)
```
return:
```
 {(1,14) 14 - "asdf@jklp.test"}
 group: 1 {(1,14) 14 - "asdf@jklp.test"}
 group: 2 {(1,4) 4 - "asdf"}
 group: 3 {(6,14) 9 - "jklp.test"}
 group: 4 {(6,9) 4 - "jklp"}
 group: 5 {(11,14) 4 - "test"}
 group: 6 {(0,-1) 0 - ""}
 group: 7 {(0,-1) 0 - ""}
```
### `match_N` example
`match_N` is the same as the `match` but returns an array of `re_match` types with size `N`.
It needs argument `N`.
```
  type(re_match),allocatable :: match_array(:)
  type(re_posix) :: re
  
  string = "asdf ghjkl asdf ghjkl"
  write(*,*) string
  
  match_array = re%match_N(string, 5)
  write(*,*) match_array
  
  match_array = match_N(re,string, 5)
  write(*,*) match_array
  
  match_array = match_N(string, "[skdj]+",5,"xn")
  write(*,*) match_array
```
return:
```
 asdf ghjkl asdf ghjkl
 {(2,3) 2 - "sd"} {(5,6) 2 - "jk"} {(4,5) 2 - "sd"} {(5,6) 2 - "jk"} {(-1,-1) 0 - ""}
 {(2,3) 2 - "sd"} {(5,6) 2 - "jk"} {(4,5) 2 - "sd"} {(5,6) 2 - "jk"} {(-1,-1) 0 - ""}
 {(2,3) 2 - "sd"} {(5,6) 2 - "jk"} {(4,5) 2 - "sd"} {(5,6) 2 - "jk"} {(-1,-1) 0 - ""}
```
### `match_all` example
`match_all` is same as the `match`, but returns an array of `re_match` types. It contains 
all matches of the pattern in a `string`.
```
  type(re_match),allocatable :: match_array(:)
  type(re_posix) :: re
  
  string = "asdf ghjkl asdf ghjkl"
  write(*,*) string
  
  match_array = re%match_all(string)
  write(*,*) match_array
  
  match_array = match_all(re,string)
  write(*,*) match_array
  
  match_array = match_all(string, "[skdj]+","xn")
  write(*,*) match_array
```
return:
```
 asdf ghjkl asdf ghjkl
 {(2,3) 2 - "sd"} {(5,6) 2 - "jk"} {(4,5) 2 - "sd"} {(5,6) 2 - "jk"}
 {(2,3) 2 - "sd"} {(5,6) 2 - "jk"} {(4,5) 2 - "sd"} {(5,6) 2 - "jk"}
 {(2,3) 2 - "sd"} {(5,6) 2 - "jk"} {(4,5) 2 - "sd"} {(5,6) 2 - "jk"}

```
### `re_replace` example
You have three choices on how to replace the regex pattern in a `string`.
1. Create `re_posix` and call its method `%replace` with `string` and substring as arguments
1. Create `re_posix` and apply to it `re_replace` function with `re_posix`, `string` and substring as arguments
1. use `re_replace` function with these arguments: `string`, `pattern`, substring and regex options
```
  type(re_posix) :: re
  
  string2 = "assssdf ghjkkkkl asddddf ghjjjjkl"
  write(*,*) string2
  write(*,*) re%replace(string2, "<......>")
  write(*,*) re_replace(re, string2, "<......>")
  write(*,*) re_replace(string2, "[skdj]+","<......>","xn")
```
return:
```
 assssdf ghjkkkkl asddddf ghjjjjkl
 a<......>f ghjkkkkl asddddf ghjjjjkl
 a<......>f ghjkkkkl asddddf ghjjjjkl
 a<......>f ghjkkkkl asddddf ghjjjjkl
```
### `re_replace_all` example
`replace_all` is same as `re_replace` but replaces all matches of the pattern.
```
  type(re_posix) :: re
  
  string2 = "assssdf ghjkkkkl asddddf ghjjjjkl"
  write(*,*) string2
  write(*,*) re%replace_all(string2, "<......>")
  write(*,*) re_replace_all(string2, "[skdj]+","<......>","xn")
  write(*,*) re_replace_all(re,string2, "<......>")
```
return:
```
 assssdf ghjkkkkl asddddf ghjjjjkl
 a<......>f gh<......>l a<......>f gh<......>l
 a<......>f gh<......>l a<......>f gh<......>l
 a<......>f gh<......>l a<......>f gh<......>l
```
### `operator (==)` example
You can compare `re_posix` and `re_match`.
`re_posix` are equal if its `%compiled`, `%c_comp_opt`, `%c_exec_opt` and `%patt` variables are equal.
`re_match` are equal if its `%m_string`, `%start_pos`, `%end_pos` and `%length` variables are equal.
```
  type(re_match) :: match_scalar
  type(re_posix) :: re
  
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
```
return:
```
 T
 T

 F
 F

 T
 F
```
### `assignment (=)` example
There is the overloaded operator for character variable.
_Note: I'm not sure it is useful._
```
  character(len=:),allocatable :: my_match
  character(len=:),allocatable :: my_regex
  type(re_match) :: match_scalar
  type(re_posix) :: re
  
  re = re_posix("[skdj]+","xn")
  write(*,*) re
  
  string = "asdf ghjkl asdf ghjkl"
  
  write(*,*) string
  match_scalar = re%match(string)
  write(*,*) match_scalar
  
  my_regex = re
  write(*,*) my_regex
  
  my_match = match_scalar
  write(*,*) my_match
```
return:
```
 {compiled regex: "[skdj]+", with options: "xn"}
 asdf ghjkl asdf ghjkl
 {(2,3) 2 - "sd"}
 [skdj]+
 sd
```

## Regex options
There are few options for regex.
It is good described on [regex.h](http://man7.org/linux/man-pages/man3/regex.3.html)
* `x` -> REG_EXTENDED
* `i` -> REG_ICASE
* `n` -> REG_NEWLINE
* `l` -> [setlocale(LC_ALL,"")](http://man7.org/linux/man-pages/man3/setlocale.3.html)
* `b` -> REG_NOTBOL
* `e` -> REG_NOTEOL

## References

* [regex.h](http://man7.org/linux/man-pages/man3/regex.3.html)
* [POSIX regular expresion](https://en.wikipedia.org/wiki/Regular_expression#POSIX_basic_and_extended)
