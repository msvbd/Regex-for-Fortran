## Regex for Fortran

Functional & Object oriented regex wrapper for Fortran. 

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
You need regex.h library.

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
clasical regex functions (`match`, `match_all`, `replace`, `replace_all`, ...).
And user friendly framework based on C interoperability with 
[regex.h](http://man7.org/linux/man-pages/man3/regex.3.html) library wraps all of them.
The library support [POSIX regular expresion](https://en.wikipedia.org/wiki/Regular_expression#POSIX_basic_and_extended).

## What's included?

Types:
* `re_posix` - Regex type
	* Variables:
		* `compiled` - logical variable equal `true` if `patt` is compiled
		* `opts` - character contains options
		* `patt` - character contains regex patter
	* Methods:
		* `match` - match first occurrence - Return `re_match` type
		* `match_N` - match first N occurrences - Return array of `re_match` types
		* `match_all` - match all occurrences - Return array of `re_match` types
		* `replace` - replace first occurrence - Return character
		* `replace_all` - replace all occurrences  - Return character
		* `clean` - deallocate
		
* `re_match` - Match type
	* Variables:
		* `m_string` - mached substring
		* `start_pos` - start positiont of mached substring in parent string
		* `end_pos` - end positiont of mached substring in parent string
		* `length` - length of mached substring
		
_Note: Both types have overloaded `write(formatted)` and `operator(==)`_

stand-alone methods:
* `scan_all` - extended intrinsic function `scan` - Return integer array
* `verify_all` - extended intrinsic function `verify` - Return integer array
* `index_all` - extended intrinsic function `index` - Return integer array
* `match` - match first occurrence  - Return `re_match` type
* `match_N` - match first N occurrences - Return array of `re_match` types
* `match_all` - match all occurrences - Return array of `re_match` types
* `replace` - replace first occurrence found by `index` intrinsic - !!! It doesn't use regex !!! - Return character
* `replace_all` - replace all occurrences found by `index` intrinsic - !!! It doesn't use regex !!! - Return character
* `re_replace` - replace first occurrence found by `match` - Return character
* `re_replace_all` - replace all occurrences found by `match` - Return character

## Example usage
You can found examples in `regex_test.f90`.
_Note: Sorry for silly examples. I have it on the list._
  
### `scan_all` example
Intrinsic `scan` found first occurrence one of the character ("a" or "j") in `string`. 
While `scan_all` return integer array of all occurrences. 
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
Intrinsic `verify` found position of first character in `string` which is not
 one of the character (no "a", nor "d", nor "h", nor "j", nor "k"). 
While `verify_all` return integer array of all "non-occurrences". 
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
Intrinsic `index` found first occurrence of substring ("f g") in `string`. 
While `index_all` return integer array of all occurrences. 
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
`replace` found substring ("f g") and replaces it by other substring ("12 345")
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
where `patt` is regex pattern and `opt` are options.
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
You have three choices how to match regex pattern in `string`.
1. Create `re_posix` and call its method `%match` with `string` as argument
1. Create `re_posix` and apply to it `match` function with `re_posix` and `string` as arguments
1. use `match` function with this arguments: `string`, `pattern` and regex options

All of them retune `re_match` type. If no macht the variables of `re_match` type have 
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
### `match_N` example
`match_N` is same as `match` but returns array of `re_match` types with size `N`.
And it need argument `N`
If 
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
`match_all` is same as `match` but returns array of `re_match` types contains 
all matches of pattern in `string`.
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
You have three choices how to replace regex pattern in `string`.
1. Create `re_posix` and call its method `%replace` with `string` and substring as arguments
1. Create `re_posix` and apply to it `re_replace` function with `re_posix`, `string` and substring as arguments
1. use `re_replace` function with this arguments: `string`, `pattern`, substring and regex options
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
`replace_all` is same as `re_replace` but replaces all matches of pattern.
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
You can copare `re_posix` and `re_match`.
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
There is overloaded operator for character.
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
