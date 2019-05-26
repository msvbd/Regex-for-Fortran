#include <regex.h>
#include <stdio.h>
#include <stdlib.h>
#include <locale.h>
#include <math.h>

void c_set_flags(int *cflags, int *eflags, char *opt) {
	int i;
	
	*cflags = 0;
	*eflags = 0;
	
    for(i=0 ; opt[i] != '\0'; i++) {
		//printf("    opt i : %s\n", &opt[i]);
		switch(opt[i]) {
			case 'x': *cflags |= REG_EXTENDED; break;
			case 'i': *cflags |= REG_ICASE; break;
			case 'n': *cflags |= REG_NEWLINE; break;
			/*case 's': cflags |= REG_NOSUB; break;*/
			case 'l': setlocale(LC_ALL,""); break;
			
			case 'b': *eflags |= REG_NOTBOL; break;
			case 'e': *eflags |= REG_NOTEOL; break;
		}
	}
}

int c_sizeof_regex_t(int part) {
  return (int) ceil((float) sizeof(regex_t) /(float) part);
}
