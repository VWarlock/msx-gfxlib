#include <stdlib.h>
#include <stdio.h>
#include "gfx.h"

u_char char_to_nibble(u_char c);

u_char char_to_nibble(u_char c){
  if (c>='0' && c<='9'){
    return (u_char) ((c-'0') & 0x0f);
  } else if (c>='A' && c<='F'){
    return (u_char) ((c-'A'+10) & 0x0f);
  } else if (c>='a' && c<='f'){
    return (u_char) ((c-'a'+10) & 0x0f);
  } else {
    return 0;
  }
}

int main(int argc, char *argv[]) {
  u_char op;
  u_char reg;
  u_char val;
  u_char val1;
  u_char val2;

#ifdef __SDCC
        init();
#endif        

  op=' ';

  printf("%s", "Usage: s34b : reg 3 <- 0x4B\n");
  printf("%s", "         g4 : reg 4 -> print\n");

  while(op!='q'){
    op=getchar();
    
    switch(op){
    case 's':
      printf("%s", "+ psg_set(");
      reg=char_to_nibble(getchar());
      printf("reg=%02X, ", reg);
      val1=char_to_nibble(getchar());
      printf("val=%01X", val1);
      val2=char_to_nibble(getchar());
      printf("%01X)\n", val2);
      val=(val1<<4)|val2;
      psg_set(reg, val);
      break;
    case 'g':
      printf("%s", "+ psg_get(");
      reg=char_to_nibble(getchar());
      val=psg_get(reg);
      printf("reg=%02X)=%02X\n", reg, val);
      break;
    }
  }

  printf("%s\n", "[end]");
  return 0;
}
