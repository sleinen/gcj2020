#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <memory.h>

static int desc (const uint32_t *x, const uint32_t *y) { return *y - *x; }

int
main () {
  unsigned T, U;
  int64_t Qi;
  char Ri[100], *cp;
  unsigned i, k;
  scanf ("%u\n", &T);
  for (unsigned caseno = 1; caseno <= T; ++caseno) {
    uint32_t seen = 0;
    uint32_t counts[26] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0},
      scounts[26], *it;
    printf ("Case #%d: ", caseno);
    scanf ("%u\n", &U);
    for (k = 0; k < 10000; ++k) {
      scanf ("%lld %s\n", &Qi, Ri);
      ++counts[Ri[0]-'A'];
      for (i = 0, cp = Ri; *cp; ++i, ++cp) seen |= (1 << (*cp-'A'));
    }
    for (k = 0; k < 26; ++k)
      if ((seen & (1 << k)) && counts[k] == 0) {
        putchar (k + 'A'); break;
      }
    memcpy (scounts, counts, sizeof counts[0] * 26);
    qsort (scounts, 26, sizeof scounts[0],
           (int (*) (const void *, const void *)) desc);
    for (it = scounts, i = 0; i < 9; ++it, ++i)
      for (int k = 0; k < 26; ++k)
        if (counts[k] == scounts[i]) putchar(k + 'A');
    putchar('\n');
  }
}
